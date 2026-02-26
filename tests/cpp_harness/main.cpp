// Gold test harness for PAPURS.
//
// Three modes:
//   ./harness <blip_scenario>   — named phase-1 blip scenarios (output i16 LE to stdout)
//   ./harness APU               — reads register-write program from stdin (i16 LE to stdout)
//   ./harness PROC <voices>     — reads params+MIDI from stdin, uses real PAPUAudioProcessor
//                                 (f32 LE to stdout: L channel then R channel per block)
//
// APU protocol (stdin, one command per line):
//   W <addr_hex> <val_hex>   — write register
//   R <n_stereo_pairs>       — render and output i16 LE stereo
//
// PROC protocol (stdin, one command per line):
//   P <key>=<val> ...        — set parameter values (using JUCE param UIDs)
//   M <pos> <type> <ch> <v>  — queue MIDI event (types: NOTE_ON NOTE_OFF PITCH_BEND ALL_NOTES_OFF)
//   B <block_size>           — process one audio block, output f32 LE (L then R)

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <iostream>
#include <string>
#include <cstdint>
#include <sstream>
#include <vector>
#include <utility>

#include "Gb_Apu.h"
#include "Multi_Buffer.h"
#include "PluginProcessor.h"

static const long CLOCK_RATE  = 4194304;
static const long SAMPLE_RATE = 44100;
static const int  FRAME_SIZE  = 1024;

// ===================================================================
// Render helpers (for blip + APU modes)
// ===================================================================

static void render_mono(int n_samples, Blip_Buffer& buf, FILE* out) {
    int done = 0;
    while (done < n_samples) {
        long avail = buf.samples_avail();
        if (avail > 0) {
            blip_sample_t tmp[512];
            int count = (int)std::min(std::min((long)(n_samples - done), avail), (long)512);
            count = (int)buf.read_samples(tmp, count);
            fwrite(tmp, sizeof(blip_sample_t), (size_t)count, out);
            done += count;
        } else {
            buf.end_frame(FRAME_SIZE);
        }
    }
}

static void render_stereo(int n_pairs, Stereo_Buffer& sbuf, bool stereo_hint, FILE* out) {
    int done = 0;
    while (done < n_pairs) {
        long avail = sbuf.samples_avail();
        if (avail > 0) {
            blip_sample_t tmp[1024];
            int count = (int)std::min(std::min((long)(n_pairs - done), avail), (long)512);
            count = (int)sbuf.read_samples(tmp, count);
            fwrite(tmp, sizeof(blip_sample_t), (size_t)(count * 2), out);
            done += count;
        } else {
            sbuf.end_frame(FRAME_SIZE, stereo_hint);
        }
    }
}

// ===================================================================
// APU Gold Harness (phases 0, 2-8) — drives Gb_Apu directly
// ===================================================================

static const uint8_t WAVE_PRESET_0[32] = {
     0,  2,  4,  6,  8, 10, 12, 14, 15, 15, 15, 14, 14, 13, 13, 12,
    12, 11, 10,  9,  8,  7,  6,  5,  4,  4,  3,  3,  2,  2,  1,  1
};

struct GoldHarness {
    Gb_Apu apu;
    Stereo_Buffer buf;
    blip_time_t time;

    void init() {
        time = 0;
        apu.treble_eq(blip_eq_t(-20.0));
        buf.bass_freq(461);
        buf.clock_rate(CLOCK_RATE);
        buf.set_sample_rate(SAMPLE_RATE);
        apu.output(buf.center(), buf.left(), buf.right());
        write_reg(0xff1A, 0x00);
        for (int s = 0; s < 16; s++) {
            uint8_t high = WAVE_PRESET_0[s * 2];
            uint8_t low  = WAVE_PRESET_0[s * 2 + 1];
            write_reg(0xff30 + s, (int)(low | (high << 4)));
        }
        write_reg(0xff1A, 0x80);
        write_reg(0xff26, 0x8f);
    }

    void write_reg(int addr, int val) {
        time += 4;
        apu.write_register(time, (gb_addr_t)addr, val);
    }

    void render(int n_pairs, FILE* out) {
        int done = 0;
        while (done < n_pairs) {
            long avail = buf.samples_avail();
            if (avail > 0) {
                blip_sample_t samples[1024];
                int count = (int)std::min(std::min((long)(n_pairs - done), avail), (long)512);
                count = (int)buf.read_samples(samples, count);
                fwrite(samples, sizeof(blip_sample_t), (size_t)(count * 2), out);
                done += count;
            } else {
                time = 0;
                bool stereo = apu.end_frame(FRAME_SIZE);
                buf.end_frame(FRAME_SIZE, stereo);
            }
        }
    }
};

// ===================================================================
// APU mode: read register-write program from stdin
// ===================================================================

static int run_apu_mode() {
    GoldHarness h;
    h.init();

    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty() || line[0] == '#') continue;
        if (line[0] == 'W') {
            unsigned addr, val;
            if (sscanf(line.c_str(), "W %x %x", &addr, &val) == 2) {
                h.write_reg((int)addr, (int)val);
            }
        } else if (line[0] == 'R') {
            int n_pairs;
            if (sscanf(line.c_str(), "R %d", &n_pairs) == 1) {
                h.render(n_pairs, stdout);
            }
        }
    }
    return 0;
}

// ===================================================================
// PROC mode: real PAPUAudioProcessor, params + MIDI from stdin
// ===================================================================

static int run_proc_mode(int voices) {
    // Construct PAPUAudioProcessor — this calls addExtParam for all 39 params with defaults
    PAPUAudioProcessor proc;
    proc.setParameterValue("param", (float)voices);
    proc.prepareToPlay(44100.0, 1024);

    std::vector<std::pair<int, juce::MidiMessage>> pending_events;

    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty() || line[0] == '#') continue;

        if (line[0] == 'P') {
            // Set params: P key=val key=val ...
            std::istringstream iss(line.substr(2));
            std::string token;
            while (iss >> token) {
                auto eq = token.find('=');
                if (eq == std::string::npos) continue;
                std::string key = token.substr(0, eq);
                float val = std::stof(token.substr(eq + 1));
                proc.setParameterValue(key, val);
            }
        } else if (line[0] == 'M') {
            // Queue MIDI: M pos TYPE channel note_or_val
            int pos, ch, val;
            char type_str[32];
            if (sscanf(line.c_str(), "M %d %31s %d %d", &pos, type_str, &ch, &val) == 4) {
                juce::MidiMessage msg;
                if (strcmp(type_str, "NOTE_ON") == 0)
                    msg = juce::MidiMessage(ch, juce::MidiMessage::NoteOn, val, 0);
                else if (strcmp(type_str, "NOTE_OFF") == 0)
                    msg = juce::MidiMessage(ch, juce::MidiMessage::NoteOff, val, 0);
                else if (strcmp(type_str, "PITCH_BEND") == 0)
                    msg = juce::MidiMessage(ch, juce::MidiMessage::PitchWheel, 0, val);
                else if (strcmp(type_str, "ALL_NOTES_OFF") == 0)
                    msg = juce::MidiMessage(ch, juce::MidiMessage::AllNotesOff, 0, 0);
                pending_events.push_back({pos, msg});
            }
        } else if (line[0] == 'B') {
            // Process block: B blocksize
            int block_size;
            if (sscanf(line.c_str(), "B %d", &block_size) == 1) {
                juce::AudioSampleBuffer buffer(2, block_size);
                buffer.clear();

                juce::MidiBuffer midi;
                for (auto& ev : pending_events) {
                    midi.addEvent(ev.second, ev.first);
                }
                pending_events.clear();

                proc.processBlock(buffer, midi);

                // Output: L channel then R channel, f32 little-endian
                fwrite(buffer.getReadPointer(0), sizeof(float), (size_t)block_size, stdout);
                fwrite(buffer.getReadPointer(1), sizeof(float), (size_t)block_size, stdout);
            }
        }
    }
    return 0;
}

// ===================================================================
// Named blip scenarios (phase 1) — low-level Blip_Buffer tests
// ===================================================================

static const double PAPU_VOL_UNIT = 1.0 / 210.0;
static const int    HALF_PERIOD_440 = 4766;

static int run_blip_scenario(const char* name) {
    FILE* out = stdout;

    if (strcmp(name, "blip_impulse_q3") == 0) {
        Blip_Buffer buf;
        buf.clock_rate(CLOCK_RATE);
        buf.set_sample_rate(SAMPLE_RATE);
        buf.bass_freq(461);
        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(blip_eq_t(-20.0));
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);
        synth.offset(0, 100, &buf);
        render_mono(512, buf, out);
    }
    else if (strcmp(name, "blip_impulse_q2") == 0) {
        Blip_Buffer buf;
        buf.clock_rate(CLOCK_RATE);
        buf.set_sample_rate(SAMPLE_RATE);
        buf.bass_freq(461);
        Blip_Synth<blip_med_quality, 210> synth;
        synth.treble_eq(blip_eq_t(-20.0));
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);
        synth.offset(0, 100, &buf);
        render_mono(512, buf, out);
    }
    else if (strcmp(name, "blip_square_440") == 0) {
        Blip_Buffer buf;
        buf.clock_rate(CLOCK_RATE);
        buf.set_sample_rate(SAMPLE_RATE);
        buf.bass_freq(461);
        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(blip_eq_t(-20.0));
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);
        long end_t = 100000;
        int  amp   = 100;
        for (long t = 0; t < end_t; t += HALF_PERIOD_440) {
            synth.offset(t, amp, &buf);
            amp = -amp;
        }
        render_mono(1024, buf, out);
    }
    else if (strcmp(name, "blip_bass_step") == 0) {
        for (int bass : {16, 461, 600}) {
            Blip_Buffer buf;
            buf.clock_rate(CLOCK_RATE);
            buf.set_sample_rate(SAMPLE_RATE);
            buf.bass_freq(bass);
            Blip_Synth<blip_good_quality, 210> synth;
            synth.treble_eq(blip_eq_t(-20.0));
            synth.volume_unit(PAPU_VOL_UNIT);
            synth.output(&buf);
            synth.offset(0, 100, &buf);
            render_mono(512, buf, out);
        }
    }
    else if (strcmp(name, "blip_stereo") == 0) {
        Stereo_Buffer sbuf;
        sbuf.clock_rate(CLOCK_RATE);
        sbuf.set_sample_rate(SAMPLE_RATE);
        sbuf.bass_freq(461);
        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(blip_eq_t(-20.0));
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(sbuf.center());
        synth.offset(0,  100, sbuf.center());
        synth.offset(10,  50, sbuf.left());
        synth.offset(20,  25, sbuf.right());
        render_stereo(512, sbuf, true, out);
    }
    else if (strcmp(name, "blip_multiframe") == 0) {
        Blip_Buffer buf;
        buf.clock_rate(CLOCK_RATE);
        buf.set_sample_rate(SAMPLE_RATE);
        buf.bass_freq(461);
        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(blip_eq_t(-20.0));
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);

        int  amp     = 100;
        long frame_t = 0;
        long abs_t   = 0;
        blip_sample_t tmp[512];

        for (int frame = 0; frame < 100; frame++) {
            long frame_end = (long)(frame + 1) * FRAME_SIZE;
            while (abs_t < frame_end) {
                synth.offset(frame_t, amp, &buf);
                amp      = -amp;
                frame_t += HALF_PERIOD_440;
                abs_t   += HALF_PERIOD_440;
            }
            buf.end_frame(FRAME_SIZE);
            frame_t -= FRAME_SIZE;

            long avail = buf.samples_avail();
            while (avail > 0) {
                int want = (int)std::min(avail, (long)512);
                int got  = (int)buf.read_samples(tmp, want);
                fwrite(tmp, sizeof(blip_sample_t), (size_t)got, out);
                avail -= got;
            }
        }
    }
    else {
        fprintf(stderr, "unknown scenario: %s\n", name);
        return 1;
    }

    return 0;
}

// ===================================================================
// main
// ===================================================================

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: harness <scenario|APU|PROC> [voices]\n");
        return 1;
    }

#ifdef _WIN32
    _setmode(_fileno(stdout), _O_BINARY);
    _setmode(_fileno(stdin), _O_BINARY);
#endif

    std::string mode = argv[1];

    if (mode == "APU")
        return run_apu_mode();

    if (mode == "PROC") {
        int voices = (argc > 2) ? atoi(argv[2]) : 1;
        return run_proc_mode(voices);
    }

    // Named blip scenarios (phase 1)
    return run_blip_scenario(mode.c_str());
}
