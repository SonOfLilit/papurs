// Gold test harness for PAPURS — drives Gb_Apu / Blip_Buffer standalone (no JUCE).
// Accepts a scenario name, writes raw little-endian i16 samples to stdout.
//
// Phase 0 scenarios (full APU):
//   silence        — 512 stereo pairs, no notes
//   note60         — 2048 stereo pairs, Square 1, MIDI note 60, default params
//
// Phase 1 scenarios (Blip_Buffer / Blip_Synth / Stereo_Buffer directly):
//   blip_impulse_q3  — single +100 delta, quality=3, mono, 512 samples
//   blip_impulse_q2  — single +100 delta, quality=2, mono, 512 samples
//   blip_square_440  — ~440 Hz square wave, quality=3, mono, 1024 samples
//   blip_bass_step   — DC step: bass_freq=16/461/600, each 512 mono samples
//   blip_stereo      — center+left+right deltas, 512 stereo pairs
//   blip_multiframe  — 100 end_frame(1024) cycles, all samples concatenated
//
// Phase 9 scenarios (PapuEngine: MIDI→APU):
//   engine_note_on_off   — note 60 on at 0, off at 512, block=1024
//   engine_mono_priority — note priority (LIFO): 60→64→release 64 → 60 resumes
//   engine_pitch_bend    — note 60, pitch bend +1 semitone at 256, 1024 pairs
//   engine_channel_split — channel_split: note 60 ch1 + note 64 ch2, 2048 pairs
//
// Phase 10 scenarios (Vibrato LFO):
//   engine_vibrato_sq1   — note 60, pulse1 vib rate=5Hz amt=100, 2048 pairs
//   engine_vibrato_wave  — note 60, wave channel vib rate=3Hz amt=50, 2048 pairs
//
// Phase 11 scenarios (Full Parameter Mapping):
//   engine_full_pulse1   — pulse1 duty=2 A=3 R=5 tune=+12 fine=+50 sweep=-3 shift=2
//   engine_full_pulse2   — pulse2 duty=1 A=2 R=4 tune=-7 fine=-25
//   engine_wave_params   — wave waveform=5 tune=-7 fine=-25
//   engine_noise_params  — noise shift=8 step=1 ratio=3 A=0 R=4 (note-on then off)
//   engine_global_params — output=5 treble=-30 bass=461
//
// Phase 12 scenarios (Polyphony — raw f32 output):
//   proc_voices1         — 1 voice, note 60 on/off, block=1024
//   proc_voices2_notes   — 2 voices, notes 60+64 on, block=2048
//   proc_voices2_steal   — 2 voices, 3 notes on (3rd dropped), block=1024
//   proc_voices2_rrobin  — 2 voices, round-robin assignment, block=2048
//
// Phase 13 scenarios (Mid-block events — raw f32 output):
//   proc_mid_block_note  — 1 voice, note 60 on at pos=256, block=1024
//   proc_multi_events    — 2 voices, events at 0/256/512/768, block=1024
//   proc_odd_block       — 1 voice, note 60 on, block=333

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <string>
#include <cstdint>
#include <initializer_list>
#include <map>
#include <vector>

#include "Gb_Apu.h"
#include "Multi_Buffer.h"

static const long CLOCK_RATE  = 4194304;
static const long SAMPLE_RATE = 44100;
static const int  FRAME_SIZE  = 1024;

// --------------------------------------------------------------------------
// Render helpers
// --------------------------------------------------------------------------

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

// --------------------------------------------------------------------------
// Phase 0: full APU test harness
// --------------------------------------------------------------------------

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

static uint16_t midi_to_gb_period(int note) {
    double freq = 440.0 * pow(2.0, (note - 69) / 12.0);
    return (uint16_t)(((4194304.0 / freq) - 65536.0) / -32.0);
}

// --------------------------------------------------------------------------
// Phase 9: PapuEngine (standalone, no JUCE)
// Mirrors PAPUEngine from PluginProcessor.cpp exactly.
// --------------------------------------------------------------------------

// Wave presets from PluginProcessor.h (Pokemon Red/Crystal source)
static const uint8_t ENGINE_WAVE_SAMPLES[15][32] = {
    { 0, 2, 4, 6, 8,10,12,14,15,15,15,14,14,13,13,12,12,11,10, 9, 8, 7, 6, 5, 4, 4, 3, 3, 2, 2, 1, 1},
    { 0, 2, 4, 6, 8,10,12,14,14,15,15,15,15,14,14,14,13,13,12,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 2, 1, 1},
    { 1, 3, 6, 9,11,13,14,14,14,14,15,15,15,15,14,13,13,14,15,15,15,15,14,14,14,14,13,11, 9, 6, 3, 1},
    { 0, 2, 4, 6, 8,10,12,13,14,15,15,14,13,14,15,15,14,14,13,12,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0},
    { 0, 1, 2, 3, 4, 5, 6, 7, 8,10,12,13,14,14,15, 7, 7,15,14,14,13,12,10, 8, 7, 6, 5, 4, 3, 2, 1, 0},
    { 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 3, 3, 2, 2, 1, 1,15,15,14,14,12,12,10,10, 8, 8,10,10,12,12,14,14},
    { 0, 2, 4, 6, 8,10,12,14,12,11,10, 9, 8, 7, 6, 5,15,15,15,14,14,13,13,12, 4, 4, 3, 3, 2, 2, 1, 1},
    {12, 0,10, 9, 8, 7,15, 5,15,15,15,14,14,13,13,12, 4, 4, 3, 3, 2, 2,15, 1, 0, 2, 4, 6, 8,10,12,14},
    { 4, 4, 3, 3, 2, 2, 1,15, 0, 0, 4, 6, 8,10,12,14,15, 8,15,14,14,13,13,12,12,11,10, 9, 8, 7, 6, 5},
    { 1, 1, 0, 0, 0, 0, 0, 8, 0, 0, 1, 3, 5, 7, 9,10,11, 4,11,10,10, 9, 9, 8, 8, 7, 6, 5, 4, 3, 2, 1},
    { 7, 9,11,13,15,15,15,15,15,15,15,15,15,13,11, 9, 7, 5, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 5},
    { 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7, 8, 8, 9, 9,10,10,11,11,12,12,13,13,14,14,15,15},
    { 4, 6, 8,10,12,12,12,12,12,12,12,12,12,10, 8, 6, 4, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2},
    { 7,10,13,15,15,15,13,10, 7, 4, 1, 0, 0, 0, 1, 4, 7,10,13,15,15,15,13,10, 7, 4, 1, 0, 0, 0, 1, 4},
    {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
};

static double engine_midi_hz(double note) {
    return 440.0 * pow(2.0, (note - 69.0) / 12.0);
}

// Sine LFO — mirrors gin::LFO (waveShape=sine, offset=0, fade=0, delay=0)
struct FakeLFO {
    double sampleRate = 44100.0;
    double frequency  = 5.0;
    double depth      = 0.0;
    double phase      = 0.0;  // 0..1
    double output     = 0.0;

    void setSampleRate(double sr) { sampleRate = sr; }
    void setParams(double freq, double d) { frequency = freq; depth = d; }
    void reset() { phase = 0.0; output = 0.0; }
    void process(int n) {
        if (n > 0 && sampleRate > 0.0) {
            phase += frequency * n / sampleRate;
            phase -= std::floor(phase);
        }
        output = depth * std::sin(2.0 * M_PI * phase);
    }
    double getOutput() const { return output; }
};

struct EngineParams {
    int  output       = 7;
    bool pulse1_ol    = true;  bool pulse1_or = true;
    int  pulse1_duty  = 0;     int pulse1_A = 1; int pulse1_R = 1;
    int  pulse1_tune  = 0;     int pulse1_fine = 0;
    int  pulse1_sweep = 0;     int pulse1_shift = 0;
    bool pulse2_ol    = false; bool pulse2_or = false;
    int  pulse2_duty  = 0;     int pulse2_A = 1; int pulse2_R = 1;
    int  pulse2_tune  = 0;     int pulse2_fine = 0;
    bool wave_ol      = false; bool wave_or = false;
    int  wave_tune    = 0;     int wave_fine = 0;
    bool noise_ol     = false; bool noise_or = false;
    int  noise_A      = 1;     int noise_R = 1;
    int  noise_shift  = 0;     int noise_step = 0; int noise_ratio = 0;
    bool channel_split = false;
    // Vibrato (amt 0..100 → depth = 0.25 * amt / 100)
    float pulse1_vib_rate = 5.0f; float pulse1_vib_amt = 0.0f;
    float pulse2_vib_rate = 5.0f; float pulse2_vib_amt = 0.0f;
    float wave_vib_rate   = 5.0f; float wave_vib_amt   = 0.0f;
    // Global EQ and wave preset
    int   wave_index = 0;
    float treble     = -20.0f;
    int   bass       = 461;
};

struct MidiEvt {
    int pos;
    enum { NOTE_ON, NOTE_OFF, PITCH_BEND, ALL_NOTES_OFF } type;
    int channel = 1;   // 1-based
    int note    = 0;
    int value   = 8192;
};

struct PapuEngineHarness {
    Gb_Apu apu;
    Stereo_Buffer buf;
    blip_time_t time = 0;
    std::map<int, int> regCache;
    std::vector<int> noteQueues[4];
    int lastNotes[4] = {-1, -1, -1, -1};
    double pitchBend = 0.0;
    float freq[3] = {0.0f, 0.0f, 0.0f};
    bool channelSplit = false;
    uint8_t waveIndex = 0;
    FakeLFO lfos[3];
    int vibNotes[3] = {0, 0, 0};
    const EngineParams* currentParams_ = nullptr;
    float currentTreble = -20.0f;
    int   currentBass   = 461;

    blip_time_t clock_() { return time += 4; }

    void writeReg(int reg, int value, bool force) {
        auto itr = regCache.find(reg);
        if (force || itr == regCache.end() || itr->second != value) {
            regCache[reg] = value;
            apu.write_register(clock_(), (gb_addr_t)reg, value);
        }
    }

    void setWave(int index) {
        if (index == (int)waveIndex) return;
        waveIndex = (uint8_t)index;
        writeReg(0xff1A, 0x00, true);
        for (int s = 0; s < 16; s++) {
            uint8_t high = ENGINE_WAVE_SAMPLES[index][s * 2];
            uint8_t low  = ENGINE_WAVE_SAMPLES[index][s * 2 + 1];
            writeReg(0xff30 + s, (int)(low | (high << 4)), true);
        }
        writeReg(0xff1A, 0x80, true);
    }

    void init(const EngineParams& p, double sampleRate = 44100.0) {
        time = 0;
        apu.treble_eq(blip_eq_t(-20.0));
        buf.bass_freq(461);
        buf.clock_rate(CLOCK_RATE);
        buf.set_sample_rate((long)sampleRate);
        apu.output(buf.center(), buf.left(), buf.right());
        for (int i = 0; i < 3; i++) lfos[i].setSampleRate(sampleRate);

        writeReg(0xff1A, 0x00, true);
        for (int s = 0; s < 16; s++) {
            uint8_t high = ENGINE_WAVE_SAMPLES[0][s * 2];
            uint8_t low  = ENGINE_WAVE_SAMPLES[0][s * 2 + 1];
            writeReg(0xff30 + s, (int)(low | (high << 4)), true);
        }
        writeReg(0xff1A, 0x80, true);
        writeReg(0xff26, 0x8f, true);
    }

    int curNote(int qi) const {
        return noteQueues[qi].empty() ? -1 : noteQueues[qi].back();
    }

    // Mirror C++ runOscs; tracks vibNotes for subsequent runVibrato calls
    void runOscs(int cn[4], bool trig[4], const EngineParams& p) {
        // Ch1: Square 1
        if (cn[0] != -1) {
            vibNotes[0] = cn[0];
            int sweep_abs = abs(p.pulse1_sweep);
            bool neg = p.pulse1_sweep < 0;
            writeReg(0xff10, (sweep_abs << 4) | ((neg ? 1 : 0) << 3) | p.pulse1_shift, trig[0]);
            writeReg(0xff11, p.pulse1_duty << 6, trig[0]);
            // fine/100.0f: int/float = float; then double+float → double
            freq[0] = (float)engine_midi_hz(cn[0] + pitchBend + p.pulse1_tune + p.pulse1_fine / 100.0f);
            uint16_t period1 = (uint16_t)(((4194304.0f / freq[0]) - 65536.0f) / -32.0f);
            writeReg(0xff13, period1 & 0xff, trig[0]);
            int a1 = p.pulse1_A;
            writeReg(0xff12, a1 ? (0x00 | (1 << 3) | a1) : 0xf0, trig[0]);
            writeReg(0xff14, (trig[0] ? 0x80 : 0x00) | ((period1 >> 8) & 0x07), trig[0]);
        } else if (trig[0]) {
            int r1 = p.pulse1_R;
            int a1 = p.pulse1_A;
            if (a1 == 0 && r1 != 0) {
                uint16_t period1 = (uint16_t)(((4194304.0f / freq[0]) - 65536.0f) / -32.0f);
                writeReg(0xff13, period1 & 0xff, trig[0]);
                writeReg(0xff12, r1 ? (0xf0 | r1) : 0, trig[0]);
                writeReg(0xff14, (trig[0] ? 0x80 : 0x00) | ((period1 >> 8) & 0x07), trig[0]);
            } else {
                writeReg(0xff12, r1 ? (0xf0 | r1) : 0, trig[0]);
            }
        }

        // Ch2: Square 2
        if (cn[1] != -1) {
            vibNotes[1] = cn[1];
            writeReg(0xff16, p.pulse2_duty << 6, trig[1]);
            freq[1] = (float)engine_midi_hz(cn[1] + pitchBend + p.pulse2_tune + p.pulse2_fine / 100.0f);
            uint16_t period2 = (uint16_t)(((4194304.0f / freq[1]) - 65536.0f) / -32.0f);
            writeReg(0xff18, period2 & 0xff, trig[1]);
            int a2 = p.pulse2_A;
            writeReg(0xff17, a2 ? (0x00 | (1 << 3) | a2) : 0xf0, trig[1]);
            writeReg(0xff19, (trig[1] ? 0x80 : 0x00) | ((period2 >> 8) & 0x07), trig[1]);
        } else if (trig[1]) {
            int r2 = p.pulse2_R;
            int a2 = p.pulse2_A;
            if (a2 == 0 && r2 != 0) {
                uint16_t period2 = (uint16_t)(((4194304.0f / freq[1]) - 65536.0f) / -32.0f);
                writeReg(0xff18, period2 & 0xff, trig[1]);
                writeReg(0xff17, r2 ? (0xf0 | r2) : 0, trig[1]);
                writeReg(0xff19, (trig[1] ? 0x80 : 0x00) | ((period2 >> 8) & 0x07), trig[1]);
            } else {
                writeReg(0xff17, r2 ? (0xf0 | r2) : 0, trig[1]);
            }
        }

        // Ch3: Wave
        if (cn[2] != -1) {
            vibNotes[2] = cn[2];
            apu.resetStopWave();
            freq[2] = (float)engine_midi_hz(cn[2] + pitchBend + p.wave_tune + p.wave_fine / 100.0f);
            uint16_t period3 = (uint16_t)(-((65536.0f - 2048.0f * freq[2]) / freq[2]));
            writeReg(0xff1D, period3 & 0xff, trig[2]);
            writeReg(0xff1C, 0x20, trig[2]);
            writeReg(0xff1E, (trig[2] ? 0x80 : 0x00) | ((period3 >> 8) & 0x07), trig[2]);
        } else if (trig[2]) {
            apu.stopWave();
        }

        // Ch4: Noise
        if (cn[3] != -1) {
            int aN = p.noise_A;
            writeReg(0xff21, aN ? (0x00 | (1 << 3) | aN) : 0xf0, trig[3]);
            writeReg(0xff22, (p.noise_shift << 4) | (p.noise_step << 3) | p.noise_ratio, trig[3]);
            writeReg(0xff23, trig[3] ? 0x80 : 0x00, trig[3]);
        } else if (trig[3]) {
            int rN = p.noise_R;
            int aN = p.noise_A;
            if (aN == 0 && rN != 0) {
                writeReg(0xff21, rN ? (0xf0 | rN) : 0, trig[3]);
                writeReg(0xff23, 0x80, trig[3]);
            } else {
                writeReg(0xff21, rN ? (0xf0 | rN) : 0, trig[3]);
            }
        }
    }

    // Mirror C++ runVibrato: advance LFO and update freq registers
    void runVibrato(int todo) {
        const EngineParams& p = *currentParams_;
        for (int i = 0; i < 3; i++) lfos[i].process(todo);

        // Ch1: Square 1
        bool trig1 = (regCache.count(0xff14) ? regCache.at(0xff14) : 0) & 0x80;
        float f1 = (float)engine_midi_hz(vibNotes[0] + pitchBend + p.pulse1_tune
                        + p.pulse1_fine / 100.0f + lfos[0].getOutput() * 12.0);
        uint16_t period1 = (uint16_t)(((4194304.0f / f1) - 65536.0f) / -32.0f);
        writeReg(0xff13, period1 & 0xff, false);
        writeReg(0xff14, (trig1 ? 0x80 : 0x00) | ((period1 >> 8) & 0x07), false);

        // Ch2: Square 2
        bool trig2 = (regCache.count(0xff19) ? regCache.at(0xff19) : 0) & 0x80;
        float f2 = (float)engine_midi_hz(vibNotes[1] + pitchBend + p.pulse2_tune
                        + p.pulse2_fine / 100.0f + lfos[1].getOutput() * 12.0);
        uint16_t period2 = (uint16_t)(((4194304.0f / f2) - 65536.0f) / -32.0f);
        writeReg(0xff18, period2 & 0xff, false);
        writeReg(0xff19, (trig2 ? 0x80 : 0x00) | ((period2 >> 8) & 0x07), false);

        // Ch3: Wave
        bool trig3 = (regCache.count(0xff1E) ? regCache.at(0xff1E) : 0) & 0x80;
        float f3 = (float)engine_midi_hz(vibNotes[2] + pitchBend + p.wave_tune
                        + p.wave_fine / 100.0f + lfos[2].getOutput() * 12.0);
        uint16_t period3 = (uint16_t)(-((65536.0f - 2048.0f * f3) / f3));
        writeReg(0xff1D, period3 & 0xff, false);
        writeReg(0xff1E, (trig3 ? 0x80 : 0x00) | ((period3 >> 8) & 0x07), false);
    }

    // Mirror C++ runUntil: runVibrato then render stereo pairs from *done to pos
    void runUntil(int& done, std::vector<int16_t>& out, int pos) {
        int todo = pos - done;
        runVibrato(todo);
        while (todo > 0) {
            long avail = buf.samples_avail();
            if (avail > 0) {
                blip_sample_t tmp[1024];
                int count = (int)std::min(std::min((long)todo, avail), (long)512);
                count = (int)buf.read_samples(tmp, count);
                for (int i = 0; i < count * 2; i++)
                    out.push_back(tmp[i]);
                done  += count;
                todo  -= count;
            } else {
                time = 0;
                bool stereo = apu.end_frame(1024);
                buf.end_frame(1024, stereo);
            }
        }
    }

    // Mirror C++ processBlock (single-voice path)
    void processBlock(int blockSize, const std::vector<MidiEvt>& events,
                      const EngineParams& p, std::vector<int16_t>& out) {
        // Apply global EQ and wave preset (mirrors outer processBlock param checks)
        setWave(p.wave_index);
        if (p.treble != currentTreble) {
            currentTreble = p.treble;
            apu.treble_eq(blip_eq_t((double)p.treble));
        }
        if (p.bass != currentBass) {
            currentBass = p.bass;
            buf.bass_freq(p.bass);
        }

        // Update LFO params (mirrors outer processBlock vib param update)
        float d1 = 0.25f * p.pulse1_vib_amt / 100.0f;
        lfos[0].setParams((double)p.pulse1_vib_rate, (double)d1);
        float d2 = 0.25f * p.pulse2_vib_amt / 100.0f;
        lfos[1].setParams((double)p.pulse2_vib_rate, (double)d2);
        float d3 = 0.25f * p.wave_vib_amt / 100.0f;
        lfos[2].setParams((double)p.wave_vib_rate, (double)d3);
        currentParams_ = &p;

        int vol_reg = 0x08 | p.output;
        writeReg(0xff24, vol_reg, false);

        int pan_reg = (p.pulse1_ol ? 0x10 : 0) | (p.pulse1_or ? 0x01 : 0) |
                      (p.pulse2_ol ? 0x20 : 0) | (p.pulse2_or ? 0x02 : 0) |
                      (p.wave_ol   ? 0x40 : 0) | (p.wave_or   ? 0x04 : 0) |
                      (p.noise_ol  ? 0x80 : 0) | (p.noise_or  ? 0x08 : 0);
        writeReg(0xff25, pan_reg, false);

        bool new_split = p.channel_split;
        if (new_split != channelSplit) {
            channelSplit = new_split;
            for (auto& q : noteQueues) q.clear();
        }

        int done = 0;
        bool trig0[4] = {false, false, false, false};
        runOscs(lastNotes, trig0, p);
        runUntil(done, out, 0);

        for (auto& evt : events) {
            runUntil(done, out, evt.pos);

            bool updateBend = false;
            if (evt.type == MidiEvt::NOTE_ON) {
                if (evt.channel == 1 || !channelSplit) noteQueues[0].push_back(evt.note);
                else if (evt.channel == 2) noteQueues[1].push_back(evt.note);
                else if (evt.channel == 3) noteQueues[2].push_back(evt.note);
                else if (evt.channel == 4) noteQueues[3].push_back(evt.note);
            } else if (evt.type == MidiEvt::NOTE_OFF) {
                int qi = (evt.channel == 1 || !channelSplit) ? 0 :
                         (evt.channel == 2) ? 1 :
                         (evt.channel == 3) ? 2 :
                         (evt.channel == 4) ? 3 : -1;
                if (qi >= 0) {
                    auto& q = noteQueues[qi];
                    auto it = std::find(q.begin(), q.end(), evt.note);
                    if (it != q.end()) q.erase(it);
                }
            } else if (evt.type == MidiEvt::ALL_NOTES_OFF) {
                for (auto& q : noteQueues) q.clear();
            } else if (evt.type == MidiEvt::PITCH_BEND) {
                updateBend = true;
                pitchBend = (evt.value - 8192) / 8192.0f * 2;
            }

            int newNotes[4];
            newNotes[0] = curNote(0);
            newNotes[1] = channelSplit ? curNote(1) : newNotes[0];
            newNotes[2] = channelSplit ? curNote(2) : newNotes[0];
            newNotes[3] = channelSplit ? curNote(3) : newNotes[0];

            bool anyChanged = updateBend;
            for (int i = 0; i < 4; i++) anyChanged |= (newNotes[i] != lastNotes[i]);

            if (anyChanged) {
                // Reset LFO on new note (not on pitch bend), mirrors C++ lines 380-382
                if (!updateBend && newNotes[0] != -1) lfos[0].reset();
                if (!updateBend && newNotes[1] != -1) lfos[1].reset();
                if (!updateBend && newNotes[2] != -1) lfos[2].reset();
                bool triggers[4] = {
                    lastNotes[0] != newNotes[0],
                    lastNotes[1] != newNotes[1],
                    lastNotes[2] != newNotes[2],
                    lastNotes[3] != newNotes[3],
                };
                runOscs(newNotes, triggers, p);
                for (int i = 0; i < 4; i++) lastNotes[i] = newNotes[i];
            }
        }

        runUntil(done, out, blockSize);
    }
};

// --------------------------------------------------------------------------
// Phase 12: PapuProcessor (multi-voice, float output — mirrors PAPUAudioProcessor)
// --------------------------------------------------------------------------

struct PapuProcessorHarness {
    std::vector<PapuEngineHarness*> engines;
    int nextVoice = 0;

    ~PapuProcessorHarness() {
        for (auto* e : engines) delete e;
    }

    void init(int voices, const EngineParams& p, double sampleRate = 44100.0) {
        for (int i = 0; i < voices; i++) {
            auto* e = new PapuEngineHarness();
            e->init(p, sampleRate);
            engines.push_back(e);
        }
    }

    // Apply params to engine (wave, treble, bass, LFO, vol/pan, channel_split)
    void applyParams(PapuEngineHarness* e, const EngineParams& p) {
        e->setWave(p.wave_index);
        if (p.treble != e->currentTreble) {
            e->currentTreble = p.treble;
            e->apu.treble_eq(blip_eq_t((double)p.treble));
        }
        if (p.bass != e->currentBass) {
            e->currentBass = p.bass;
            e->buf.bass_freq(p.bass);
        }
        float d1 = 0.25f * p.pulse1_vib_amt / 100.0f;
        e->lfos[0].setParams((double)p.pulse1_vib_rate, (double)d1);
        float d2 = 0.25f * p.pulse2_vib_amt / 100.0f;
        e->lfos[1].setParams((double)p.pulse2_vib_rate, (double)d2);
        float d3 = 0.25f * p.wave_vib_amt / 100.0f;
        e->lfos[2].setParams((double)p.wave_vib_rate, (double)d3);
        e->currentParams_ = &p;
    }

    // Mirror C++ prepareBlock: apply params, write vol/pan, run sustained oscs
    void prepareVoice(PapuEngineHarness* e, const EngineParams& p) {
        applyParams(e, p);

        int vol_reg = 0x08 | p.output;
        e->writeReg(0xff24, vol_reg, false);

        int pan_reg = (p.pulse1_ol ? 0x10 : 0) | (p.pulse1_or ? 0x01 : 0) |
                      (p.pulse2_ol ? 0x20 : 0) | (p.pulse2_or ? 0x02 : 0) |
                      (p.wave_ol   ? 0x40 : 0) | (p.wave_or   ? 0x04 : 0) |
                      (p.noise_ol  ? 0x80 : 0) | (p.noise_or  ? 0x08 : 0);
        e->writeReg(0xff25, pan_reg, false);

        if (p.channel_split != e->channelSplit) {
            e->channelSplit = p.channel_split;
            for (auto& q : e->noteQueues) q.clear();
        }

        bool trig0[4] = {false, false, false, false};
        e->runOscs(e->lastNotes, trig0, p);
        // runUntil(done=0, pos=0) → todo=0 → just runVibrato(0)
        e->runVibrato(0);
    }

    // Mirror C++ handleMessage: update queues, re-run oscs if changed
    void handleMessageForVoice(PapuEngineHarness* e, const MidiEvt& evt, const EngineParams& p) {
        bool updateBend = false;
        if (evt.type == MidiEvt::NOTE_ON) {
            int ch = evt.channel;
            if (ch == 1 || !e->channelSplit) e->noteQueues[0].push_back(evt.note);
            else if (ch == 2) e->noteQueues[1].push_back(evt.note);
            else if (ch == 3) e->noteQueues[2].push_back(evt.note);
            else if (ch == 4) e->noteQueues[3].push_back(evt.note);
        } else if (evt.type == MidiEvt::NOTE_OFF) {
            int ch = evt.channel;
            int qi = (ch == 1 || !e->channelSplit) ? 0 :
                     (ch == 2) ? 1 : (ch == 3) ? 2 : (ch == 4) ? 3 : -1;
            if (qi >= 0) {
                auto& q = e->noteQueues[qi];
                auto it = std::find(q.begin(), q.end(), evt.note);
                if (it != q.end()) q.erase(it);
            }
        } else if (evt.type == MidiEvt::ALL_NOTES_OFF) {
            for (auto& q : e->noteQueues) q.clear();
        } else if (evt.type == MidiEvt::PITCH_BEND) {
            updateBend = true;
            e->pitchBend = (evt.value - 8192) / 8192.0f * 2;
        }

        int newNotes[4];
        newNotes[0] = e->curNote(0);
        newNotes[1] = e->channelSplit ? e->curNote(1) : newNotes[0];
        newNotes[2] = e->channelSplit ? e->curNote(2) : newNotes[0];
        newNotes[3] = e->channelSplit ? e->curNote(3) : newNotes[0];

        bool anyChanged = updateBend;
        for (int i = 0; i < 4; i++) anyChanged |= (newNotes[i] != e->lastNotes[i]);

        if (anyChanged) {
            if (!updateBend && newNotes[0] != -1) e->lfos[0].reset();
            if (!updateBend && newNotes[1] != -1) e->lfos[1].reset();
            if (!updateBend && newNotes[2] != -1) e->lfos[2].reset();
            bool triggers[4] = {
                e->lastNotes[0] != newNotes[0],
                e->lastNotes[1] != newNotes[1],
                e->lastNotes[2] != newNotes[2],
                e->lastNotes[3] != newNotes[3],
            };
            e->runOscs(newNotes, triggers, p);
            for (int i = 0; i < 4; i++) e->lastNotes[i] = newNotes[i];
        }
    }

    // Mirror C++ PAPUEngine::runUntil but accumulate i16 into float buffer
    // fb layout: [left_0..left_{N-1}, right_0..right_{N-1}]
    void runVoiceUntil(PapuEngineHarness* e, int doneCopy, std::vector<float>& fb, int pos, int blockSize) {
        int todo = pos - doneCopy;
        e->runVibrato(todo);
        while (todo > 0) {
            long avail = e->buf.samples_avail();
            if (avail > 0) {
                blip_sample_t tmp[1024];
                int count = (int)std::min(std::min((long)todo, avail), (long)512);
                count = (int)e->buf.read_samples(tmp, count);
                for (int i = 0; i < count; i++) {
                    fb[doneCopy + i]             += tmp[i*2+0] / 32768.0f;
                    fb[doneCopy + i + blockSize] += tmp[i*2+1] / 32768.0f;
                }
                doneCopy += count;
                todo     -= count;
            } else {
                e->time = 0;
                bool stereo = e->apu.end_frame(1024);
                e->buf.end_frame(1024, stereo);
            }
        }
    }

    // Mirror C++ PAPUAudioProcessor::runUntil: advance all voices in parallel
    void runUntilAll(int& done, std::vector<float>& fb, int pos, int blockSize) {
        int clamped = std::min(pos, blockSize);
        int todo    = clamped - done;
        for (auto* e : engines) {
            int doneCopy = done;
            runVoiceUntil(e, doneCopy, fb, clamped, blockSize);
        }
        done += todo;
    }

    // getNote(channel): returns lastNotes[channel-1] (mirrors C++ PAPUEngine::getNote)
    int getNote(PapuEngineHarness* e, int channel) {
        return (channel >= 1 && channel <= 4) ? e->lastNotes[channel-1] : -1;
    }

    int findFreeVoiceIdx(int channel) {
        int voices = (int)engines.size();
        for (int i = 0; i < voices; i++) {
            int vi = (nextVoice + i) % voices;
            if (getNote(engines[vi], channel) == -1) {
                nextVoice = (vi + 1) % voices;
                return vi;
            }
        }
        return -1;
    }

    int findVoiceForNoteIdx(int note, int channel) {
        for (int i = 0; i < (int)engines.size(); i++) {
            if (getNote(engines[i], channel) == note) return i;
        }
        return -1;
    }

    // Full block processing — mirrors PAPUAudioProcessor::processBlock multi-voice path
    std::vector<float> processBlock(int blockSize, const std::vector<MidiEvt>& events, const EngineParams& p) {
        std::vector<float> fb(2 * blockSize, 0.0f);

        for (auto* e : engines) prepareVoice(e, p);

        int done = 0;

        for (auto& evt : events) {
            runUntilAll(done, fb, evt.pos, blockSize);

            if (evt.type == MidiEvt::NOTE_ON) {
                int ch = p.channel_split ? evt.channel : 1;
                int vi = findFreeVoiceIdx(ch);
                if (vi >= 0) handleMessageForVoice(engines[vi], evt, p);
            } else if (evt.type == MidiEvt::NOTE_OFF) {
                int ch = p.channel_split ? evt.channel : 1;
                int vi = findVoiceForNoteIdx(evt.note, ch);
                if (vi >= 0) handleMessageForVoice(engines[vi], evt, p);
            } else {
                for (auto* e : engines) handleMessageForVoice(e, evt, p);
            }
        }

        runUntilAll(done, fb, blockSize, blockSize);
        return fb;
    }
};

// --------------------------------------------------------------------------
// Phase 1: common settings matching PAPU defaults
// --------------------------------------------------------------------------

static const blip_eq_t PAPU_EQ(-20.0);
static const double    PAPU_VOL_UNIT = 1.0 / 210.0;

static void setup_blip_buf(Blip_Buffer& buf, int bass = 461) {
    buf.clock_rate(CLOCK_RATE);
    buf.set_sample_rate(SAMPLE_RATE);
    buf.bass_freq(bass);
}

// --------------------------------------------------------------------------
// main
// --------------------------------------------------------------------------

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <scenario>\n", argv[0]);
        return 1;
    }
    const std::string scenario = argv[1];

    // ---- Phase 0 ----

    if (scenario == "silence") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        h.render(512, stdout);

    } else if (scenario == "note60") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        uint16_t period = midi_to_gb_period(60);
        h.write_reg(0xff10, 0x00);
        h.write_reg(0xff11, 0 << 6);
        h.write_reg(0xff13, period & 0xff);
        h.write_reg(0xff12, 0x00 | (1 << 3) | 1);
        h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
        h.render(2048, stdout);

    // ---- Phase 1: blip_impulse_q3 ----
    // Single +100 delta at t=0, quality=3 (blip_good_quality), mono, 512 samples

    } else if (scenario == "blip_impulse_q3") {
        Blip_Buffer buf;
        setup_blip_buf(buf);
        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(PAPU_EQ);
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);
        synth.offset(0, 100, &buf);
        render_mono(512, buf, stdout);

    // ---- Phase 1: blip_impulse_q2 ----
    // Single +100 delta at t=0, quality=2 (blip_med_quality), mono, 512 samples

    } else if (scenario == "blip_impulse_q2") {
        Blip_Buffer buf;
        setup_blip_buf(buf);
        Blip_Synth<blip_med_quality, 210> synth;
        synth.treble_eq(PAPU_EQ);
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);
        synth.offset(0, 100, &buf);
        render_mono(512, buf, stdout);

    // ---- Phase 1: blip_square_440 ----
    // ~440 Hz square wave: half_period=4766 clocks, quality=3, mono, 1024 samples

    } else if (scenario == "blip_square_440") {
        const int HALF_PERIOD = 4766;
        Blip_Buffer buf;
        setup_blip_buf(buf);
        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(PAPU_EQ);
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);

        // Enough transitions to produce 1024 output samples
        // clocks_per_sample ≈ 95.1 → 1024 samples ≈ 97K clocks
        blip_time_t end_t = 100000;
        int amp = 100;
        for (blip_time_t t = 0; t < end_t; t += HALF_PERIOD) {
            synth.offset(t, amp, &buf);
            amp = -amp;
        }
        render_mono(1024, buf, stdout);

    // ---- Phase 1: blip_bass_step ----
    // DC step (+100 at t=0) with bass_freq=16, 461, 600. Each: 512 mono samples.
    // Total output: 1536 samples (3 * 512).

    } else if (scenario == "blip_bass_step") {
        for (int bass : {16, 461, 600}) {
            Blip_Buffer buf;
            setup_blip_buf(buf, bass);
            Blip_Synth<blip_good_quality, 210> synth;
            synth.treble_eq(PAPU_EQ);
            synth.volume_unit(PAPU_VOL_UNIT);
            synth.output(&buf);
            synth.offset(0, 100, &buf);
            render_mono(512, buf, stdout);
        }

    // ---- Phase 1: blip_stereo ----
    // Add deltas: center +100 at t=0, left +50 at t=10, right +25 at t=20.
    // L_out = center+left = 150, R_out = center+right = 125. 512 stereo pairs.

    } else if (scenario == "blip_stereo") {
        Stereo_Buffer sbuf;
        sbuf.clock_rate(CLOCK_RATE);
        sbuf.set_sample_rate(SAMPLE_RATE);
        sbuf.bass_freq(461);

        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(PAPU_EQ);
        synth.volume_unit(PAPU_VOL_UNIT);

        synth.output(sbuf.center());
        synth.offset(0, 100, sbuf.center());

        synth.output(sbuf.left());
        synth.offset(10, 50, sbuf.left());

        synth.output(sbuf.right());
        synth.offset(20, 25, sbuf.right());

        render_stereo(512, sbuf, true, stdout);

    // ---- Phase 1: blip_multiframe ----
    // ~440 Hz square wave, 100 end_frame(1024) calls.
    // Reads samples between each frame (typical PAPU usage pattern).
    // Total output: all samples from all 100 frames concatenated.

    } else if (scenario == "blip_multiframe") {
        const int HALF_PERIOD = 4766;
        Blip_Buffer buf;
        setup_blip_buf(buf);
        Blip_Synth<blip_good_quality, 210> synth;
        synth.treble_eq(PAPU_EQ);
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.output(&buf);

        int amp = 100;
        blip_time_t frame_t = 0;   // time within current frame
        blip_time_t abs_t   = 0;   // absolute time for transition calc

        for (int frame = 0; frame < 100; frame++) {
            blip_time_t frame_end = (frame + 1) * FRAME_SIZE;
            // Add transitions in this frame
            while (abs_t < frame_end) {
                synth.offset(frame_t, amp, &buf);
                amp     = -amp;
                frame_t += HALF_PERIOD;
                abs_t  += HALF_PERIOD;
            }
            buf.end_frame(FRAME_SIZE);
            frame_t -= FRAME_SIZE;  // localize to new frame

            // Read all available samples
            long avail = buf.samples_avail();
            while (avail > 0) {
                blip_sample_t tmp[512];
                int count = (int)std::min(avail, (long)512);
                count = (int)buf.read_samples(tmp, count);
                fwrite(tmp, sizeof(blip_sample_t), (size_t)count, stdout);
                avail -= count;
            }
        }

    // ---- Phase 2-8: APU gold tests ----
    // All use GoldHarness (full APU + Stereo_Buffer), render 2048 stereo pairs.

    // apu_sq_duty{0-3}: Square 1, all duty cycles, MIDI note 60, volume=15, 2048 pairs
    } else if (scenario == "apu_sq_duty0" || scenario == "apu_sq_duty1" ||
               scenario == "apu_sq_duty2" || scenario == "apu_sq_duty3") {
        int duty_idx = scenario.back() - '0'; // 0-3
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);  // square1 left+right → center
        uint16_t period = midi_to_gb_period(60);
        h.write_reg(0xff10, 0x00);
        h.write_reg(0xff11, duty_idx << 6); // duty cycle
        h.write_reg(0xff12, 0x08 | (15 << 4)); // volume=15, env off, dir=up
        h.write_reg(0xff13, period & 0xff);
        h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
        h.render(2048, stdout);

    // apu_sq_freq_min: Square 1, minimum frequency (period=8192 → freq=0)
    } else if (scenario == "apu_sq_freq_min") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        h.write_reg(0xff10, 0x00);
        h.write_reg(0xff11, 2 << 6); // duty=2 (50%)
        h.write_reg(0xff12, 0x08 | (15 << 4));
        h.write_reg(0xff13, 0x00); // freq low = 0
        h.write_reg(0xff14, 0x80); // trigger, freq high = 0 → period = 2048*4 = 8192
        h.render(2048, stdout);

    // apu_sq_freq_max: Square 1, high frequency (freq=2020 → period=112)
    } else if (scenario == "apu_sq_freq_max") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        h.write_reg(0xff10, 0x00);
        h.write_reg(0xff11, 2 << 6);
        h.write_reg(0xff12, 0x08 | (15 << 4));
        h.write_reg(0xff13, 2020 & 0xff);
        h.write_reg(0xff14, 0x80 | ((2020 >> 8) & 0x07));
        h.render(2048, stdout);

    // apu_sq_vol0: Square 1, volume=0 → silence
    } else if (scenario == "apu_sq_vol0") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        uint16_t period = midi_to_gb_period(60);
        h.write_reg(0xff10, 0x00);
        h.write_reg(0xff11, 2 << 6);
        h.write_reg(0xff12, 0x00); // volume=0
        h.write_reg(0xff13, period & 0xff);
        h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
        h.render(2048, stdout);

    // apu_envelope: Square 1, attack=1 (vol ramps from 0 up), render 4096 pairs
    } else if (scenario == "apu_envelope") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        uint16_t period = midi_to_gb_period(60);
        h.write_reg(0xff10, 0x00);
        h.write_reg(0xff11, 2 << 6);
        // initial vol=0, dir=up, env_period=1
        h.write_reg(0xff12, (0 << 4) | 0x08 | 1);
        h.write_reg(0xff13, period & 0xff);
        h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
        h.render(4096, stdout);

    // apu_sweep: Square 1, upward sweep, render 4096 pairs
    } else if (scenario == "apu_sweep") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        // sweep_period=2, sweep_shift=1, dir=up
        h.write_reg(0xff10, (2 << 4) | 0 | 1); // period=2, dir=0 (up), shift=1
        h.write_reg(0xff11, 2 << 6);
        h.write_reg(0xff12, 0x08 | (15 << 4));
        uint16_t period = midi_to_gb_period(60);
        h.write_reg(0xff13, period & 0xff);
        h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
        h.render(4096, stdout);

    // apu_length: Square 1, length counter enabled, short length → silence
    } else if (scenario == "apu_length") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x11);
        uint16_t period = midi_to_gb_period(60);
        h.write_reg(0xff10, 0x00);
        // length=2 (64-62=2 remaining), length_enabled=true (bit 6 of reg 4)
        h.write_reg(0xff11, (2 << 6) | (64 - 2));
        h.write_reg(0xff12, 0x08 | (15 << 4));
        h.write_reg(0xff13, period & 0xff);
        h.write_reg(0xff14, 0x80 | 0x40 | ((period >> 8) & 0x07)); // trigger+length_enabled
        h.render(4096, stdout);

    // apu_wave: Wave channel, preset 0, full volume, note 60
    } else if (scenario == "apu_wave") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x44); // wave channel → center
        uint16_t period = midi_to_gb_period(60);
        uint16_t wave_freq = (uint16_t)((4194304.0 / (440.0 * pow(2.0, (60 - 69) / 12.0)) - 65536.0) / -16.0 + 0.5);
        // Wave freq = 2048 - (clock/(2*freq)) = 2048 - 4194304/(2*261.63) ≈ 2048-8018 ... use period calc
        // Wave period = (2048-freq)*2, so freq = 2048 - period/2
        // Let's just use midi_to_gb_period / 2 for wave
        h.write_reg(0xff1C, 1 << 5); // volume = 1 (100%)
        h.write_reg(0xff1D, period & 0xff);
        h.write_reg(0xff1E, 0x80 | ((period >> 8) & 0x07));
        h.render(2048, stdout);

    // apu_noise: Noise channel, 15-bit LFSR, volume=15
    } else if (scenario == "apu_noise") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x88); // noise → center
        h.write_reg(0xff21, 0x08 | (15 << 4)); // vol=15, dir=up, period=0
        h.write_reg(0xff22, 0x00); // divisor=0 (8), shift=0, step=0 (15-bit)
        h.write_reg(0xff23, 0x80); // trigger
        h.render(2048, stdout);

    // apu_stereo: Square1 → left, Square2 → right, both note 60, 2048 pairs
    } else if (scenario == "apu_stereo") {
        GoldHarness h;
        h.init();
        h.write_reg(0xff24, 0x08 | 7);
        h.write_reg(0xff25, 0x21); // sq1=left(bit4), sq2=right(bit1)
        uint16_t period = midi_to_gb_period(60);
        // Square 1
        h.write_reg(0xff10, 0x00);
        h.write_reg(0xff11, 2 << 6);
        h.write_reg(0xff12, 0x08 | (15 << 4));
        h.write_reg(0xff13, period & 0xff);
        h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
        // Square 2
        uint16_t period2 = midi_to_gb_period(64); // E4
        h.write_reg(0xff16, 2 << 6);
        h.write_reg(0xff17, 0x08 | (15 << 4));
        h.write_reg(0xff18, period2 & 0xff);
        h.write_reg(0xff19, 0x80 | ((period2 >> 8) & 0x07));
        h.render(2048, stdout);

    // ---- Phase 9: engine scenarios ----

    // engine_note_on_off: note 60 on at pos=0, off at pos=512, block=1024
    } else if (scenario == "engine_note_on_off") {
        PapuEngineHarness h;
        EngineParams p;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0,   MidiEvt::NOTE_ON,  1, 60},
            {512, MidiEvt::NOTE_OFF, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(1024, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_mono_priority: notes 60 and 64 on (LIFO), release 64 → 60 resumes
    } else if (scenario == "engine_mono_priority") {
        PapuEngineHarness h;
        EngineParams p;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0,    MidiEvt::NOTE_ON,  1, 60},
            {256,  MidiEvt::NOTE_ON,  1, 64},
            {1024, MidiEvt::NOTE_OFF, 1, 64},
        };
        std::vector<int16_t> out;
        // Two blocks of 1024 samples each
        h.processBlock(1024, events, p, out);
        std::vector<MidiEvt> events2;
        h.processBlock(1024, events2, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_pitch_bend: note 60, pitch bend +1 semitone at pos=256, block=1024
    } else if (scenario == "engine_pitch_bend") {
        PapuEngineHarness h;
        EngineParams p;
        h.init(p);
        // pitch wheel 12288: (12288-8192)/8192.0f*2 = 1.0 semitone
        std::vector<MidiEvt> events = {
            {0,   MidiEvt::NOTE_ON,    1, 60, 0},
            {256, MidiEvt::PITCH_BEND, 1, 0, 12288},
        };
        std::vector<int16_t> out;
        h.processBlock(1024, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_channel_split: channel_split=true, note 60 on ch1, note 64 on ch2
    } else if (scenario == "engine_channel_split") {
        PapuEngineHarness h;
        EngineParams p;
        p.channel_split = true;
        p.pulse2_ol = true; p.pulse2_or = true;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
            {0, MidiEvt::NOTE_ON, 2, 64},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // ---- Phase 10: vibrato scenarios ----

    // engine_vibrato_sq1: note 60, pulse1 vibrato rate=5Hz amt=100, 2048 pairs
    } else if (scenario == "engine_vibrato_sq1") {
        PapuEngineHarness h;
        EngineParams p;
        p.pulse1_vib_rate = 5.0f;
        p.pulse1_vib_amt  = 100.0f;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_vibrato_wave: note 60 on wave channel, vib rate=3Hz amt=50, 2048 pairs
    } else if (scenario == "engine_vibrato_wave") {
        PapuEngineHarness h;
        EngineParams p;
        p.pulse1_ol = false; p.pulse1_or = false;
        p.wave_ol   = true;  p.wave_or   = true;
        p.wave_vib_rate = 3.0f;
        p.wave_vib_amt  = 50.0f;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // ---- Phase 11: full parameter mapping ----

    // engine_full_pulse1: pulse1 duty=2 A=3 R=5 tune=+12 fine=+50 sweep=-3 shift=2
    } else if (scenario == "engine_full_pulse1") {
        PapuEngineHarness h;
        EngineParams p;
        p.pulse1_duty  = 2;
        p.pulse1_A     = 3;
        p.pulse1_R     = 5;
        p.pulse1_tune  = 12;
        p.pulse1_fine  = 50;
        p.pulse1_sweep = -3;
        p.pulse1_shift = 2;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0,    MidiEvt::NOTE_ON,  1, 60},
            {1024, MidiEvt::NOTE_OFF, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_full_pulse2: pulse2 duty=1 A=2 R=4 tune=-7 fine=-25
    } else if (scenario == "engine_full_pulse2") {
        PapuEngineHarness h;
        EngineParams p;
        p.pulse1_ol = false; p.pulse1_or = false;
        p.pulse2_ol = true;  p.pulse2_or = true;
        p.pulse2_duty = 1;
        p.pulse2_A    = 2;
        p.pulse2_R    = 4;
        p.pulse2_tune = -7;
        p.pulse2_fine = -25;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0,    MidiEvt::NOTE_ON,  1, 60},
            {1024, MidiEvt::NOTE_OFF, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_wave_params: wave waveform=5 tune=-7 fine=-25
    } else if (scenario == "engine_wave_params") {
        PapuEngineHarness h;
        EngineParams p;
        p.pulse1_ol = false; p.pulse1_or = false;
        p.wave_ol   = true;  p.wave_or   = true;
        p.wave_tune  = -7;
        p.wave_fine  = -25;
        p.wave_index = 5;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_noise_params: noise shift=8 step=1 ratio=3 A=0 R=4
    } else if (scenario == "engine_noise_params") {
        PapuEngineHarness h;
        EngineParams p;
        p.pulse1_ol = false; p.pulse1_or = false;
        p.noise_ol   = true;  p.noise_or  = true;
        p.noise_A     = 0;
        p.noise_R     = 4;
        p.noise_shift = 8;
        p.noise_step  = 1;
        p.noise_ratio = 3;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0,    MidiEvt::NOTE_ON,  1, 60},
            {1024, MidiEvt::NOTE_OFF, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // engine_global_params: output=5 treble=-30 bass=461
    } else if (scenario == "engine_global_params") {
        PapuEngineHarness h;
        EngineParams p;
        p.output = 5;
        p.treble = -30.0f;
        p.bass   = 461;
        h.init(p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
        };
        std::vector<int16_t> out;
        h.processBlock(2048, events, p, out);
        fwrite(out.data(), sizeof(int16_t), out.size(), stdout);

    // ---- Phase 12: proc_voices1 ----
    // 1 voice, note 60 on at 0, off at 512, block=1024. Output: raw f32 bytes.
    } else if (scenario == "proc_voices1") {
        PapuProcessorHarness h;
        EngineParams p;
        h.init(1, p);
        std::vector<MidiEvt> events = {
            {0,   MidiEvt::NOTE_ON,  1, 60},
            {512, MidiEvt::NOTE_OFF, 1, 60},
        };
        auto fb = h.processBlock(1024, events, p);
        fwrite(fb.data(), sizeof(float), fb.size(), stdout);

    // ---- Phase 12: proc_voices2_notes ----
    // 2 voices, note 60 on voice 0 + note 64 on voice 1 at pos=0, block=2048.
    } else if (scenario == "proc_voices2_notes") {
        PapuProcessorHarness h;
        EngineParams p;
        h.init(2, p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
            {0, MidiEvt::NOTE_ON, 1, 64},
        };
        auto fb = h.processBlock(2048, events, p);
        fwrite(fb.data(), sizeof(float), fb.size(), stdout);

    // ---- Phase 12: proc_voices2_steal ----
    // 2 voices, 3 note-ons: 60→v0, 64→v1, 67→dropped (no free voice), block=1024.
    } else if (scenario == "proc_voices2_steal") {
        PapuProcessorHarness h;
        EngineParams p;
        h.init(2, p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
            {0, MidiEvt::NOTE_ON, 1, 64},
            {0, MidiEvt::NOTE_ON, 1, 67},
        };
        auto fb = h.processBlock(1024, events, p);
        fwrite(fb.data(), sizeof(float), fb.size(), stdout);

    // ---- Phase 12: proc_voices2_rrobin ----
    // 2 voices round-robin: note 60 on (v0, next=1), off at 256,
    // note 64 on at 512 (v1, next=0), note 67 on at 512 (v0, next=1), block=2048.
    } else if (scenario == "proc_voices2_rrobin") {
        PapuProcessorHarness h;
        EngineParams p;
        h.init(2, p);
        std::vector<MidiEvt> events = {
            {0,   MidiEvt::NOTE_ON,  1, 60},
            {256, MidiEvt::NOTE_OFF, 1, 60},
            {512, MidiEvt::NOTE_ON,  1, 64},
            {512, MidiEvt::NOTE_ON,  1, 67},
        };
        auto fb = h.processBlock(2048, events, p);
        fwrite(fb.data(), sizeof(float), fb.size(), stdout);

    // ---- Phase 13: proc_mid_block_note ----
    // 1 voice, note 60 on at pos=256 (mid-block), block=1024. First 256 samples silent.
    } else if (scenario == "proc_mid_block_note") {
        PapuProcessorHarness h;
        EngineParams p;
        h.init(1, p);
        std::vector<MidiEvt> events = {
            {256, MidiEvt::NOTE_ON, 1, 60},
        };
        auto fb = h.processBlock(1024, events, p);
        fwrite(fb.data(), sizeof(float), fb.size(), stdout);

    // ---- Phase 13: proc_multi_events ----
    // 2 voices, note-on/off at positions 0, 256, 512, 768, block=1024.
    } else if (scenario == "proc_multi_events") {
        PapuProcessorHarness h;
        EngineParams p;
        h.init(2, p);
        std::vector<MidiEvt> events = {
            {0,   MidiEvt::NOTE_ON,  1, 60},
            {0,   MidiEvt::NOTE_ON,  1, 64},
            {256, MidiEvt::NOTE_OFF, 1, 60},
            {512, MidiEvt::NOTE_ON,  1, 67},
            {768, MidiEvt::NOTE_OFF, 1, 64},
        };
        auto fb = h.processBlock(1024, events, p);
        fwrite(fb.data(), sizeof(float), fb.size(), stdout);

    // ---- Phase 13: proc_odd_block ----
    // 1 voice, note 60 on at 0, block=333 (non-power-of-2 size).
    } else if (scenario == "proc_odd_block") {
        PapuProcessorHarness h;
        EngineParams p;
        h.init(1, p);
        std::vector<MidiEvt> events = {
            {0, MidiEvt::NOTE_ON, 1, 60},
        };
        auto fb = h.processBlock(333, events, p);
        fwrite(fb.data(), sizeof(float), fb.size(), stdout);

    } else {
        fprintf(stderr, "Unknown scenario: '%s'\n", scenario.c_str());
        return 1;
    }

    return 0;
}
