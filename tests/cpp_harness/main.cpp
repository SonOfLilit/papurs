// Gold test harness for PAPURS — drives Gb_Apu + Stereo_Buffer standalone (no JUCE).
// Accepts a scenario name, writes raw little-endian i16 stereo pairs to stdout.
//
// Usage: ./harness <scenario>
// Scenarios:
//   silence  — 512 stereo pairs, APU initialized, no notes playing
//   note60   — 2048 stereo pairs, Square 1, MIDI note 60 (C4), default params

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <string>
#include <cstdint>

// gb_apu headers (compile with -I pointing to gb_apu/ directory)
#include "Gb_Apu.h"
#include "Multi_Buffer.h"

static const long CLOCK_RATE  = 4194304; // Game Boy CPU clock Hz
static const long SAMPLE_RATE = 44100;
static const int  FRAME_SIZE  = 1024;    // clocks per render frame (matches PAPUEngine)

// Wave RAM preset 0 (32 samples, 4-bit each) from Pokemon Red/Crystal
// Matches wave_samples[0] in PluginProcessor.h
static const uint8_t WAVE_PRESET_0[32] = {
     0,  2,  4,  6,  8, 10, 12, 14, 15, 15, 15, 14, 14, 13, 13, 12,
    12, 11, 10,  9,  8,  7,  6,  5,  4,  4,  3,  3,  2,  2,  1,  1
};

struct GoldHarness {
    Gb_Apu apu;
    Stereo_Buffer buf;
    blip_time_t time;

    // Initialize APU and buffers — matches PAPUEngine::prepareToPlay()
    void init() {
        time = 0;

        // Match PAPUEngine::prepareToPlay() exactly
        apu.treble_eq(blip_eq_t(-20.0));
        buf.bass_freq(461);
        buf.clock_rate(CLOCK_RATE);
        buf.set_sample_rate(SAMPLE_RATE);
        apu.output(buf.center(), buf.left(), buf.right());

        // Load wave RAM with preset 0 (as in prepareToPlay)
        write_reg(0xff1A, 0x00); // disable wave channel before write
        for (int s = 0; s < 16; s++) {
            uint8_t high = WAVE_PRESET_0[s * 2];
            uint8_t low  = WAVE_PRESET_0[s * 2 + 1];
            write_reg(0xff30 + s, (int)(low | (high << 4)));
        }
        write_reg(0xff1A, 0x80); // re-enable wave channel

        // Enable APU (bit 7) and set initial oscillator enables
        write_reg(0xff26, 0x8f);
    }

    // Write a register at current time, advancing time by 4 clocks (matches clock())
    void write_reg(int addr, int val) {
        time += 4;
        apu.write_register(time, (gb_addr_t)addr, val);
    }

    // Render n_pairs stereo frames to file (raw little-endian i16 interleaved L,R)
    void render(int n_pairs, FILE* out) {
        int done = 0;
        while (done < n_pairs) {
            long avail = buf.samples_avail();
            if (avail > 0) {
                blip_sample_t samples[1024]; // 512 stereo pairs max per read
                int count = (int)std::min(std::min((long)(n_pairs - done), avail), (long)512);
                count = (int)buf.read_samples(samples, count);
                fwrite(samples, sizeof(blip_sample_t), (size_t)(count * 2), out);
                done += count;
            } else {
                // Advance one 1024-clock frame — matches PAPUEngine::runUntil()
                time = 0;
                bool stereo = apu.end_frame(FRAME_SIZE);
                buf.end_frame(FRAME_SIZE, stereo);
            }
        }
    }
};

// Convert MIDI note number to GB frequency register (11-bit period value)
// Formula from PAPUEngine: period = ((4194304 / freq) - 65536) / -32
static uint16_t midi_to_gb_period(int note) {
    double freq = 440.0 * pow(2.0, (note - 69) / 12.0);
    return (uint16_t)(((4194304.0 / freq) - 65536.0) / -32.0);
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <scenario>\n", argv[0]);
        return 1;
    }

    const std::string scenario = argv[1];

    if (scenario == "silence") {
        // Test 0.1: APU initialized, no notes, 512 stereo pairs
        GoldHarness h;
        h.init();
        // Set output volume and panning (default: output=7, pulse1 L+R on)
        h.write_reg(0xff24, 0x08 | 7); // NR50: right Vin + volume=7
        h.write_reg(0xff25, 0x11);     // NR51: pulse1 → left (bit4) + right (bit0)
        h.render(512, stdout);

    } else if (scenario == "note60") {
        // Test 0.2: Square 1, MIDI note 60 (C4), default params, 2048 stereo pairs
        // Default params: duty=0 (12.5%), A=1 (attack), OL=on, OR=on, output=7
        GoldHarness h;
        h.init();

        // Set output volume and panning (matches processBlock preamble)
        h.write_reg(0xff24, 0x08 | 7); // NR50
        h.write_reg(0xff25, 0x11);     // NR51

        // Square 1 note-on: note 60, duty=0, A=1, trigger=true
        // Matches runOscs(curNote1=60, ..., trigger1=true, ...)
        uint16_t period = midi_to_gb_period(60);
        h.write_reg(0xff10, 0x00);                          // NR10: no sweep
        h.write_reg(0xff11, 0 << 6);                        // NR11: duty=0 (12.5%)
        h.write_reg(0xff13, period & 0xff);                 // NR13: freq low 8 bits
        h.write_reg(0xff12, 0x00 | (1 << 3) | 1);          // NR12: A=1 → 0x09
        h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07)); // NR14: trigger + freq high

        h.render(2048, stdout);

    } else {
        fprintf(stderr, "Unknown scenario: '%s'\n", scenario.c_str());
        fprintf(stderr, "Available: silence, note60\n");
        return 1;
    }

    return 0;
}
