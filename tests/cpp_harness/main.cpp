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

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <algorithm>
#include <string>
#include <cstdint>
#include <initializer_list>

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

    } else {
        fprintf(stderr, "Unknown scenario: '%s'\n", scenario.c_str());
        return 1;
    }

    return 0;
}
