# PAPURS — Game Boy APU Synth VST (Rust port of PAPU)

## Project Overview

Port the [PAPU](https://github.com/FigBug/PAPU) Game Boy sound chip synth's audio engine from C++ to Rust, producing a UI-less VST plugin. The original code (excluding git submodules) is licensed LGPL per `license-grant.txt`.

The original C++ engine lives in `papu-original/plugin/Source/` and consists of:
- **`gb_apu/`** — Blargg's Gb_Snd_Emu 0.1.4 + Blip_Buffer 0.3.4 (pure Game Boy APU emulation, LGPL)
- **`PluginProcessor.h/.cpp`** — `PAPUEngine` (MIDI→APU translation, vibrato, parameter mapping) and `PAPUAudioProcessor` (JUCE host integration, polyphony)

## Architecture

```
src/
  lib.rs          — crate root, re-exports modules
  blip.rs         — Port of Blip_Buffer 0.3.4 + Blip_Synth + Stereo_Buffer (bit-exact with C++ original)
  apu.rs          — Gb_Apu equivalent: 4 oscillators, register interface, frame sequencer
  engine.rs       — PAPUEngine equivalent: MIDI processing, note queues, vibrato LFO, parameter mapping
tests/
  integration.rs  — end-to-end: configure params → play MIDI → render → snapshot audio output
```

### Design decision: port Blip_Buffer 0.3.4, not use blip_buf crate

We port Blargg's Blip_Buffer 0.3.4 directly to Rust rather than using the `blip_buf` crate. Reasons:

1. **Bit-exact gold tests.** With the same BLEP algorithm, gold tests can use exact sample comparison against the C++ original. Every phase is simply "make the next gold test pass."
2. **Feature parity.** Blip_Buffer 0.3.4 has configurable `bass_freq` (PAPU uses 461 Hz → `bass_shift=4`), configurable quality levels (1-5), and fine/coarse modes. The `blip_buf` crate has a fixed `BASS_SHIFT=9` and single quality level — losing these means the port sounds different.
3. **Forward compatibility.** If upstream PAPU is updated, we can port those changes without fighting library differences.
4. **Self-contained.** No external C dependency; pure Rust.

| Aspect | Original C++ | Rust port |
|--------|-------------|-----------|
| BLEP library | Blargg Blip_Buffer 0.3.4 | Direct Rust port of same (bit-exact) |
| Stereo | 3-buffer Stereo_Buffer (center/left/right with Blip_Reader mixing) | Same architecture, ported to Rust |
| Vibrato LFO | `gin::LFO` (sine wave) | Pure Rust sine LFO |
| VST framework | JUCE/Gin | TBD (nih-plug or similar) |
| Polyphony | Up to 16 PAPUEngine instances | Same architecture |

## Testing

Decompose into modules with thin interfaces. Every module must be thoroughly tested — functionality that doesn't fail a test isn't implemented. Always run tests with `UPDATE_EXPECT=1`; the workflow is: run, review diff, commit.

Each module has one harness function taking a scenario and an `expect_test::Expect`. It formats everything you'd need to see to know the code works. Each project has one or two integration harnesses (in `tests/`) composing most modules except I/O.

```rust
fn check(input: &str, expect: Expect) {
    let ast = parse(input);
    let result = eval(&ast);
    expect.assert_eq(&format!("{ast:#?}\n---\n{result:#?}"));
}

#[test]
fn tests() {
    check("1+2", expect![[r#"
        Add(Lit(1), Lit(2))
        ---
        3
    "#]]);
}
```

Design scenario eDSLs for readability, writability, and easy forking. Use `Debug` by default; invest in custom `Debug` impls or formats for heavily-used harnesses.

```rust
fn check(workers: usize, tasks: &[Task], expect: Expect) {
    let trace = Scheduler::new(workers).run(tasks);
    expect.assert_debug_eq(&trace);
}

#[test]
fn tests() {
    let base = tasks(&[
        task("fetch").duration(2),
        task("parse").duration(3).after("fetch"),
        task("index").duration(1).after("fetch"),
    ]);

    check(1, &base, expect![[r#"
        [
            (t=0, W0, Start("fetch")),
            (t=2, W0, Start("parse")),
            (t=5, W0, Start("index")),
            (t=6, Done),
        ]
    "#]]);

    check(2, &base, expect![[r#"
        [
            (t=0, W0, Start("fetch")),
            (t=2, W0, Start("parse")),
            (t=2, W1, Start("index")),
            (t=3, W1, Idle),
            (t=5, Done),
        ]
    "#]]);

    check(2, &base.and(&[
        task("render").duration(2).after("parse").after("index"),
    ]), expect![[r#"
        [
            (t=0, W0, Start("fetch")),
            (t=2, W0, Start("parse")),
            (t=2, W1, Start("index")),
            (t=3, W1, Idle),
            (t=5, W0, Start("render")),
            (t=7, Done),
        ]
    "#]]);
}
```

For non-text output (audio, images), write to content-addressed gitignored files. The prefix lets tooling launch the right diff viewer.

```rust
fn check(input: &str, expect: Expect) {
    let wav_data = synth.render(input);
    testfile("wav", &wav_data, expect);
}

#[test]
fn tests() {
    check("A4 quarter", expect![[r#"wav:a1b2c3d4e5..."#]]);
}
```

## Porting Roadmap

See [ROADMAP.md](ROADMAP.md) for the full phase-by-phase plan with gold test specifications.

## Running Tests

```bash
# Normal test run (verifies snapshots match)
cargo test

# Update snapshots after intentional changes (review diffs before committing!)
UPDATE_EXPECT=1 cargo test
```

## Original C++ Reference

The original PAPU code is in `papu-original/` as a git submodule. Key files:

- `papu-original/plugin/Source/PluginProcessor.h` — PAPUEngine + PAPUAudioProcessor declarations
- `papu-original/plugin/Source/PluginProcessor.cpp` — Full implementation
- `papu-original/plugin/Source/gb_apu/Gb_Apu.h/.cpp` — APU top-level
- `papu-original/plugin/Source/gb_apu/Gb_Oscs.h/.cpp` — Oscillator implementations
- `papu-original/plugin/Source/gb_apu/Blip_Buffer.h/.cpp` — Band-limited synthesis
- `papu-original/plugin/Source/gb_apu/Blip_Synth.h` — Synthesis template
- `papu-original/plugin/Source/gb_apu/Multi_Buffer.h/.cpp` — Stereo buffer

### Game Boy APU registers (0xff10-0xff3f)

| Address | Channel | Register |
|---------|---------|----------|
| 0xff10 | Square 1 | Sweep (period, direction, shift) |
| 0xff11 | Square 1 | Length/Duty |
| 0xff12 | Square 1 | Envelope (volume, direction, period) |
| 0xff13 | Square 1 | Frequency low 8 bits |
| 0xff14 | Square 1 | Trigger, length enable, frequency high 3 bits |
| 0xff15 | Square 2 | (unused) |
| 0xff16 | Square 2 | Length/Duty |
| 0xff17 | Square 2 | Envelope |
| 0xff18 | Square 2 | Frequency low |
| 0xff19 | Square 2 | Trigger, length enable, frequency high |
| 0xff1A | Wave | Enable (bit 7) |
| 0xff1B | Wave | Length |
| 0xff1C | Wave | Volume (bits 5-6) |
| 0xff1D | Wave | Frequency low |
| 0xff1E | Wave | Trigger, length enable, frequency high |
| 0xff1F | Noise | (unused) |
| 0xff20 | Noise | Length |
| 0xff21 | Noise | Envelope |
| 0xff22 | Noise | Shift, step, divisor ratio |
| 0xff23 | Noise | Trigger, length enable |
| 0xff24 | Global | Master volume (L/R) |
| 0xff25 | Global | Panning (per-oscillator L/R enable) |
| 0xff26 | Global | APU enable, oscillator status (read) |
| 0xff30-3f | Wave | Wave RAM (16 bytes = 32 4-bit samples) |

### Wavetable presets (from Pokemon Red/Crystal)

15 preset waveforms (indices 0-14) are hardcoded in `wave_samples[15][32]` in `PluginProcessor.h`.

### Constants

- Game Boy CPU clock: 4,194,304 Hz
- Frame sequencer: 256 Hz (every 16,384 clocks)
- Envelope clock: 64 Hz (every 4th frame)
- Sweep clock: 128 Hz (every 2nd frame, odd)
- Default treble EQ: -20.0 (prepareToPlay) / -30.0 (parameter default)
- Default bass frequency: 461 Hz
- Default output level: 7
- MIDI pitch bend range: -2 to +2 semitones
