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

The port proceeds in phases. Each phase adds functionality by writing gold tests first — tests that produce C++ reference output from the original PAPU, then verify the Rust port produces bit-exact matching output. The workflow: write the gold test, watch it fail, implement until it passes.

All gold tests follow this pattern:
1. Build a C++ test harness (once, in Phase 1) that drives the original PAPU engine headlessly
2. For each scenario, generate reference output from C++ and check it into the repo
3. The Rust test runs the same scenario and compares output sample-by-sample

Everything is single-voice until Phase 12 (polyphony).

### Phase 0: Port Blip_Buffer 0.3.4 to Rust (`src/blip.rs`)

Port Blargg's Blip_Buffer 0.3.4 + Blip_Synth + Stereo_Buffer to pure Rust, targeting bit-exact output with the C++ original. This replaces the `blip_buf` crate dependency.

**What to port** (from `papu-original/plugin/Source/gb_apu/`):
- `Blip_Buffer` — sample buffer with clock rate conversion, bass frequency high-pass filter, `end_frame`/`read_samples`
- `Blip_Synth<quality,range>` — band-limited impulse synthesis with configurable quality/range, fine/coarse modes
- `Blip_Impulse_` — DSF impulse table generation (`treble_eq`), volume scaling
- `Blip_Reader` — accumulator-based sample reader with high-pass filter (used by stereo mixer)
- `Stereo_Buffer` — 3x Blip_Buffer (center/left/right), stereo mixing via Blip_Reader
- `blip_eq_t` — treble EQ parameters

**Key implementation details:**
- `buf_t_` is `u16` with `sample_offset_ = 0x7F7F` bias (allows memset clearing)
- `Blip_Synth::offset_resampled` uses pair-wise `u32` arithmetic (two `u16` samples packed)
- `treble_eq` generates impulse tables via DSF synthesis (Stilson & Smith 1996)
- Fine mode interpolates between two impulse tables for large amplitude ranges
- Quality parameter controls impulse width: `width = quality * 4` (quality 1-4), `width = 24` (quality 5)
- PAPU uses `Blip_Synth<blip_good_quality, 15*7*2>` for squares and `Blip_Synth<blip_med_quality, 15*7*2>` for wave/noise

**Test 0.1: Single impulse — bit-exact with C++**
Feed a single delta of amplitude +1000 at clock time 0 into both C++ Blip_Buffer and Rust port, configured at clock_rate=4194304, sample_rate=44100, bass_freq=461, treble_eq=-20. Read all output samples. Verify bit-exact match.

**Test 0.2: Square wave at 440 Hz — bit-exact**
Alternate +volume/-volume deltas at period=(2048-1046)*4=4008 clocks (A4 ≈ 440 Hz). Render 1024 output samples with both. Verify bit-exact.

**Test 0.3: Bass frequency filter**
Render a DC step with bass_freq=461. Verify the high-pass decay matches C++. Then test bass_freq=16 (default) and bass_freq=600. All bit-exact.

**Test 0.4: Stereo mixing**
Add deltas to center, left, and right buffers independently. Read interleaved stereo output. Verify L = center + left, R = center + right, bit-exact with C++ Stereo_Buffer.

**Test 0.5: Fine mode vs normal mode**
Test with `Blip_Synth<blip_good_quality, 210>` (15*7*2, fine mode since range > 512 is false but let's verify) and verify impulse table generation matches C++.

**Test 0.6: Multiple end_frame cycles**
Call end_frame(1024) repeatedly, reading samples between frames. Verify no timing drift vs C++ across 100 frames.

### Phase 1: Gold Test Scaffolding

Build the C++ test harness that drives PAPU's audio engine headlessly (no JUCE, no UI) and produces reference output files. Build the Rust test harness that loads reference files and compares.

**C++ harness** (in `tests/cpp_harness/`):
- Compile `Gb_Apu`, `Gb_Oscs`, `Blip_Buffer`, `Multi_Buffer` standalone (no JUCE dependency)
- Wrap `PAPUEngine`'s register-writing logic (or replicate it — it's thin)
- Accept a scenario description: parameters, MIDI events with timestamps, block size, sample rate
- Output raw interleaved i16 stereo samples to stdout/file

**Rust harness** (in `tests/`):
- For each gold test: load the C++ reference, run the same scenario through Rust, compare sample-by-sample
- Use `expect_test` for snapshot-based assertions on short outputs
- Use content-addressed files for longer audio (per testing philosophy)

**Test 1.1: Silence**
No notes, default parameters, render 512 samples at 44100 Hz. Both C++ and Rust output silence (all zeros or near-zero from high-pass settling). Bit-exact.

**Test 1.2: Single square wave note — default parameters**
Play MIDI note 60 (C4) on channel 1 with default parameters (Pulse 1: duty=0, A=1, OL=on, OR=on, output=7). Render 2048 samples. C++ reference → check in. Rust must match bit-exact.

### Phase 2: Square Wave Oscillator (`src/apu.rs`)

Port Gb_Osc base + Gb_Square. No envelope, sweep, or length counter yet — hardcode volume=15, enabled=true.

**Test 2.1: 50% duty at A4**
Configure square wave: frequency register for 440 Hz, duty=2 (50%), volume=15, global_volume=7. Render 2048 samples. Gold test vs C++.

**Test 2.2: All four duty cycles**
Duty 0 (12.5%), 1 (25%), 2 (50%), 3 (75%) at same frequency. Gold test each.

**Test 2.3: Frequency extremes**
Min frequency (period=8192), max usable frequency (period=28), and period<27 (should produce silence). Gold test each.

**Test 2.4: Phase continuity across frames**
Render two consecutive end_frame(1024) calls. Gold test verifies no glitch at boundary.

**Test 2.5: Volume=0 silence optimization**
Set volume=0. Verify output is silent and `last_amp` is zeroed (the early-exit path in `Gb_Square::run`).

### Phase 3: Envelope Generator (`src/apu.rs`)

Port Gb_Env (extends Gb_Osc). Square wave now uses real envelope.

**Test 3.1: Attack ramp — square wave with A=1**
Play note with attack=1 (env_period=1, env_dir=up, initial volume=0). Gold test: output amplitude ramps up over time as envelope clocks at 64 Hz.

**Test 3.2: Sustain at max volume — A=0**
Play note with attack=0 (envelope starts at volume=15, no ramping). Gold test: immediate full volume.

**Test 3.3: Release — note off with R=3**
Play note, then release. Gold test: amplitude decays as envelope counts down.

**Test 3.4: Attack then release**
Play note with A=3 for 0.25s, release with R=5. Gold test: full attack-sustain-release envelope visible in output.

**Test 3.5: Envelope period=0 (disabled)**
Set env_period=0. Gold test: volume stays constant.

### Phase 4: Frequency Sweep (`src/apu.rs`)

Port sweep unit (Square 1 only, `has_sweep=true`).

**Test 4.1: Upward sweep**
sweep_period=1, sweep_shift=2, sweep_dir=up. Play note, render long enough for several sweep clocks. Gold test: frequency rises audibly.

**Test 4.2: Downward sweep**
sweep_dir=down. Gold test: frequency falls.

**Test 4.3: Sweep overflow → silence**
Set up sweep that will push frequency >= 2048. Gold test: channel goes silent at the right moment.

**Test 4.4: Sweep on trigger**
Trigger with sweep_period>0, sweep_shift>0. Gold test: immediate clock_sweep() on trigger produces correct initial frequency.

### Phase 5: Length Counter

Port length counter logic in Gb_Osc.

**Test 5.1: Length countdown silences channel**
Set length to a small value, length_enabled=true. Gold test: channel plays for the expected duration then silence.

**Test 5.2: Length disabled**
length_enabled=false. Gold test: plays indefinitely.

### Phase 6: Wave Channel (`src/apu.rs`)

Port Gb_Wave (wavetable oscillator).

**Test 6.1: Pokemon waveform 0**
Load wave_samples[0], set volume=1 (full), play a note. Gold test: wavetable output.

**Test 6.2: All 15 waveforms**
Gold test for each of the 15 Pokemon waveforms at the same frequency.

**Test 6.3: Wave volume levels**
Volume 0 (mute), 1 (100%), 2 (50%), 3 (25%). Gold test each.

**Test 6.4: Wave enable/disable**
Write 0x00 to register 0 (disable). Gold test: silence. Write 0x80 (enable). Gold test: output.

**Test 6.5: Zero-crossing disable (stopWave)**
PAPU's custom feature: `disableOnZeroCrossing=32`. Gold test: wave stops cleanly at next zero crossing.

### Phase 7: Noise Channel (`src/apu.rs`)

Port Gb_Noise (LFSR generator).

**Test 7.1: 15-bit LFSR noise**
tap=14, default divisor/shift. Gold test: noise output matches C++.

**Test 7.2: 7-bit LFSR noise**
tap=6 (7-bit mode). Gold test: shorter, more tonal pattern.

**Test 7.3: Various divisor/shift combos**
Test divisor=0,3,7 with shift=0,4,8. Gold test each combination.

**Test 7.4: Noise with envelope**
Noise + attack/release envelope. Gold test: amplitude-modulated noise.

### Phase 8: APU Register Interface & Frame Sequencer (`src/apu.rs`)

Port Gb_Apu top-level: register routing, panning, global volume, frame sequencer.

**Test 8.1: Register routing**
Write all registers 0xff10-0xff3f via the Gb_Apu interface. Gold test: verify each oscillator receives correct register/value.

**Test 8.2: Global volume**
Render with output=7, then output=3. Gold test: amplitude scales correctly.

**Test 8.3: Stereo panning**
Configure oscillator 1 to left only, oscillator 2 to right only. Gold test: correct channel routing.

**Test 8.4: APU enable/disable**
Write 0x00 to 0xff26 (disable all). Gold test: silence. Write 0x80 (re-enable). Gold test: output resumes.

**Test 8.5: All four channels simultaneously**
Square1=C4, Square2=E4, Wave=G4, Noise. All playing at once, default panning. Gold test: mixed output.

**Test 8.6: Frame sequencer timing**
Run APU for several seconds. Gold test: verify length/envelope/sweep clock at correct rates (256/64/128 Hz).

### Phase 9: MIDI → APU Engine (`src/engine.rs`)

Port PAPUEngine: MIDI processing, note queues, frequency calculation, register cache.

**Test 9.1: Single note on/off**
MIDI note 60 on, render, note off, render. Gold test vs C++ PAPUEngine output.

**Test 9.2: Monophonic last-note priority**
Notes 60, 64, 67 on; release in reverse order. Gold test: each note resumes correctly.

**Test 9.3: MIDI note → Game Boy frequency**
Test several notes across the MIDI range. Gold test: exact register values match.

**Test 9.4: Pitch bend**
Pitch wheel = 12288 (+2 semitones at half range). Gold test: frequency shifts.

**Test 9.5: Channel split**
Enable channel_split, play on MIDI channels 1-4. Gold test: correct oscillator routing.

**Test 9.6: Register cache**
Same parameters twice → no duplicate register writes. Gold test: verify via register write log.

**Test 9.7: Waveform switching**
Change waveform 0→7. Gold test: wave RAM rewrite sequence.

### Phase 10: Vibrato LFO (`src/engine.rs`)

Port the sine-wave vibrato.

**Test 10.1: Vibrato disabled**
amount=0. Gold test: stable frequency.

**Test 10.2: Vibrato active**
rate=5 Hz, amount=50%. Gold test: frequency modulation visible in output.

**Test 10.3: Vibrato reset on new note**
Gold test: phase resets to 0 on note change.

**Test 10.4: Vibrato unaffected by pitch bend**
Gold test: vibrato phase continues through pitch bend.

### Phase 11: Full Parameter Mapping (`src/engine.rs`)

Port the complete parameter-to-register translation, including all 40 parameters.

**Test 11.1: Pulse 1 full params**
duty=2, A=3, R=5, tune=+12, fine=+50, sweep=-3, shift=2, OL=on, OR=on. Gold test.

**Test 11.2: Pulse 2 full params**
Similar to 11.1, no sweep. Gold test.

**Test 11.3: Wave params**
waveform=5, tune=-7, fine=-25, OL=on, OR=off. Gold test.

**Test 11.4: Noise params**
shift=8, step=1, ratio=3, A=0, R=4. Gold test.

**Test 11.5: Global params**
output=5, treble=-30, bass=461. Gold test: verify buffer EQ settings + volume register.

### Phase 12: Polyphony (`src/engine.rs`)

Port multi-voice support (PAPUAudioProcessor level).

**Test 12.1: Single voice**
voices=1. Gold test: identical to Phase 9 tests.

**Test 12.2: Two voices**
voices=2, play two overlapping notes. Gold test: both contribute to output.

**Test 12.3: Voice stealing**
voices=2, play 3 notes. Gold test: matches C++ behavior (no free voice → note dropped).

**Test 12.4: Round-robin allocation**
Gold test: findFreeVoice wraps correctly.

### Phase 13: Audio Rendering Pipeline (`src/engine.rs`)

Port the full processBlock pipeline with per-sample MIDI granularity.

**Test 13.1: Note on mid-block**
Note-on at sample 256 in a 512-sample block. Gold test: silence then signal.

**Test 13.2: Note off with release**
Note-off mid-block. Gold test: release envelope begins at correct position.

**Test 13.3: Multiple MIDI events per block**
Note-on at 0, pitch bend at 100, note-off at 300 in 512-sample block. Gold test.

**Test 13.4: Frame boundary handling**
Block sizes that don't align to 1024-clock frames. Gold test: no artifacts.

**Test 13.5: Output level scaling**
Verify raw i16 samples / 32768.0 = float output in [-1.0, 1.0]. Gold test.

### Phase 14: VST Plugin Shell

Wire up the engine as a VST plugin (e.g., nih-plug).

**Test 14.1: Plugin loads**
Instantiate plugin. Verify stereo out, MIDI in, all 40 parameters exposed.

**Test 14.2: Parameter names/ranges match original**
Verify all parameter metadata matches the original PAPU.

**Test 14.3: Offline render matches engine**
Process buffer through VST interface. Gold test: identical to direct engine output.

### Phase 15: Musical Integration Tests

Port real musical content.

**Test 15.1-15.15: RPG Chiptune MIDI files**
Port the 15 melodic RPG chiptune MIDI files from [OpenGameArt](https://opengameart.org/content/15-melodic-rpg-chiptunes) to play well on the original PAPU (may require channel remapping, velocity-to-volume mapping, etc.). Then use them as gold tests:

For each MIDI file:
1. Play through the original C++ PAPU → reference render (checked in as content-addressed file)
2. Play through the Rust port → test render
3. Compare bit-exact

Order by complexity:
- Test 15.1: Simplest/shortest track
- Test 15.2-15.10: Progressively more complex
- Test 15.11-15.14: All 4 channels simultaneously
- Test 15.15: Most complex track

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
