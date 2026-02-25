# PAPURS — Game Boy APU Synth VST (Rust port of PAPU)

## Project Overview

Port the [PAPU](https://github.com/FigBug/PAPU) Game Boy sound chip synth's audio engine from C++ to Rust, producing a UI-less VST plugin. The original code (excluding git submodules) is licensed LGPL per `license-grant.txt`.

The original C++ engine lives in `papu-original/plugin/Source/` and consists of:
- **`gb_apu/`** — Blargg's Gb_Snd_Emu 0.1.4 + Blip_Buffer 0.3.4 (pure Game Boy APU emulation, LGPL)
- **`PluginProcessor.h/.cpp`** — `PAPUEngine` (MIDI→APU translation, vibrato, parameter mapping) and `PAPUAudioProcessor` (JUCE host integration, polyphony)

Our Rust port uses the [`blip_buf`](https://crates.io/crates/blip_buf) crate for band-limited synthesis.

## Architecture

```
src/
  lib.rs          — crate root, re-exports modules
  blip.rs         — Stereo buffer wrapper around blip_buf::BlipBuf (replaces Blip_Buffer + Stereo_Buffer + Multi_Buffer)
  apu.rs          — Gb_Apu equivalent: 4 oscillators, register interface, frame sequencer
  engine.rs       — PAPUEngine equivalent: MIDI processing, note queues, vibrato LFO, parameter mapping
tests/
  integration.rs  — end-to-end: configure params → play MIDI → render → snapshot audio output
```

### Key differences from original

| Aspect | Original C++ | Rust port |
|--------|-------------|-----------|
| BLEP library | Blargg Blip_Buffer 0.3.4 (windowed sinc, configurable quality/range) | `blip_buf` crate (fixed BL_STEP table, single quality level) |
| Stereo | 3-buffer Stereo_Buffer (center/left/right with Blip_Reader mixing) | 3x BlipBuf + custom stereo mixer |
| Vibrato LFO | `gin::LFO` (sine wave) | Pure Rust sine LFO |
| VST framework | JUCE/Gin | TBD (nih-plug or similar) |
| Polyphony | Up to 16 PAPUEngine instances | Same architecture |

### BLEP compatibility note

**CRITICAL**: Blargg's Blip_Buffer 0.3.4 and the `blip_buf` crate use fundamentally different BLEP algorithms:
- Blip_Buffer 0.3.4: Dynamically generates windowed sinc impulse tables via DSF synthesis, with configurable quality levels (1-5) and fine/coarse modes based on amplitude range
- `blip_buf` crate: Uses a fixed 33-phase BL_STEP lookup table with 8-wide impulse and linear interpolation

These will **NOT** produce bit-exact output. The first milestone is to quantify the difference and establish an appropriate comparison tolerance for gold tests.

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

The port proceeds in phases. Each phase has a series of test scenarios that must be written (using the test harness) and then made to pass. Tests are written against the Rust port; where noted, we also verify against the C++ original for gold-master comparison.

### Phase 0: BLEP Foundation — Quantify blip_buf vs Blip_Buffer differences

Before any APU porting, we must understand how `blip_buf` (Rust) differs from `Blip_Buffer 0.3.4` (C++). This determines whether gold tests can use exact sample comparison or need a tolerance metric.

**Test 0.1: Single impulse response comparison**
Feed a single delta of amplitude +1000 at clock time 0 into both libraries configured at clock_rate=4194304, sample_rate=44100. Read back all output samples. Compare the impulse response shapes. Document the peak difference and RMS difference.

**Test 0.2: Square wave comparison at multiple frequencies**
Generate a 50% duty cycle square wave by alternating +volume/-volume deltas at specific periods. Test at:
- 440 Hz (A4, typical musical frequency)
- 100 Hz (low bass, long period)
- 4000 Hz (high treble)
- 10000 Hz (near Nyquist, where anti-aliasing matters most)

For each frequency, render 1024 output samples with both libraries and compute max absolute sample difference and RMS difference.

**Test 0.3: Establish comparison metric**
Based on results from 0.1 and 0.2, decide on:
- If max difference < 1 LSB: use exact comparison
- If max difference is small but nonzero: use SNR threshold (e.g., >90dB)
- If differences are significant: use perceptual metric (e.g., spectral similarity) or accept approximate matching with documented tolerance

Document the chosen metric and threshold in this file.

### Phase 1: Stereo BlipBuf wrapper (`src/blip.rs`)

Port the `Stereo_Buffer` functionality: 3 internal `BlipBuf` instances (center/left/right), stereo mixing with high-pass filter, clock rate and sample rate management.

**Test 1.1: Mono buffer basics**
Create a stereo buffer at 44100 Hz with clock rate 4194304. Add a single delta to the center channel. Call `end_frame`. Read samples. Verify both L and R channels contain the same signal (mono routing).

**Test 1.2: Stereo panning**
Add deltas only to the left channel buffer. Read interleaved stereo output. Verify left channel has signal, right channel is silent (plus center contribution only).

**Test 1.3: Center + left + right mixing**
Add different deltas to center, left, and right buffers. Read output. Verify L = center + left, R = center + right.

**Test 1.4: Bass frequency filter**
Configure bass_freq at 461 Hz (PAPU default). Render a DC step. Verify the high-pass filter causes the signal to decay back toward zero. Compare decay rate against expected bass_shift calculation.

**Test 1.5: Multiple end_frame cycles**
Call end_frame repeatedly with 1024-clock frames. Verify samples accumulate correctly and timing doesn't drift across frame boundaries.

### Phase 2: Square Wave Oscillator (`src/apu.rs` — Gb_Square)

Port the square wave oscillator including duty cycle, period calculation, and phase accumulation. No envelope, sweep, or length counter yet.

**Test 2.1: Square wave at 50% duty, fixed frequency**
Configure a square wave at MIDI note 69 (A4, 440 Hz). Duty=2 (50%). Volume=15, global_volume=7. Render 2048 samples. Snapshot the output waveform (first 64 samples as integers).

**Test 2.2: All four duty cycles**
For duty values 0-3 (12.5%, 25%, 50%, 75%), render one period of the square wave at a fixed frequency. Verify the duty cycle pattern: duty_table = [1, 2, 4, 6] means phase transitions at those points in the 8-step cycle.

**Test 2.3: Frequency range**
Test square wave at minimum frequency (period = 2048*4 = 8192 clocks), maximum usable frequency (period ~28 clocks, just above the period<27 cutoff), and verify silence when period < 27.

**Test 2.4: Phase continuity across frames**
Render two consecutive frames. Verify the phase carries over correctly (no glitch at frame boundary).

**Test 2.5: Volume = 0 produces silence**
Set volume to 0. Verify no samples are generated (optimization path in original).

### Phase 3: Envelope Generator (`src/apu.rs` — Gb_Env)

Port the volume envelope: attack (volume increase) and release (volume decrease) at programmable rates.

**Test 3.1: Envelope attack (volume ramp up)**
Set initial volume=0, env_dir=up, env_period=1. Clock the envelope multiple times. Verify volume ramps 0→1→2→...→15 and stays at 15.

**Test 3.2: Envelope release (volume ramp down)**
Set initial volume=15, env_dir=down, env_period=2. Clock the envelope. Verify volume decreases by 1 every 2 clocks: 15→14→13→...→0.

**Test 3.3: Envelope period = 0 (disabled)**
Set env_period=0. Verify envelope never clocks (volume stays constant).

**Test 3.4: Trigger re-initializes envelope**
Write trigger bit. Verify env_delay resets to env_period and volume resets to new_volume.

**Test 3.5: Envelope integrated with square wave**
Render a square wave with envelope attack. Verify the amplitude changes over time as the envelope progresses (visible in rendered samples).

### Phase 4: Frequency Sweep (`src/apu.rs` — Gb_Square sweep)

Port the frequency sweep unit (Square 1 only).

**Test 4.1: Upward sweep**
Set sweep_period=1, sweep_shift=2, sweep_dir=0 (up). Trigger. Clock sweep. Verify frequency increases by freq >> shift each period.

**Test 4.2: Downward sweep**
Set sweep_dir=1 (down). Verify frequency decreases.

**Test 4.3: Sweep overflow disables channel**
Sweep upward until frequency >= 2048. Verify the channel is silenced (sweep_freq = 2048, sweep_delay = 0).

**Test 4.4: Sweep underflow clamped to 0**
Sweep downward until frequency would go negative. Verify frequency clamps to 0.

**Test 4.5: Sweep on trigger with immediate clock**
When trigger is written with sweep_period>0 and sweep_shift>0, sweep_delay is set to 1 and clock_sweep() is called immediately. Verify this behavior.

### Phase 5: Length Counter

Port the length counter (all oscillators).

**Test 5.1: Length countdown silences channel**
Set length=10, length_enabled=true. Clock length 10 times. Verify channel continues playing for 10 clocks then produces silence.

**Test 5.2: Length disabled**
Set length_enabled=false. Verify channel plays indefinitely regardless of length value.

**Test 5.3: Length reload on frequency write**
Write to register 3 (frequency low) or register 4 (frequency high). Verify length reloads from new_length.

### Phase 6: Wave Channel (`src/apu.rs` — Gb_Wave)

Port the wavetable oscillator.

**Test 6.1: Default waveform playback**
Load wave_samples[0] (Pokemon waveform 0) into wave RAM. Set volume_shift=0 (full volume). Render output. Snapshot the waveform shape.

**Test 6.2: All 15 Pokemon waveforms**
For each of the 15 preset waveforms, render a short snippet and snapshot. Verify the wave data is read correctly from the 32-sample table.

**Test 6.3: Wave volume levels**
Test all 4 volume levels: 0 (mute), 1 (100%, shift=0), 2 (50%, shift=1), 3 (25%, shift=2). Verify amplitude scaling.

**Test 6.4: Wave enable/disable**
Write 0x00 to register 0 (disable), then 0x80 (enable). Verify wave only plays when enabled.

**Test 6.5: Wave position reset on trigger**
Trigger the wave channel. Verify wave_pos resets to 0.

**Test 6.6: Zero-crossing disable (stopWave)**
Call stopWave (disableOnZeroCrossing=32). Verify the wave stops at the next zero crossing (or after one full period, whichever comes first). This is PAPU's custom addition for clean note-off.

**Test 6.7: Wave period calculation**
Verify wave period = (2048 - frequency) * 2 (half the square wave period for same frequency value).

### Phase 7: Noise Channel (`src/apu.rs` — Gb_Noise)

Port the LFSR noise generator.

**Test 7.1: 15-bit LFSR sequence**
Set tap=14 (15-bit mode). Trigger. Render a sequence of noise output bits. Verify the LFSR produces the correct pseudo-random pattern.

**Test 7.2: 7-bit LFSR sequence**
Set tap=6 (7-bit mode). Verify shorter, more tonal noise pattern.

**Test 7.3: Noise divisor and shift**
Test multiple divisor (0-7) and shift (0-13) combinations. Verify period = (divisor * 16 or 8 if 0) << shift.

**Test 7.4: Noise with envelope**
Combine noise with envelope attack/release. Verify amplitude changes apply to noise output.

**Test 7.5: Noise trigger resets LFSR**
Trigger the noise channel. Verify bits resets to ~0 (all 1s).

### Phase 8: APU Register Interface (`src/apu.rs` — Gb_Apu)

Port the top-level APU register write/read routing, panning, and global volume.

**Test 8.1: Register routing**
Write to each register address 0xff10-0xff3f. Verify the correct oscillator receives the correct register index and data.

**Test 8.2: Global volume (0xff24)**
Change global volume. Verify all oscillator output amplitudes scale accordingly. Test the amplitude correction logic that adjusts last_amp when global volume changes mid-stream.

**Test 8.3: Stereo panning (0xff25)**
Set various panning configurations. Verify each oscillator routes to the correct L/R/center buffer. Test output_select calculation: `(bits >> 3 & 2) | (bits & 1)`.

**Test 8.4: APU enable/disable (0xff26)**
Write 0x00 to 0xff26. Verify all oscillators are silenced. Write 0x80. Verify they can play again.

**Test 8.5: Wave RAM writes (0xff30-0xff3f)**
Write 16 bytes to wave RAM. Verify the 32-nibble wave table is correctly populated (high nibble first, then low nibble for each byte).

**Test 8.6: Frame sequencer timing**
Run the APU for several frame periods. Verify:
- Length counters clock at 256 Hz (every 16384 clocks at 4194304 Hz)
- Envelopes clock at 64 Hz (every 4th length clock)
- Sweep clocks at 128 Hz (every other length clock, odd frames)

**Test 8.7: Register read (0xff26 status)**
Read 0xff26. Verify it returns oscillator enabled status in bits 0-3.

### Phase 9: MIDI Note Processing (`src/engine.rs`)

Port PAPUEngine's MIDI handling: note queues, note-to-frequency conversion, pitch bend.

**Test 9.1: Single note on/off**
Send MIDI note on (note 60, channel 1). Verify note is added to noteQueue1 and the correct APU registers are written (frequency, envelope trigger). Send note off. Verify release envelope is triggered.

**Test 9.2: Monophonic last-note priority**
Send notes 60, 64, 67 on channel 1 (no channel split). Verify only the last note (67) is playing. Release 67. Verify 64 resumes. Release 64. Verify 60 resumes.

**Test 9.3: MIDI note to Game Boy frequency**
Verify the frequency calculation: `freq = getMidiNoteInHertz(note + tune + fine/100)`, `period = ((4194304 / freq) - 65536) / -32`. Test several notes across the MIDI range.

**Test 9.4: Pitch bend**
Send pitch wheel message (value 12288, which is +2 semitones at half range). Verify frequency shifts accordingly. Verify pitch bend range is -2 to +2 semitones.

**Test 9.5: Channel split mode**
Enable channel_split. Send notes on MIDI channels 1-4. Verify each MIDI channel maps to a different oscillator (ch1→square1, ch2→square2, ch3→wave, ch4→noise).

**Test 9.6: All notes off**
Send all-notes-off message. Verify all note queues are cleared.

### Phase 10: Vibrato LFO (`src/engine.rs`)

Port the sine-wave vibrato LFO.

**Test 10.1: Vibrato disabled (amount = 0)**
Set vibrato amount to 0. Verify frequency is stable (no modulation) across a rendered block.

**Test 10.2: Vibrato basic operation**
Set vibrato rate=5 Hz, amount=50%. Render several frames. Verify frequency oscillates sinusoidally around the base frequency. The vibrato depth of 0.25 * amount/100 * 12 semitones should be visible in the period register writes.

**Test 10.3: Vibrato reset on new note**
Play a note, let vibrato accumulate phase, then play a new note. Verify vibrato phase resets to 0.

**Test 10.4: Vibrato does not reset on pitch bend**
Send a pitch bend while a note is held. Verify vibrato phase is NOT reset.

### Phase 11: Parameter Mapping and Register Cache (`src/engine.rs`)

Port the full parameter-to-register mapping including the register cache.

**Test 11.1: Pulse 1 full parameter set**
Configure all Pulse 1 parameters: duty=2, A=3, R=5, tune=+12, fine=+50, sweep=-3, shift=2, OL=on, OR=on. Render a note. Verify the exact register values written to the APU.

**Test 11.2: Pulse 2 full parameter set**
Same as 11.1 but for Pulse 2 (no sweep).

**Test 11.3: Wave channel parameters**
Configure wave: waveform=5, tune=-7, fine=-25, OL=on, OR=off. Verify wave RAM is loaded with waveform 5 and panning is left-only.

**Test 11.4: Noise channel parameters**
Configure noise: shift=8, step=1 (15-bit), ratio=3, A=0, R=4. Verify register values.

**Test 11.5: Global parameters**
Set output=5, treble=-30, bass=461. Verify treble_eq and bass_freq are applied to the buffer, and global volume register is written.

**Test 11.6: Register cache deduplication**
Write the same parameter values twice. Verify the second write does NOT result in APU register writes (cache hit). Then change a value. Verify it DOES write.

**Test 11.7: Waveform switching**
Change waveform parameter from 0 to 7. Verify wave RAM is rewritten: disable (0xff1A=0x00), write 16 bytes of wave data, re-enable (0xff1A=0x80).

### Phase 12: Audio Rendering Pipeline (`src/engine.rs`)

Port the full processBlock pipeline: per-sample MIDI event handling, runUntil, output conversion.

**Test 12.1: Silent output with no notes**
Render a block with no MIDI events. Verify output is silence (all zeros or near-zero after high-pass settling).

**Test 12.2: Single note render**
Play MIDI note 69 (A4) for an entire block (512 samples at 44100 Hz). Verify output contains a square wave at approximately 440 Hz.

**Test 12.3: Note on mid-block**
Place a note-on event at sample position 256 in a 512-sample block. Verify silence for the first ~256 samples, then signal appears.

**Test 12.4: Note off with release envelope**
Play a note, then send note-off mid-block. Verify the envelope release begins at the correct sample position.

**Test 12.5: Frame boundary handling**
The engine processes audio in 1024-clock chunks via `apu.end_frame(1024)`. Verify this works correctly when the output block size doesn't align to 1024 clocks.

**Test 12.6: Output level scaling**
Verify the raw int16 samples from the blip buffer are divided by 32768.0 to produce float output in [-1.0, 1.0] range.

**Test 12.7: Multiple MIDI events in one block**
Place note-on at sample 0, pitch bend at sample 100, note-off at sample 300 in a 512-sample block. Verify all events are processed at the correct sample positions.

### Phase 13: Polyphony (`src/engine.rs`)

Port multi-voice support.

**Test 13.1: Single voice mode**
Set voices=1. Play one note. Verify single PAPUEngine processes everything.

**Test 13.2: Two voices**
Set voices=2. Play two overlapping notes. Verify each note is assigned to a different engine instance and both contribute to the output.

**Test 13.3: Voice stealing**
Set voices=2. Play 3 notes. Verify the third note either steals a voice or is dropped (matching original behavior).

**Test 13.4: Voice allocation round-robin**
Verify findFreeVoice uses round-robin allocation (nextVoice counter wraps).

**Test 13.5: Voice per-channel in split mode**
Enable channel_split with voices=2. Send notes on channels 1 and 2. Verify correct voice-to-channel mapping.

### Phase 14: Gold Master Comparison Tests

End-to-end tests comparing Rust output against C++ reference renders.

**Test 14.1: Reference render — single square wave note**
C++ reference: configure default parameters, play MIDI note 60 for 1 second at 44100 Hz. Save output. Rust: same configuration. Compare using the metric established in Phase 0.

**Test 14.2: Reference render — all four channels simultaneously**
Channel split mode. Play notes on all 4 MIDI channels (square1=C4, square2=E4, wave=G4, noise). Render 2 seconds. Compare.

**Test 14.3: Reference render — envelope attack and release**
Play a note with A=3, R=5 for 0.5s, then release. Render the full attack-sustain-release cycle. Compare.

**Test 14.4: Reference render — vibrato**
Play a sustained note with vibrato rate=5, amount=50. Render 2 seconds. Compare frequency modulation pattern.

**Test 14.5: Reference render — pitch bend sequence**
Play a note, sweep pitch bend from center to +2 semitones over 1 second. Compare.

**Test 14.6: Reference render — waveform sweep**
Play wave channel, cycling through all 15 waveforms (switching every 0.5s). Compare.

**Test 14.7: Reference render — noise varieties**
Render noise with various shift/step/ratio combinations. Compare each configuration.

### Phase 15: VST Plugin Shell

Wire up the engine as a VST plugin using a Rust VST framework (e.g., nih-plug).

**Test 15.1: Plugin loads without crash**
Instantiate the plugin. Verify it reports correct channel configuration (stereo out), accepts MIDI, and exposes all parameters.

**Test 15.2: Parameter list matches original**
Verify all 40 parameters are exposed with correct names, ranges, and default values matching the original PAPU.

**Test 15.3: Offline render matches engine tests**
Process a buffer through the VST plugin interface. Verify output matches the direct engine render from Phase 12 tests.

### Phase 16: Musical Integration Tests

Port real musical content and verify it plays correctly.

**Test 16.1-16.15: RPG Chiptune MIDI files**
Port the 15 melodic RPG chiptune MIDI files from [OpenGameArt](https://opengameart.org/content/15-melodic-rpg-chiptunes) to play well on the original PAPU (may require channel remapping, velocity-to-volume mapping, etc.). Then use them as integration tests:

For each MIDI file:
1. Play through the original C++ PAPU with tuned parameters → reference render
2. Play through the Rust port with identical parameters → test render
3. Compare using the established metric from Phase 0
4. Store reference audio as content-addressed gitignored files

Specific files (to be assigned numbers after downloading and porting):
- Test 16.1: Simplest/shortest track (establish baseline)
- Test 16.2-16.10: Progressively more complex tracks
- Test 16.11-16.14: Tracks using all 4 channels simultaneously
- Test 16.15: Most complex track (final integration validation)

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
