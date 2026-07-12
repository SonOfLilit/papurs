// PAPURS — Game Boy APU Synth (Rust port of PAPU)
// Copyright (C) 2026  Roland Rabien (original C++ code)
// Copyright (C) 2026  Aur Saraf (Rust port)
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
// USA

//! C ABI for hosts that link papurs as a shared library (`.so`/`.dylib`/
//! `.wasm` side module), satisfying the LGPL relinking requirement.
//!
//! The surface is deliberately number-only (an opaque handle plus scalar
//! arguments) so a WebAssembly host can bridge every call through trivial
//! JS shims without sharing linear memory; the one buffer that crosses the
//! boundary (the rendered block) is exposed as a pointer into this
//! module's memory for the host to copy out.
//!
//! Contract:
//! - A handle is created by [`papurs_new`] and must only be passed to the
//!   other `papurs_*` functions, from one thread at a time, until
//!   [`papurs_free`].
//! - Real-time safety: `papurs_set_param` and `papurs_push_midi` never
//!   allocate. `papurs_render` allocates exactly as much as
//!   [`crate::engine::PapuProcessor::process_block`] does today.
//! - The pointer returned by `papurs_render` is valid until the next
//!   `papurs_render`/`papurs_reset`/`papurs_free` call on the same handle.
//!
//! Rust hosts that link papurs statically (development builds, tests) call
//! these same functions directly, so the dynamic and static builds share
//! one code path.

use crate::engine::{MidiEvent, MidiKind, PapuProcessor, Params};

/// Bumped on any breaking change to the function signatures or parameter
/// index layout below. Hosts should check it before using a replacement
/// library.
pub const PAPURS_ABI_VERSION: u32 = 1;

/// Parameter indices for [`papurs_set_param`]. Booleans are set with
/// value > 0.5, integer fields round to nearest.
pub mod param {
    pub const PULSE1_OL: u32 = 0;
    pub const PULSE1_OR: u32 = 1;
    pub const PULSE1_DUTY: u32 = 2;
    pub const PULSE1_A: u32 = 3;
    pub const PULSE1_R: u32 = 4;
    pub const PULSE1_TUNE: u32 = 5;
    pub const PULSE1_FINE: u32 = 6;
    pub const PULSE1_SWEEP: u32 = 7;
    pub const PULSE1_SHIFT: u32 = 8;
    pub const PULSE1_VIB_RATE: u32 = 9;
    pub const PULSE1_VIB_AMT: u32 = 10;
    pub const PULSE2_OL: u32 = 11;
    pub const PULSE2_OR: u32 = 12;
    pub const PULSE2_DUTY: u32 = 13;
    pub const PULSE2_A: u32 = 14;
    pub const PULSE2_R: u32 = 15;
    pub const PULSE2_TUNE: u32 = 16;
    pub const PULSE2_FINE: u32 = 17;
    pub const PULSE2_VIB_RATE: u32 = 18;
    pub const PULSE2_VIB_AMT: u32 = 19;
    pub const NOISE_OL: u32 = 20;
    pub const NOISE_OR: u32 = 21;
    pub const NOISE_A: u32 = 22;
    pub const NOISE_R: u32 = 23;
    pub const NOISE_SHIFT: u32 = 24;
    pub const NOISE_STEP: u32 = 25;
    pub const NOISE_RATIO: u32 = 26;
    pub const WAVE_OL: u32 = 27;
    pub const WAVE_OR: u32 = 28;
    pub const WAVE_INDEX: u32 = 29;
    pub const WAVE_TUNE: u32 = 30;
    pub const WAVE_FINE: u32 = 31;
    pub const WAVE_VIB_RATE: u32 = 32;
    pub const WAVE_VIB_AMT: u32 = 33;
    pub const CHANNEL_SPLIT: u32 = 34;
    pub const TREBLE: u32 = 35;
    pub const BASS: u32 = 36;
    pub const OUTPUT: u32 = 37;
    pub const FIX_SILENT_RETRIGGER: u32 = 38;
    pub const FIX_PERIOD_CLAMP: u32 = 39;
    pub const FIX_WAVE_VIBRATO_PERIOD: u32 = 40;
    pub const COUNT: u32 = 41;
}

/// MIDI kinds for [`papurs_push_midi`].
pub mod midi {
    /// `value` = MIDI note number.
    pub const NOTE_ON: u32 = 0;
    /// `value` = MIDI note number.
    pub const NOTE_OFF: u32 = 1;
    /// `value` = 14-bit bend, 0..=16383, 8192 = center.
    pub const PITCH_BEND: u32 = 2;
    /// `value` ignored.
    pub const ALL_NOTES_OFF: u32 = 3;
}

/// Staged MIDI events beyond this are dropped (never reallocate on the
/// audio thread). Generous: a 128-sample block carries a handful.
const MIDI_CAPACITY: usize = 1024;

struct Instance {
    processor: PapuProcessor,
    voices: usize,
    params: Params,
    midi: Vec<MidiEvent>,
    out: Vec<f32>,
}

/// Engine-neutral defaults matching upstream PAPU's parameter defaults.
/// The fix flags default to off = bit-exact upstream behavior.
fn default_params() -> Params {
    Params {
        output: 7,
        pulse1_ol: true,
        pulse1_or: true,
        pulse1_duty: 0,
        pulse1_a: 1,
        pulse1_r: 1,
        pulse1_tune: 0,
        pulse1_fine: 0,
        pulse1_sweep: 0,
        pulse1_shift: 0,
        pulse2_ol: false,
        pulse2_or: false,
        pulse2_duty: 0,
        pulse2_a: 1,
        pulse2_r: 1,
        pulse2_tune: 0,
        pulse2_fine: 0,
        wave_ol: false,
        wave_or: false,
        wave_tune: 0,
        wave_fine: 0,
        noise_ol: false,
        noise_or: false,
        noise_a: 1,
        noise_r: 1,
        noise_shift: 0,
        noise_step: 0,
        noise_ratio: 0,
        channel_split: false,
        pulse1_vib_rate: 5.0,
        pulse1_vib_amt: 0.0,
        pulse2_vib_rate: 5.0,
        pulse2_vib_amt: 0.0,
        wave_vib_rate: 5.0,
        wave_vib_amt: 0.0,
        wave_index: 0,
        treble: -30.0,
        bass: 461,
        fix_silent_retrigger: false,
        fix_period_clamp: false,
        fix_wave_vibrato_period: false,
    }
}

/// The ABI version this library was built with. Check against
/// [`PAPURS_ABI_VERSION`] before using a replacement library.
#[unsafe(no_mangle)]
pub extern "C" fn papurs_abi_version() -> u32 {
    PAPURS_ABI_VERSION
}

/// Create a synth with `voices` voices, prepared at `sample_rate`.
/// Returns an opaque handle (never 0 on success).
#[unsafe(no_mangle)]
pub extern "C" fn papurs_new(voices: usize, sample_rate: f64) -> usize {
    let mut processor = PapuProcessor::new(voices);
    processor.prepare(sample_rate);
    let instance = Box::new(Instance {
        processor,
        voices,
        params: default_params(),
        midi: Vec::with_capacity(MIDI_CAPACITY),
        out: Vec::new(),
    });
    Box::into_raw(instance) as usize
}

/// Destroy a handle. Passing 0 is a no-op.
///
/// # Safety
/// `handle` must come from [`papurs_new`] and not be used afterwards.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn papurs_free(handle: usize) {
    if handle == 0 {
        return;
    }
    drop(unsafe { Box::from_raw(handle as *mut Instance) });
}

/// Drop all playing notes and re-prepare at `sample_rate`. Parameters are
/// kept.
///
/// # Safety
/// `handle` must come from [`papurs_new`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn papurs_reset(handle: usize, sample_rate: f64) {
    if handle == 0 {
        return;
    }
    let instance = unsafe { &mut *(handle as *mut Instance) };
    instance.processor = PapuProcessor::new(instance.voices);
    instance.processor.prepare(sample_rate);
    instance.midi.clear();
}

/// Set one parameter (see [`param`] for the index layout). Out-of-range
/// indices are ignored. Takes effect from the next [`papurs_render`].
///
/// # Safety
/// `handle` must come from [`papurs_new`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn papurs_set_param(handle: usize, index: u32, value: f32) {
    if handle == 0 {
        return;
    }
    let p = &mut unsafe { &mut *(handle as *mut Instance) }.params;
    let on = value > 0.5;
    let int = value.round() as i32;
    match index {
        param::PULSE1_OL => p.pulse1_ol = on,
        param::PULSE1_OR => p.pulse1_or = on,
        param::PULSE1_DUTY => p.pulse1_duty = int,
        param::PULSE1_A => p.pulse1_a = int,
        param::PULSE1_R => p.pulse1_r = int,
        param::PULSE1_TUNE => p.pulse1_tune = int,
        param::PULSE1_FINE => p.pulse1_fine = int,
        param::PULSE1_SWEEP => p.pulse1_sweep = int,
        param::PULSE1_SHIFT => p.pulse1_shift = int,
        param::PULSE1_VIB_RATE => p.pulse1_vib_rate = value,
        param::PULSE1_VIB_AMT => p.pulse1_vib_amt = value,
        param::PULSE2_OL => p.pulse2_ol = on,
        param::PULSE2_OR => p.pulse2_or = on,
        param::PULSE2_DUTY => p.pulse2_duty = int,
        param::PULSE2_A => p.pulse2_a = int,
        param::PULSE2_R => p.pulse2_r = int,
        param::PULSE2_TUNE => p.pulse2_tune = int,
        param::PULSE2_FINE => p.pulse2_fine = int,
        param::PULSE2_VIB_RATE => p.pulse2_vib_rate = value,
        param::PULSE2_VIB_AMT => p.pulse2_vib_amt = value,
        param::NOISE_OL => p.noise_ol = on,
        param::NOISE_OR => p.noise_or = on,
        param::NOISE_A => p.noise_a = int,
        param::NOISE_R => p.noise_r = int,
        param::NOISE_SHIFT => p.noise_shift = int,
        param::NOISE_STEP => p.noise_step = int,
        param::NOISE_RATIO => p.noise_ratio = int,
        param::WAVE_OL => p.wave_ol = on,
        param::WAVE_OR => p.wave_or = on,
        param::WAVE_INDEX => p.wave_index = int.clamp(0, 14) as u8,
        param::WAVE_TUNE => p.wave_tune = int,
        param::WAVE_FINE => p.wave_fine = int,
        param::WAVE_VIB_RATE => p.wave_vib_rate = value,
        param::WAVE_VIB_AMT => p.wave_vib_amt = value,
        param::CHANNEL_SPLIT => p.channel_split = on,
        param::TREBLE => p.treble = value as f64,
        param::BASS => p.bass = int,
        param::OUTPUT => p.output = int,
        param::FIX_SILENT_RETRIGGER => p.fix_silent_retrigger = on,
        param::FIX_PERIOD_CLAMP => p.fix_period_clamp = on,
        param::FIX_WAVE_VIBRATO_PERIOD => p.fix_wave_vibrato_period = on,
        _ => {}
    }
}

/// Stage one MIDI event (see [`midi`] for kinds) for the next
/// [`papurs_render`]. `pos` is the sample offset within that block,
/// `channel` is the 1-based MIDI channel. Unknown kinds are ignored;
/// events beyond the staging capacity are dropped.
///
/// # Safety
/// `handle` must come from [`papurs_new`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn papurs_push_midi(
    handle: usize,
    pos: i32,
    channel: u8,
    kind: u32,
    value: i32,
) {
    if handle == 0 {
        return;
    }
    let instance = unsafe { &mut *(handle as *mut Instance) };
    if instance.midi.len() == MIDI_CAPACITY {
        return;
    }
    let kind = match kind {
        midi::NOTE_ON => MidiKind::NoteOn(value as u8),
        midi::NOTE_OFF => MidiKind::NoteOff(value as u8),
        midi::PITCH_BEND => MidiKind::PitchBend(value),
        midi::ALL_NOTES_OFF => MidiKind::AllNotesOff,
        _ => return,
    };
    instance.midi.push(MidiEvent { pos, channel, kind });
}

/// Render one block of `block_size` frames using the staged MIDI events
/// and current parameters, then clear the staged events. Returns a pointer
/// to `2 * block_size` f32 samples laid out planar:
/// `[L0..L(n-1), R0..R(n-1)]`, valid until the next
/// `papurs_render`/`papurs_reset`/`papurs_free` on this handle.
///
/// # Safety
/// `handle` must come from [`papurs_new`].
#[unsafe(no_mangle)]
pub unsafe extern "C" fn papurs_render(handle: usize, block_size: u32) -> *const f32 {
    if handle == 0 {
        return core::ptr::null();
    }
    let instance = unsafe { &mut *(handle as *mut Instance) };
    instance.out =
        instance
            .processor
            .process_block(block_size as i32, &instance.params, &instance.midi);
    instance.midi.clear();
    instance.out.as_ptr()
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{expect, Expect};

    const BLOCK: u32 = 128;

    /// Drive the C ABI exactly as a dynamic-linking host would: create,
    /// set params, stage MIDI per block, render, copy out. Returns the
    /// concatenated left channel.
    fn render_via_ffi(
        voices: usize,
        params: &[(u32, f32)],
        events: &[(usize, u32, i32)],
        num_blocks: usize,
    ) -> Vec<f32> {
        let handle = papurs_new(voices, 48000.0);
        assert_ne!(handle, 0);
        let mut left = Vec::new();
        unsafe {
            for &(index, value) in params {
                papurs_set_param(handle, index, value);
            }
            for block in 0..num_blocks {
                for &(b, kind, value) in events {
                    if b == block {
                        papurs_push_midi(handle, 0, 1, kind, value);
                    }
                }
                let ptr = papurs_render(handle, BLOCK);
                let out = core::slice::from_raw_parts(ptr, 2 * BLOCK as usize);
                left.extend_from_slice(&out[..BLOCK as usize]);
            }
            papurs_free(handle);
        }
        left
    }

    /// The same scenario through the plain Rust API. The FFI must be a
    /// bit-exact veneer over this.
    fn render_via_rust(
        voices: usize,
        params: &Params,
        events: &[(usize, u32, i32)],
        num_blocks: usize,
    ) -> Vec<f32> {
        let mut processor = PapuProcessor::new(voices);
        processor.prepare(48000.0);
        let mut left = Vec::new();
        for block in 0..num_blocks {
            let midi: Vec<MidiEvent> = events
                .iter()
                .filter(|&&(b, _, _)| b == block)
                .map(|&(_, kind, value)| MidiEvent {
                    pos: 0,
                    channel: 1,
                    kind: match kind {
                        midi::NOTE_ON => MidiKind::NoteOn(value as u8),
                        midi::NOTE_OFF => MidiKind::NoteOff(value as u8),
                        midi::PITCH_BEND => MidiKind::PitchBend(value),
                        _ => MidiKind::AllNotesOff,
                    },
                })
                .collect();
            let out = processor.process_block(BLOCK as i32, params, &midi);
            left.extend_from_slice(&out[..BLOCK as usize]);
        }
        left
    }

    /// Peak amplitude per window of blocks — traces the envelope.
    fn format_peaks(samples: &[f32], window_blocks: usize) -> String {
        samples
            .chunks(window_blocks * BLOCK as usize)
            .enumerate()
            .map(|(w, chunk)| {
                let peak = chunk.iter().fold(0.0f32, |m, &s| m.max(s.abs()));
                let start = w * window_blocks;
                format!(
                    "blocks {:>3}-{:>3}: peak={peak:.4}",
                    start,
                    start + window_blocks - 1
                )
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// FFI vs plain-Rust equivalence plus the envelope snapshot: params set
    /// through `papurs_set_param` must build exactly the `Params` value a
    /// Rust host would, and render must be bit-identical.
    fn check_equivalence(
        voices: usize,
        ffi_params: &[(u32, f32)],
        rust_params: &Params,
        events: &[(usize, u32, i32)],
        num_blocks: usize,
        expect: Expect,
    ) {
        let via_ffi = render_via_ffi(voices, ffi_params, events, num_blocks);
        let via_rust = render_via_rust(voices, rust_params, events, num_blocks);
        assert_eq!(via_ffi, via_rust, "FFI render diverged from Rust render");
        expect.assert_eq(&format_peaks(&via_ffi, 16));
    }

    #[test]
    fn abi_version() {
        assert_eq!(papurs_abi_version(), PAPURS_ABI_VERSION);
    }

    #[test]
    fn null_handle_is_safe() {
        unsafe {
            papurs_free(0);
            papurs_reset(0, 48000.0);
            papurs_set_param(0, param::OUTPUT, 7.0);
            papurs_push_midi(0, 0, 1, midi::NOTE_ON, 60);
            assert!(papurs_render(0, BLOCK).is_null());
        }
    }

    /// Defaults (pulse 1 on): a full note lifecycle — attack ramp while
    /// gated, release decay to silence after NoteOff.
    #[test]
    fn default_note_lifecycle_matches_rust() {
        check_equivalence(
            4,
            &[],
            &default_params(),
            &[(0, midi::NOTE_ON, 60), (128, midi::NOTE_OFF, 60)],
            256,
            expect![[r#"
                blocks   0- 15: peak=0.0324
                blocks  16- 31: peak=0.0815
                blocks  32- 47: peak=0.1293
                blocks  48- 63: peak=0.1634
                blocks  64- 79: peak=0.2091
                blocks  80- 95: peak=0.2432
                blocks  96-111: peak=0.2451
                blocks 112-127: peak=0.2451
                blocks 128-143: peak=0.2446
                blocks 144-159: peak=0.2104
                blocks 160-175: peak=0.1634
                blocks 176-191: peak=0.1126
                blocks 192-207: peak=0.0647
                blocks 208-223: peak=0.0326
                blocks 224-239: peak=0.0000
                blocks 240-255: peak=0.0000"#]],
        );
    }

    /// Every parameter index round-trips: a kick-style preset (sweep,
    /// envelope, tune) set entirely through the FFI matches the same
    /// `Params` built in Rust, including the fix flags.
    #[test]
    fn kick_preset_with_fix_flags_matches_rust() {
        let ffi_params: &[(u32, f32)] = &[
            (param::PULSE1_DUTY, 2.0),
            (param::PULSE1_A, 0.0),
            (param::PULSE1_R, 1.0),
            (param::PULSE1_TUNE, 31.0),
            (param::PULSE1_SWEEP, -1.0),
            (param::PULSE1_SHIFT, 2.0),
            (param::FIX_SILENT_RETRIGGER, 1.0),
            (param::FIX_PERIOD_CLAMP, 1.0),
            (param::FIX_WAVE_VIBRATO_PERIOD, 1.0),
        ];
        let rust_params = Params {
            pulse1_duty: 2,
            pulse1_a: 0,
            pulse1_r: 1,
            pulse1_tune: 31,
            pulse1_sweep: -1,
            pulse1_shift: 2,
            fix_silent_retrigger: true,
            fix_period_clamp: true,
            fix_wave_vibrato_period: true,
            ..default_params()
        };
        check_equivalence(
            4,
            ffi_params,
            &rust_params,
            &[(0, midi::NOTE_ON, 36), (8, midi::NOTE_OFF, 36)],
            128,
            expect![[r#"
                blocks   0- 15: peak=0.2446
                blocks  16- 31: peak=0.2125
                blocks  32- 47: peak=0.1611
                blocks  48- 63: peak=0.1289
                blocks  64- 79: peak=0.0809
                blocks  80- 95: peak=0.0386
                blocks  96-111: peak=0.0000
                blocks 112-127: peak=0.0000"#]],
        );
    }

    /// Wave + noise channels through the FFI (indices 20-33), single
    /// voice, with a pitch bend mid-note.
    #[test]
    fn wave_noise_and_pitch_bend_matches_rust() {
        let ffi_params: &[(u32, f32)] = &[
            (param::PULSE1_OL, 0.0),
            (param::PULSE1_OR, 0.0),
            (param::WAVE_OL, 1.0),
            (param::WAVE_OR, 1.0),
            (param::WAVE_INDEX, 2.0),
            (param::NOISE_OL, 1.0),
            (param::NOISE_OR, 1.0),
            (param::NOISE_SHIFT, 8.0),
        ];
        let rust_params = Params {
            pulse1_ol: false,
            pulse1_or: false,
            wave_ol: true,
            wave_or: true,
            wave_index: 2,
            noise_ol: true,
            noise_or: true,
            noise_shift: 8,
            ..default_params()
        };
        check_equivalence(
            1,
            ffi_params,
            &rust_params,
            &[
                (0, midi::NOTE_ON, 60),
                (32, midi::PITCH_BEND, 12000),
                (64, midi::NOTE_OFF, 60),
            ],
            128,
            expect![[r#"
                blocks   0- 15: peak=0.0907
                blocks  16- 31: peak=0.1590
                blocks  32- 47: peak=0.1932
                blocks  48- 63: peak=0.2460
                blocks  64- 79: peak=0.2566
                blocks  80- 95: peak=0.1507
                blocks  96-111: peak=0.1029
                blocks 112-127: peak=0.0513"#]],
        );
    }

    /// `papurs_reset` keeps parameters but drops sounding notes: after a
    /// reset mid-note the output returns to silence, and a new note under
    /// the same (kept) params sounds again.
    #[test]
    fn reset_drops_notes_and_keeps_params() {
        let handle = papurs_new(4, 48000.0);
        unsafe {
            papurs_set_param(handle, param::PULSE1_DUTY, 2.0);
            papurs_push_midi(handle, 0, 1, midi::NOTE_ON, 60);
            let mut before = Vec::new();
            for _ in 0..64 {
                let ptr = papurs_render(handle, BLOCK);
                before
                    .extend_from_slice(core::slice::from_raw_parts(ptr, BLOCK as usize));
            }
            papurs_reset(handle, 48000.0);
            let mut after = Vec::new();
            for _ in 0..64 {
                let ptr = papurs_render(handle, BLOCK);
                after.extend_from_slice(core::slice::from_raw_parts(ptr, BLOCK as usize));
            }
            papurs_push_midi(handle, 0, 1, midi::NOTE_ON, 60);
            let mut renote = Vec::new();
            for _ in 0..64 {
                let ptr = papurs_render(handle, BLOCK);
                renote
                    .extend_from_slice(core::slice::from_raw_parts(ptr, BLOCK as usize));
            }
            papurs_free(handle);

            let peak = |s: &[f32]| s.iter().fold(0.0f32, |m, &x| m.max(x.abs()));
            expect![[r#"
                before reset: peak=0.1764
                after reset:  peak=0.0000
                new note:     peak=0.1630"#]].assert_eq(&format!(
                "before reset: peak={:.4}\nafter reset:  peak={:.4}\nnew note:     peak={:.4}",
                peak(&before),
                peak(&after),
                peak(&renote)
            ));
        }
    }

    /// Out-of-range param indices and unknown MIDI kinds are ignored:
    /// a render preceded by both is bit-identical to one with neither.
    #[test]
    fn out_of_range_param_and_unknown_midi_are_ignored() {
        let baseline = render_via_ffi(4, &[], &[(0, midi::NOTE_ON, 60)], 32);
        let handle = papurs_new(4, 48000.0);
        let mut left = Vec::new();
        unsafe {
            papurs_set_param(handle, param::COUNT, 3.0);
            papurs_set_param(handle, 9999, 1.0);
            for block in 0..32 {
                if block == 0 {
                    papurs_push_midi(handle, 0, 1, 99, 60);
                    papurs_push_midi(handle, 0, 1, midi::NOTE_ON, 60);
                }
                let ptr = papurs_render(handle, BLOCK);
                let out = core::slice::from_raw_parts(ptr, 2 * BLOCK as usize);
                left.extend_from_slice(&out[..BLOCK as usize]);
            }
            papurs_free(handle);
        }
        assert_eq!(left, baseline);
    }

    /// MIDI staging overflow: the event at capacity is dropped, not
    /// reallocated — the queue length stays pinned at MIDI_CAPACITY.
    #[test]
    fn midi_overflow_drops_events() {
        let handle = papurs_new(1, 48000.0);
        unsafe {
            for i in 0..(MIDI_CAPACITY + 100) {
                papurs_push_midi(handle, 0, 1, midi::NOTE_ON, (i % 128) as i32);
            }
            let instance = &mut *(handle as *mut Instance);
            assert_eq!(instance.midi.len(), MIDI_CAPACITY);
            assert_eq!(instance.midi.capacity(), MIDI_CAPACITY);
            papurs_free(handle);
        }
    }
}
