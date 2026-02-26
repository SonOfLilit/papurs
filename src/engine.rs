// PAPU engine: MIDI-to-APU translation (Phase 9+).
// Port of PAPUEngine from PluginProcessor.cpp (no JUCE).

use std::collections::HashMap;

use crate::apu::{sbuf_split, GbApu, GbAddr, GbTime};
use crate::blip::{BlipEq, StereoBuffer};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const CLOCK_RATE: i64 = 4_194_304;
const FRAME_SIZE: i64 = 1_024;

/// Wave preset samples (from PluginProcessor.h, Pokemon Red/Crystal source).
pub const WAVE_SAMPLES: [[u8; 32]; 15] = [
    [ 0, 2, 4, 6, 8,10,12,14,15,15,15,14,14,13,13,12,12,11,10, 9, 8, 7, 6, 5, 4, 4, 3, 3, 2, 2, 1, 1],
    [ 0, 2, 4, 6, 8,10,12,14,14,15,15,15,15,14,14,14,13,13,12,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 2, 1, 1],
    [ 1, 3, 6, 9,11,13,14,14,14,14,15,15,15,15,14,13,13,14,15,15,15,15,14,14,14,14,13,11, 9, 6, 3, 1],
    [ 0, 2, 4, 6, 8,10,12,13,14,15,15,14,13,14,15,15,14,14,13,12,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
    [ 0, 1, 2, 3, 4, 5, 6, 7, 8,10,12,13,14,14,15, 7, 7,15,14,14,13,12,10, 8, 7, 6, 5, 4, 3, 2, 1, 0],
    [ 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 3, 3, 2, 2, 1, 1,15,15,14,14,12,12,10,10, 8, 8,10,10,12,12,14,14],
    [ 0, 2, 4, 6, 8,10,12,14,12,11,10, 9, 8, 7, 6, 5,15,15,15,14,14,13,13,12, 4, 4, 3, 3, 2, 2, 1, 1],
    [12, 0,10, 9, 8, 7,15, 5,15,15,15,14,14,13,13,12, 4, 4, 3, 3, 2, 2,15, 1, 0, 2, 4, 6, 8,10,12,14],
    [ 4, 4, 3, 3, 2, 2, 1,15, 0, 0, 4, 6, 8,10,12,14,15, 8,15,14,14,13,13,12,12,11,10, 9, 8, 7, 6, 5],
    [ 1, 1, 0, 0, 0, 0, 0, 8, 0, 0, 1, 3, 5, 7, 9,10,11, 4,11,10,10, 9, 9, 8, 8, 7, 6, 5, 4, 3, 2, 1],
    [ 7, 9,11,13,15,15,15,15,15,15,15,15,15,13,11, 9, 7, 5, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 5],
    [ 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7, 8, 8, 9, 9,10,10,11,11,12,12,13,13,14,14,15,15],
    [ 4, 6, 8,10,12,12,12,12,12,12,12,12,12,10, 8, 6, 4, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2],
    [ 7,10,13,15,15,15,13,10, 7, 4, 1, 0, 0, 0, 1, 4, 7,10,13,15,15,15,13,10, 7, 4, 1, 0, 0, 0, 1, 4],
    [14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
];

// ---------------------------------------------------------------------------
// Sine LFO (mirrors gin::LFO with waveShape=sine, offset=0, fade=0, delay=0)
// ---------------------------------------------------------------------------

struct LfoState {
    sample_rate: f64,
    frequency:   f64,
    depth:       f64,
    phase:       f64,   // 0..1, wraps each cycle
    output:      f64,   // last computed output
}

impl Default for LfoState {
    fn default() -> Self {
        LfoState { sample_rate: 44100.0, frequency: 5.0, depth: 0.0, phase: 0.0, output: 0.0 }
    }
}

impl LfoState {
    fn set_sample_rate(&mut self, sr: f64) { self.sample_rate = sr; }
    fn set_params(&mut self, frequency: f64, depth: f64) {
        self.frequency = frequency;
        self.depth     = depth;
    }
    fn reset(&mut self) { self.phase = 0.0; self.output = 0.0; }
    fn process(&mut self, n: i32) {
        if n > 0 && self.sample_rate > 0.0 {
            self.phase += self.frequency * n as f64 / self.sample_rate;
            self.phase -= self.phase.floor();
        }
        self.output = self.depth * (std::f64::consts::TAU * self.phase).sin();
    }
    fn get_output(&self) -> f64 { self.output }
}

// ---------------------------------------------------------------------------
// Params — per-block configuration (mirrors plugin parameters)
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Params {
    pub output:        i32,  // 0-7
    pub pulse1_ol:     bool,
    pub pulse1_or:     bool,
    pub pulse1_duty:   i32,  // 0-3
    pub pulse1_a:      i32,  // 0-7 attack
    pub pulse1_r:      i32,  // 0-7 release
    pub pulse1_tune:   i32,  // -48..48 semitones
    pub pulse1_fine:   i32,  // -100..100 cents
    pub pulse1_sweep:  i32,  // -7..7
    pub pulse1_shift:  i32,  // 0-7
    pub pulse2_ol:     bool,
    pub pulse2_or:     bool,
    pub pulse2_duty:   i32,
    pub pulse2_a:      i32,
    pub pulse2_r:      i32,
    pub pulse2_tune:   i32,
    pub pulse2_fine:   i32,
    pub wave_ol:       bool,
    pub wave_or:       bool,
    pub wave_tune:     i32,
    pub wave_fine:     i32,
    pub noise_ol:      bool,
    pub noise_or:      bool,
    pub noise_a:       i32,
    pub noise_r:       i32,
    pub noise_shift:      i32,  // 0-13
    pub noise_step:       i32,  // 0=15-bit, 1=7-bit LFSR
    pub noise_ratio:      i32,  // 0-7
    pub channel_split:    bool,
    // Vibrato LFO (per-channel). amt 0..100 → depth = 0.25 * amt / 100.
    pub pulse1_vib_rate:  f32,  // Hz
    pub pulse1_vib_amt:   f32,  // 0..100
    pub pulse2_vib_rate:  f32,
    pub pulse2_vib_amt:   f32,
    pub wave_vib_rate:    f32,
    pub wave_vib_amt:     f32,
    // Global EQ and wave preset
    pub wave_index:       u8,   // 0-14, wave RAM preset
    pub treble:           f64,  // treble EQ in dB (e.g. -20.0)
    pub bass:             i32,  // bass high-pass frequency in Hz (e.g. 461)
}

impl Default for Params {
    fn default() -> Self {
        Params {
            output: 7,
            pulse1_ol: true, pulse1_or: true,
            pulse1_duty: 0, pulse1_a: 1, pulse1_r: 1,
            pulse1_tune: 0, pulse1_fine: 0,
            pulse1_sweep: 0, pulse1_shift: 0,
            pulse2_ol: false, pulse2_or: false,
            pulse2_duty: 0, pulse2_a: 1, pulse2_r: 1,
            pulse2_tune: 0, pulse2_fine: 0,
            wave_ol: false, wave_or: false,
            wave_tune: 0, wave_fine: 0,
            noise_ol: false, noise_or: false,
            noise_a: 1, noise_r: 1,
            noise_shift: 0, noise_step: 0, noise_ratio: 0,
            channel_split: false,
            pulse1_vib_rate: 5.0, pulse1_vib_amt: 0.0,
            pulse2_vib_rate: 5.0, pulse2_vib_amt: 0.0,
            wave_vib_rate:   5.0, wave_vib_amt:   0.0,
            wave_index: 0, treble: -20.0, bass: 461,
        }
    }
}

// ---------------------------------------------------------------------------
// MIDI events
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum MidiKind {
    NoteOn(u8),
    NoteOff(u8),
    PitchBend(i32),
    AllNotesOff,
}

#[derive(Clone, Debug)]
pub struct MidiEvent {
    pub pos:     i32,  // sample position within block
    pub channel: u8,   // 1-based MIDI channel
    pub kind:    MidiKind,
}

// ---------------------------------------------------------------------------
// PapuEngine
// ---------------------------------------------------------------------------

pub struct PapuEngine {
    apu:           GbApu,
    sbuf:          StereoBuffer,
    time:          GbTime,
    reg_cache:     HashMap<u32, u8>,
    note_queues:   [Vec<u8>; 4],
    last_notes:    [i32; 4],      // -1 = no note
    pitch_bend:    f64,           // semitones, ±2; stored as f64 of f32 value
    freq:          [f32; 3],      // last computed freq for ch1/2/3 (for release path)
    channel_split: bool,
    wave_index:    u8,
    lfos:          [LfoState; 3], // vibrato LFO for ch1/2/3
    vib_notes:     [i32; 3],      // last MIDI note used for vibrato freq calculation
    current_treble: f64,          // cached treble EQ to skip redundant rebuilds
    current_bass:   i32,          // cached bass freq to skip redundant writes
}

/// Convert a MIDI note number (possibly fractional) to Hertz.
/// Matches JUCE `gin::getMidiNoteInHertz(double)`.
fn midi_hz(note: f64) -> f64 {
    440.0 * 2.0_f64.powf((note - 69.0) / 12.0)
}

/// Square/pulse frequency register period from freq (f32 arithmetic, matches C++).
/// The `as i32 as u16` chain matches C++ `uint16_t(float)` which converts via
/// int32 truncation then unsigned wrapping, rather than Rust's saturating `as u16`.
fn sq_period(freq_f32: f32) -> u16 {
    ((4_194_304.0_f32 / freq_f32 - 65_536.0_f32) / -32.0_f32) as i32 as u16
}

/// Wave channel frequency register period (f32 arithmetic, matches C++).
fn wave_period(freq_f32: f32) -> u16 {
    (-(65_536.0_f32 - 2048.0_f32 * freq_f32) / freq_f32) as i32 as u16
}

impl PapuEngine {
    pub fn new() -> Self {
        PapuEngine {
            apu:           GbApu::new(),
            sbuf:          StereoBuffer::new(),
            time:          0,
            reg_cache:     HashMap::new(),
            note_queues:   [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            last_notes:    [-1; 4],
            pitch_bend:    0.0,
            freq:          [0.0; 3],
            channel_split: false,
            wave_index:    0,
            lfos:           Default::default(),
            vib_notes:      [0; 3],
            current_treble: -20.0,
            current_bass:   461,
        }
    }

    /// Set up APU/buffer at the given sample rate. Matches PAPUEngine::prepareToPlay.
    pub fn prepare(&mut self, sample_rate: f64) {
        let eq = BlipEq::new(-20.0);
        self.apu.treble_eq(&eq);
        self.sbuf.bass_freq(461);
        self.sbuf.clock_rate(CLOCK_RATE);
        self.sbuf.set_sample_rate(sample_rate as i64);
        for lfo in &mut self.lfos { lfo.set_sample_rate(sample_rate); }

        // Load wave preset 0
        self.write_reg(0xff1A, 0x00, true);
        let ws = WAVE_SAMPLES[0];
        for s in 0..16_u32 {
            let high = ws[(s * 2) as usize];
            let low  = ws[(s * 2 + 1) as usize];
            self.write_reg(0xff30 + s, low | (high << 4), true);
        }
        self.write_reg(0xff1A, 0x80, true);
        self.write_reg(0xff26, 0x8f, true);
    }

    fn clock_(&mut self) -> GbTime {
        self.time += 4;
        self.time
    }

    fn write_reg(&mut self, addr: u32, value: u8, force: bool) {
        let cached = self.reg_cache.get(&addr).copied();
        if force || cached != Some(value) {
            self.reg_cache.insert(addr, value);
            let t = self.clock_();
            let (center, left, right) = sbuf_split(&mut self.sbuf);
            self.apu.write_register(t, addr as GbAddr, value, center, left, right);
        }
    }

    /// Mirror C++ runOscs: translate current notes + params into APU register writes.
    fn run_oscs(&mut self, cur_notes: [i32; 4], triggers: [bool; 4], p: &Params) {
        // --- Ch1: Square 1 ---
        if cur_notes[0] != -1 {
            self.vib_notes[0] = cur_notes[0];
            let sweep_abs = p.pulse1_sweep.unsigned_abs() as u8;
            let neg: u8   = if p.pulse1_sweep < 0 { 1 } else { 0 };
            self.write_reg(0xff10, (sweep_abs << 4) | (neg << 3) | p.pulse1_shift as u8, triggers[0]);
            self.write_reg(0xff11, (p.pulse1_duty as u8) << 6, triggers[0]);
            let fine_f32  = p.pulse1_fine as f32 / 100.0_f32;
            let note_f64  = cur_notes[0] as f64 + self.pitch_bend + p.pulse1_tune as f64 + fine_f32 as f64;
            let f1        = midi_hz(note_f64) as f32;
            self.freq[0]  = f1;
            let period1   = sq_period(f1);
            self.write_reg(0xff13, (period1 & 0xff) as u8, triggers[0]);
            let a1        = p.pulse1_a as u8;
            let env1: u8  = if a1 != 0 { 0x00 | (1 << 3) | a1 } else { 0xf0 };
            self.write_reg(0xff12, env1, triggers[0]);
            self.write_reg(0xff14,
                (if triggers[0] { 0x80u8 } else { 0x00u8 }) | ((period1 >> 8) as u8 & 0x07),
                triggers[0]);
        } else if triggers[0] {
            let r1 = p.pulse1_r as u8;
            let a1 = p.pulse1_a as u8;
            if a1 == 0 && r1 != 0 {
                let period1 = sq_period(self.freq[0]);
                self.write_reg(0xff13, (period1 & 0xff) as u8, triggers[0]);
                self.write_reg(0xff12, if r1 != 0 { 0xf0 | r1 } else { 0 }, triggers[0]);
                self.write_reg(0xff14,
                    (if triggers[0] { 0x80u8 } else { 0x00u8 }) | ((period1 >> 8) as u8 & 0x07),
                    triggers[0]);
            } else {
                self.write_reg(0xff12, if r1 != 0 { 0xf0 | r1 } else { 0 }, triggers[0]);
            }
        }

        // --- Ch2: Square 2 ---
        if cur_notes[1] != -1 {
            self.vib_notes[1] = cur_notes[1];
            self.write_reg(0xff16, (p.pulse2_duty as u8) << 6, triggers[1]);
            let fine_f32 = p.pulse2_fine as f32 / 100.0_f32;
            let note_f64 = cur_notes[1] as f64 + self.pitch_bend + p.pulse2_tune as f64 + fine_f32 as f64;
            let f2       = midi_hz(note_f64) as f32;
            self.freq[1] = f2;
            let period2  = sq_period(f2);
            self.write_reg(0xff18, (period2 & 0xff) as u8, triggers[1]);
            let a2       = p.pulse2_a as u8;
            let env2: u8 = if a2 != 0 { 0x00 | (1 << 3) | a2 } else { 0xf0 };
            self.write_reg(0xff17, env2, triggers[1]);
            self.write_reg(0xff19,
                (if triggers[1] { 0x80u8 } else { 0x00u8 }) | ((period2 >> 8) as u8 & 0x07),
                triggers[1]);
        } else if triggers[1] {
            let r2 = p.pulse2_r as u8;
            let a2 = p.pulse2_a as u8;
            if a2 == 0 && r2 != 0 {
                let period2 = sq_period(self.freq[1]);
                self.write_reg(0xff18, (period2 & 0xff) as u8, triggers[1]);
                self.write_reg(0xff17, if r2 != 0 { 0xf0 | r2 } else { 0 }, triggers[1]);
                self.write_reg(0xff19,
                    (if triggers[1] { 0x80u8 } else { 0x00u8 }) | ((period2 >> 8) as u8 & 0x07),
                    triggers[1]);
            } else {
                self.write_reg(0xff17, if r2 != 0 { 0xf0 | r2 } else { 0 }, triggers[1]);
            }
        }

        // --- Ch3: Wave ---
        if cur_notes[2] != -1 {
            self.vib_notes[2] = cur_notes[2];
            self.apu.reset_stop_wave();
            let fine_f32 = p.wave_fine as f32 / 100.0_f32;
            let note_f64 = cur_notes[2] as f64 + self.pitch_bend + p.wave_tune as f64 + fine_f32 as f64;
            let f3       = midi_hz(note_f64) as f32;
            self.freq[2] = f3;
            let period3  = wave_period(f3);
            self.write_reg(0xff1D, (period3 & 0xff) as u8, triggers[2]);
            self.write_reg(0xff1C, 0x20, triggers[2]);
            self.write_reg(0xff1E,
                (if triggers[2] { 0x80u8 } else { 0x00u8 }) | ((period3 >> 8) as u8 & 0x07),
                triggers[2]);
        } else if triggers[2] {
            self.apu.stop_wave();
        }

        // --- Ch4: Noise ---
        if cur_notes[3] != -1 {
            let an       = p.noise_a as u8;
            let env_n: u8 = if an != 0 { 0x00 | (1 << 3) | an } else { 0xf0 };
            self.write_reg(0xff21, env_n, triggers[3]);
            let noise_reg = ((p.noise_shift as u8) << 4) | ((p.noise_step as u8) << 3) | (p.noise_ratio as u8);
            self.write_reg(0xff22, noise_reg, triggers[3]);
            self.write_reg(0xff23, if triggers[3] { 0x80 } else { 0x00 }, triggers[3]);
        } else if triggers[3] {
            let rn = p.noise_r as u8;
            let an = p.noise_a as u8;
            if an == 0 && rn != 0 {
                self.write_reg(0xff21, if rn != 0 { 0xf0 | rn } else { 0 }, triggers[3]);
                self.write_reg(0xff23, 0x80, triggers[3]);
            } else {
                self.write_reg(0xff21, if rn != 0 { 0xf0 | rn } else { 0 }, triggers[3]);
            }
        }
    }

    /// Advance LFO and update freq registers. Mirrors C++ PAPUEngine::runVibrato.
    fn run_vibrato(&mut self, todo: i32, p: &Params) {
        for lfo in &mut self.lfos { lfo.process(todo); }

        let fine1 = p.pulse1_fine as f32 / 100.0_f32;
        let note1 = self.vib_notes[0] as f64 + self.pitch_bend
            + p.pulse1_tune as f64 + fine1 as f64
            + self.lfos[0].get_output() * 12.0;
        let f1      = midi_hz(note1) as f32;
        let period1 = sq_period(f1);
        // Use entry().or_insert(0) to match C++ std::map::operator[] which inserts
        // default 0 when key is missing. This affects later writeReg skip logic.
        let trig1   = *self.reg_cache.entry(0xff14u32).or_insert(0) & 0x80 != 0;
        self.write_reg(0xff13, (period1 & 0xff) as u8, false);
        self.write_reg(0xff14, (if trig1 { 0x80u8 } else { 0 }) | ((period1 >> 8) as u8 & 0x07), false);

        let fine2 = p.pulse2_fine as f32 / 100.0_f32;
        let note2 = self.vib_notes[1] as f64 + self.pitch_bend
            + p.pulse2_tune as f64 + fine2 as f64
            + self.lfos[1].get_output() * 12.0;
        let f2      = midi_hz(note2) as f32;
        let period2 = sq_period(f2);
        let trig2   = *self.reg_cache.entry(0xff19u32).or_insert(0) & 0x80 != 0;
        self.write_reg(0xff18, (period2 & 0xff) as u8, false);
        self.write_reg(0xff19, (if trig2 { 0x80u8 } else { 0 }) | ((period2 >> 8) as u8 & 0x07), false);

        let fine3 = p.wave_fine as f32 / 100.0_f32;
        let note3 = self.vib_notes[2] as f64 + self.pitch_bend
            + p.wave_tune as f64 + fine3 as f64
            + self.lfos[2].get_output() * 12.0;
        let f3      = midi_hz(note3) as f32;
        // C++ runVibrato uses sq_period here (bug in original), not wave_period.
        let period3 = sq_period(f3);
        let trig3   = *self.reg_cache.entry(0xff1eu32).or_insert(0) & 0x80 != 0;
        self.write_reg(0xff1D, (period3 & 0xff) as u8, false);
        self.write_reg(0xff1E, (if trig3 { 0x80u8 } else { 0 }) | ((period3 >> 8) as u8 & 0x07), false);
    }

    /// Render stereo pairs from `*done` up to `pos`, appending i16 pairs to `out`.
    /// Mirrors C++ PAPUEngine::runUntil: calls runVibrato first, then renders.
    fn run_until(&mut self, done: &mut i32, out: &mut Vec<i16>, pos: i32, p: &Params) {
        let todo = pos - *done;
        self.run_vibrato(todo, p);
        let mut todo = todo;
        while todo > 0 {
            let avail = self.sbuf.samples_avail() as i32;
            if avail > 0 {
                let want  = todo.min(512).min(avail) as usize;
                let start = out.len();
                out.resize(start + want * 2, 0i16);
                let got   = self.sbuf.read_samples(&mut out[start..], want);
                out.truncate(start + got * 2);
                *done += got as i32;
                todo  -= got as i32;
            } else {
                self.time = 0;
                let (center, left, right) = sbuf_split(&mut self.sbuf);
                let stereo = self.apu.end_frame(FRAME_SIZE, center, left, right);
                self.sbuf.end_frame(FRAME_SIZE, stereo);
            }
        }
    }

    fn current_notes(&self) -> [i32; 4] {
        let q = |i: usize| -> i32 { self.note_queues[i].last().map_or(-1, |&n| n as i32) };
        let n0 = q(0);
        [
            n0,
            if self.channel_split { q(1) } else { n0 },
            if self.channel_split { q(2) } else { n0 },
            if self.channel_split { q(3) } else { n0 },
        ]
    }

    /// Process one audio block. Mirrors C++ PAPUEngine::processBlock.
    /// MIDI events must be sorted by `pos`. Output is appended as i16 stereo pairs.
    pub fn process_block(&mut self, block_size: i32, params: &Params, events: &[MidiEvent], out: &mut Vec<i16>) {
        // Apply global EQ and wave preset (mirrors outer processBlock param checks)
        self.set_wave(params.wave_index as usize);
        if params.treble != self.current_treble {
            self.current_treble = params.treble;
            self.set_treble(params.treble);
        }
        if params.bass != self.current_bass {
            self.current_bass = params.bass;
            self.set_bass(params.bass);
        }

        // Update LFO params (mirrors outer processBlock vib param update before engine call)
        let d1 = 0.25_f32 * params.pulse1_vib_amt / 100.0_f32;
        self.lfos[0].set_params(params.pulse1_vib_rate as f64, d1 as f64);
        let d2 = 0.25_f32 * params.pulse2_vib_amt / 100.0_f32;
        self.lfos[1].set_params(params.pulse2_vib_rate as f64, d2 as f64);
        let d3 = 0.25_f32 * params.wave_vib_amt / 100.0_f32;
        self.lfos[2].set_params(params.wave_vib_rate as f64, d3 as f64);

        // Global volume + panning (written every block via regCache, like C++)
        self.write_reg(0xff24, (0x08 | params.output) as u8, false);
        let pan: u8 =
            (if params.pulse1_ol { 0x10u8 } else { 0 }) |
            (if params.pulse1_or { 0x01u8 } else { 0 }) |
            (if params.pulse2_ol { 0x20u8 } else { 0 }) |
            (if params.pulse2_or { 0x02u8 } else { 0 }) |
            (if params.wave_ol   { 0x40u8 } else { 0 }) |
            (if params.wave_or   { 0x04u8 } else { 0 }) |
            (if params.noise_ol  { 0x80u8 } else { 0 }) |
            (if params.noise_or  { 0x08u8 } else { 0 });
        self.write_reg(0xff25, pan, false);

        // channelsplit changes clear queues (note: lastNotes are NOT cleared, like C++)
        let new_split = params.channel_split;
        if new_split != self.channel_split {
            self.channel_split = new_split;
            for q in &mut self.note_queues { q.clear(); }
        }

        let mut done: i32 = 0;

        // Initial sustain run (uses last_notes, no triggers — mirrors C++)
        self.run_oscs(self.last_notes, [false; 4], params);
        self.run_until(&mut done, out, 0, params);  // pos=0 → todo=0, no-op for samples

        'event: for event in events {
            self.run_until(&mut done, out, event.pos, params);

            let ch = event.channel;          // 1-based
            let mut update_bend = false;

            match &event.kind {
                MidiKind::NoteOn(note) => {
                    if ch == 1 || !self.channel_split {
                        self.note_queues[0].push(*note);
                    } else if ch == 2 { self.note_queues[1].push(*note); }
                    else if ch == 3  { self.note_queues[2].push(*note); }
                    else if ch == 4  { self.note_queues[3].push(*note); }
                }
                MidiKind::NoteOff(note) => {
                    let qi: usize = if ch == 1 || !self.channel_split { 0 }
                                    else if ch == 2 { 1 }
                                    else if ch == 3 { 2 }
                                    else if ch == 4 { 3 }
                                    else { continue 'event; };
                    if let Some(p) = self.note_queues[qi].iter().position(|&n| n == *note) {
                        self.note_queues[qi].remove(p);
                    }
                }
                MidiKind::AllNotesOff => {
                    for q in &mut self.note_queues { q.clear(); }
                }
                MidiKind::PitchBend(value) => {
                    update_bend = true;
                    // C++: pitchBend (double) = (value - 8192) / 8192.0f * 2
                    // float arithmetic then stored in double
                    let pb_f32 = (*value - 8192) as f32 / 8192.0_f32 * 2.0_f32;
                    self.pitch_bend = pb_f32 as f64;
                }
            }

            let cur_notes = self.current_notes();
            let any_changed = update_bend
                || (0..4).any(|i| cur_notes[i] != self.last_notes[i]);

            if any_changed {
                // Reset LFO on new note (not on pitch bend), mirrors C++ lines 380-382
                if !update_bend && cur_notes[0] != -1 { self.lfos[0].reset(); }
                if !update_bend && cur_notes[1] != -1 { self.lfos[1].reset(); }
                if !update_bend && cur_notes[2] != -1 { self.lfos[2].reset(); }
                let triggers = [
                    self.last_notes[0] != cur_notes[0],
                    self.last_notes[1] != cur_notes[1],
                    self.last_notes[2] != cur_notes[2],
                    self.last_notes[3] != cur_notes[3],
                ];
                self.run_oscs(cur_notes, triggers, params);
                self.last_notes = cur_notes;
            }
        }

        self.run_until(&mut done, out, block_size, params);
    }

    /// Change the loaded wave preset. Matches PAPUEngine::setWave().
    pub fn set_wave(&mut self, index: usize) {
        if index == self.wave_index as usize { return; }
        self.wave_index = index as u8;
        self.write_reg(0xff1A, 0x00, true);
        let ws = WAVE_SAMPLES[index];
        for s in 0..16_u32 {
            let high = ws[(s * 2) as usize];
            let low  = ws[(s * 2 + 1) as usize];
            self.write_reg(0xff30 + s, low | (high << 4), true);
        }
        self.write_reg(0xff1A, 0x80, true);
    }

    pub fn set_treble(&mut self, treble: f64) {
        let eq = BlipEq::new(treble);
        self.apu.treble_eq(&eq);
    }

    pub fn set_bass(&mut self, bass: i32) {
        self.sbuf.bass_freq(bass);
    }

    /// Return the last-played note for the given 1-based channel (-1 = silent).
    /// Mirrors C++ PAPUEngine::getNote(channel).
    pub fn get_note(&self, channel: u8) -> i32 {
        if channel >= 1 && channel <= 4 {
            self.last_notes[(channel - 1) as usize]
        } else {
            -1
        }
    }
}

// ---------------------------------------------------------------------------
// PapuProcessor — multi-voice wrapper (mirrors PAPUAudioProcessor).
// ---------------------------------------------------------------------------

pub struct PapuProcessor {
    engines:    Vec<PapuEngine>,
    next_voice: usize,
}

impl PapuProcessor {
    pub fn new(voices: usize) -> Self {
        PapuProcessor {
            engines:    (0..voices).map(|_| PapuEngine::new()).collect(),
            next_voice: 0,
        }
    }

    pub fn prepare(&mut self, sample_rate: f64) {
        for e in &mut self.engines { e.prepare(sample_rate); }
    }

    /// Process one block. Returns f32 as [left_0..left_{N-1}, right_0..right_{N-1}].
    /// For voices==1, delegates to PapuEngine::process_block (single-voice path with
    /// note priority stacking), matching C++ `papus[0]->processBlock(buffer, midi)`.
    /// For voices>1, uses multi-voice dispatch with findFreeVoice/findVoiceForNote.
    pub fn process_block(&mut self, block_size: i32, params: &Params, events: &[MidiEvent]) -> Vec<f32> {
        let n = self.engines.len();
        let bs = block_size as usize;

        if n == 1 {
            // Single-voice: delegate to PapuEngine::process_block (mirrors C++ voices==1 path)
            let mut i16_out = Vec::new();
            self.engines[0].process_block(block_size, params, events, &mut i16_out);
            // Convert i16 stereo pairs [L0,R0,L1,R1,...] → f32 [L0..Ln-1, R0..Rn-1]
            let mut fb = vec![0.0f32; 2 * bs];
            let pairs = i16_out.len() / 2;
            for i in 0..pairs {
                fb[i]      = i16_out[i * 2]     as f32 / 32768.0_f32;
                fb[i + bs] = i16_out[i * 2 + 1] as f32 / 32768.0_f32;
            }
            return fb;
        }

        let mut fb = vec![0.0f32; 2 * bs];

        for i in 0..n {
            Self::prepare_voice(&mut self.engines[i], params);
        }

        let mut done: i32 = 0;

        for event in events {
            Self::run_until_all(&mut self.engines, &mut done, &mut fb, event.pos, block_size, params);

            match &event.kind {
                MidiKind::NoteOn(_) => {
                    let ch = if params.channel_split { event.channel } else { 1 };
                    let voices = self.engines.len();
                    let mut found_vi: Option<usize> = None;
                    for i in 0..voices {
                        let vi = (self.next_voice + i) % voices;
                        if self.engines[vi].get_note(ch) == -1 {
                            self.next_voice = (vi + 1) % voices;
                            found_vi = Some(vi);
                            break;
                        }
                    }
                    if let Some(vi) = found_vi {
                        Self::handle_message_for_voice(&mut self.engines[vi], event, params);
                    }
                }
                MidiKind::NoteOff(note) => {
                    let note_val = *note;
                    let ch = if params.channel_split { event.channel } else { 1 };
                    let voices = self.engines.len();
                    let mut found_vi: Option<usize> = None;
                    for vi in 0..voices {
                        if self.engines[vi].get_note(ch) == note_val as i32 {
                            found_vi = Some(vi);
                            break;
                        }
                    }
                    if let Some(vi) = found_vi {
                        Self::handle_message_for_voice(&mut self.engines[vi], event, params);
                    }
                }
                MidiKind::PitchBend(_) | MidiKind::AllNotesOff => {
                    for i in 0..n {
                        Self::handle_message_for_voice(&mut self.engines[i], event, params);
                    }
                }
            }
        }

        Self::run_until_all(&mut self.engines, &mut done, &mut fb, block_size, block_size, params);
        fb
    }

    /// Mirror C++ prepareBlock: apply params, write vol/pan, run sustained oscs.
    fn prepare_voice(engine: &mut PapuEngine, params: &Params) {
        engine.set_wave(params.wave_index as usize);
        if params.treble != engine.current_treble {
            engine.current_treble = params.treble;
            engine.set_treble(params.treble);
        }
        if params.bass != engine.current_bass {
            engine.current_bass = params.bass;
            engine.set_bass(params.bass);
        }

        let d1 = 0.25_f32 * params.pulse1_vib_amt / 100.0_f32;
        engine.lfos[0].set_params(params.pulse1_vib_rate as f64, d1 as f64);
        let d2 = 0.25_f32 * params.pulse2_vib_amt / 100.0_f32;
        engine.lfos[1].set_params(params.pulse2_vib_rate as f64, d2 as f64);
        let d3 = 0.25_f32 * params.wave_vib_amt / 100.0_f32;
        engine.lfos[2].set_params(params.wave_vib_rate as f64, d3 as f64);

        engine.write_reg(0xff24, (0x08 | params.output) as u8, false);
        let pan: u8 =
            (if params.pulse1_ol { 0x10u8 } else { 0 }) |
            (if params.pulse1_or { 0x01u8 } else { 0 }) |
            (if params.pulse2_ol { 0x20u8 } else { 0 }) |
            (if params.pulse2_or { 0x02u8 } else { 0 }) |
            (if params.wave_ol   { 0x40u8 } else { 0 }) |
            (if params.wave_or   { 0x04u8 } else { 0 }) |
            (if params.noise_ol  { 0x80u8 } else { 0 }) |
            (if params.noise_or  { 0x08u8 } else { 0 });
        engine.write_reg(0xff25, pan, false);

        let new_split = params.channel_split;
        if new_split != engine.channel_split {
            engine.channel_split = new_split;
            for q in &mut engine.note_queues { q.clear(); }
        }

        let last_notes = engine.last_notes;
        engine.run_oscs(last_notes, [false; 4], params);
        // runUntil(done=0, pos=0) → todo=0 → just runVibrato(0)
        engine.run_vibrato(0, params);
    }

    /// Mirror C++ handleMessage: update queues, re-run oscs if notes changed.
    fn handle_message_for_voice(engine: &mut PapuEngine, event: &MidiEvent, params: &Params) {
        let ch = event.channel;
        let mut update_bend = false;

        match &event.kind {
            MidiKind::NoteOn(note) => {
                if ch == 1 || !engine.channel_split {
                    engine.note_queues[0].push(*note);
                } else if ch == 2 { engine.note_queues[1].push(*note); }
                else if ch == 3  { engine.note_queues[2].push(*note); }
                else if ch == 4  { engine.note_queues[3].push(*note); }
            }
            MidiKind::NoteOff(note) => {
                let qi: usize = if ch == 1 || !engine.channel_split { 0 }
                                else if ch == 2 { 1 }
                                else if ch == 3 { 2 }
                                else if ch == 4 { 3 }
                                else { return; };
                if let Some(p) = engine.note_queues[qi].iter().position(|&n| n == *note) {
                    engine.note_queues[qi].remove(p);
                }
            }
            MidiKind::AllNotesOff => {
                for q in &mut engine.note_queues { q.clear(); }
            }
            MidiKind::PitchBend(value) => {
                update_bend = true;
                let pb_f32 = (*value - 8192) as f32 / 8192.0_f32 * 2.0_f32;
                engine.pitch_bend = pb_f32 as f64;
            }
        }

        let cur_notes = engine.current_notes();
        let any_changed = update_bend
            || (0..4).any(|i| cur_notes[i] != engine.last_notes[i]);

        if any_changed {
            if !update_bend && cur_notes[0] != -1 { engine.lfos[0].reset(); }
            if !update_bend && cur_notes[1] != -1 { engine.lfos[1].reset(); }
            if !update_bend && cur_notes[2] != -1 { engine.lfos[2].reset(); }
            let triggers = [
                engine.last_notes[0] != cur_notes[0],
                engine.last_notes[1] != cur_notes[1],
                engine.last_notes[2] != cur_notes[2],
                engine.last_notes[3] != cur_notes[3],
            ];
            engine.run_oscs(cur_notes, triggers, params);
            engine.last_notes = cur_notes;
        }
    }

    /// Mirror C++ PAPUEngine::runUntil: runVibrato then accumulate i16→f32 into fb.
    /// fb layout: [left_0..left_{block_size-1}, right_0..right_{block_size-1}].
    fn run_voice_until(engine: &mut PapuEngine, mut done_copy: i32, fb: &mut [f32], pos: i32, block_size: i32, params: &Params) {
        let mut todo = pos - done_copy;
        engine.run_vibrato(todo, params);
        while todo > 0 {
            let avail = engine.sbuf.samples_avail() as i32;
            if avail > 0 {
                let want = todo.min(512).min(avail) as usize;
                let mut tmp = vec![0i16; want * 2];
                let got = engine.sbuf.read_samples(&mut tmp, want);
                for i in 0..got {
                    fb[done_copy as usize + i]
                        += tmp[i * 2]     as f32 / 32768.0_f32;
                    fb[done_copy as usize + i + block_size as usize]
                        += tmp[i * 2 + 1] as f32 / 32768.0_f32;
                }
                done_copy += got as i32;
                todo      -= got as i32;
            } else {
                engine.time = 0;
                let (center, left, right) = sbuf_split(&mut engine.sbuf);
                let stereo = engine.apu.end_frame(FRAME_SIZE, center, left, right);
                engine.sbuf.end_frame(FRAME_SIZE, stereo);
            }
        }
    }

    /// Mirror C++ PAPUAudioProcessor::runUntil: advance all voices in parallel.
    fn run_until_all(engines: &mut [PapuEngine], done: &mut i32, fb: &mut Vec<f32>, pos: i32, block_size: i32, params: &Params) {
        let clamped = pos.min(block_size);
        let todo = (clamped - *done).max(0);
        for engine in engines.iter_mut() {
            let done_copy = *done;
            Self::run_voice_until(engine, done_copy, fb, clamped, block_size, params);
        }
        *done += todo;
    }
}
