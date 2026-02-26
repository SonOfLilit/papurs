// Game Boy APU emulation — port of Gb_Snd_Emu 0.1.4
// Copyright (C) 2003-2005  Shay Green (original C++ Gb_Snd_Emu)
// Copyright (C) 2026  Roland Rabien (PAPU integration)
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
//
// Bit-exact with C++ original (Phases 2-8 of the port roadmap).

use crate::blip::{BlipBuffer, BlipEq, BlipSynth};

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

pub type GbTime = i64;
pub type GbAddr = u32;

const GB_APU_MAX_VOL: i32 = 7;
const OSC_COUNT:      usize = 4;

/// Start address of APU registers.
pub const START_ADDR:     GbAddr = 0xff10;
/// End address of APU registers (inclusive).
pub const END_ADDR:       GbAddr = 0xff3f;
pub const REGISTER_COUNT: usize  = (END_ADDR - START_ADDR + 1) as usize;

// output_select values (mirror C++ outputs[] indices)
const OUT_NONE:   u8 = 0;
const OUT_RIGHT:  u8 = 1;
const OUT_LEFT:   u8 = 2;
const OUT_CENTER: u8 = 3;

// ---------------------------------------------------------------------------
// Gb_Osc — base oscillator state
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct GbOsc {
    pub output_select: u8,   // 0=none, 1=right, 2=left, 3=center
    pub delay:         i32,
    pub last_amp:      i32,
    pub period:        i32,
    pub volume:        i32,
    pub global_volume: i32,
    pub frequency:     i32,
    pub length:        i32,
    pub new_length:    i32,
    pub enabled:       bool,
    pub length_enabled: bool,
}

impl GbOsc {
    fn new() -> Self {
        GbOsc {
            output_select: OUT_CENTER,
            delay:         0,
            last_amp:      0,
            period:        2048,
            volume:        0,
            global_volume: GB_APU_MAX_VOL,
            frequency:     0,
            length:        0,
            new_length:    0,
            enabled:       false,
            length_enabled: false,
        }
    }

    fn reset(&mut self) {
        self.delay         = 0;
        self.last_amp      = 0;
        self.period        = 2048;
        self.volume        = 0;
        self.global_volume = GB_APU_MAX_VOL;
        self.frequency     = 0;
        self.length        = 0;
        self.enabled       = false;
        self.length_enabled = false;
        self.output_select = OUT_CENTER;
    }

    fn clock_length(&mut self) {
        if self.length_enabled && self.length > 0 {
            self.length -= 1;
        }
    }

    fn write_register(&mut self, reg: usize, value: u8) {
        if reg == 4 {
            self.length_enabled = (value & 0x40) != 0;
        }
    }
}

// ---------------------------------------------------------------------------
// Gb_Env — envelope generator
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct GbEnv {
    pub osc:        GbOsc,
    pub env_period: i32,
    pub env_dir:    i32,
    pub env_delay:  i32,
    pub new_volume: i32,
}

impl GbEnv {
    fn new() -> Self {
        GbEnv {
            osc:        GbOsc::new(),
            env_period: 0,
            env_dir:    0,
            env_delay:  0,
            new_volume: 0,
        }
    }

    fn reset(&mut self) {
        self.env_period = 0;
        self.env_dir    = 0;
        self.env_delay  = 0;
        self.new_volume = 0;
        self.osc.reset();
    }

    fn clock_envelope(&mut self) {
        if self.env_delay > 0 {
            self.env_delay -= 1;
            if self.env_delay == 0 {
                self.env_delay = self.env_period;
                if self.env_dir != 0 {
                    if self.osc.volume < 15 {
                        self.osc.volume += 1;
                    }
                } else if self.osc.volume > 0 {
                    self.osc.volume -= 1;
                }
            }
        }
    }

    fn write_register(&mut self, reg: usize, value: u8) {
        let trigger = 0x80u8;
        if reg == 2 {
            self.env_period = (value & 7) as i32;
            self.env_dir    = (value & 8) as i32;
            self.osc.volume = (value >> 4) as i32;
            self.new_volume = self.osc.volume;
        } else if reg == 4 && (value & trigger) != 0 {
            self.env_delay   = self.env_period;
            self.osc.volume  = self.new_volume;
            self.osc.enabled = true;
        }
        self.osc.write_register(reg, value);
    }
}

// ---------------------------------------------------------------------------
// Gb_Square — square wave with optional sweep
// ---------------------------------------------------------------------------

const DUTY_TABLE: [i32; 4] = [1, 2, 4, 6];

#[derive(Clone, Debug)]
pub struct GbSquare {
    pub env:          GbEnv,
    pub phase:        i32,
    pub duty:         i32,
    pub sweep_period: i32,
    pub sweep_delay:  i32,
    pub sweep_shift:  i32,
    pub sweep_dir:    i32,
    pub sweep_freq:   i32,
    pub has_sweep:    bool,
}

impl GbSquare {
    fn new(has_sweep: bool) -> Self {
        GbSquare {
            env:          GbEnv::new(),
            phase:        1,
            duty:         1,
            sweep_period: 0,
            sweep_delay:  0,
            sweep_shift:  0,
            sweep_dir:    0,
            sweep_freq:   0,
            has_sweep,
        }
    }

    fn reset(&mut self) {
        self.phase        = 1;
        self.duty         = 1;
        self.sweep_period = 0;
        self.sweep_delay  = 0;
        self.sweep_shift  = 0;
        self.sweep_dir    = 0;
        self.sweep_freq   = 0;
        self.env.reset();
        self.env.osc.new_length = 0;
    }

    fn clock_length(&mut self)   { self.env.osc.clock_length(); }
    fn clock_envelope(&mut self) { self.env.clock_envelope(); }

    fn clock_sweep(&mut self) {
        if self.sweep_period != 0 && self.sweep_delay > 0 {
            self.sweep_delay -= 1;
            if self.sweep_delay == 0 {
                self.sweep_delay = self.sweep_period;
                self.env.osc.frequency = self.sweep_freq;
                self.env.osc.period    = (2048 - self.sweep_freq) * 4;

                let offset = if self.sweep_dir != 0 {
                    -(self.sweep_freq >> self.sweep_shift)
                } else {
                    self.sweep_freq >> self.sweep_shift
                };
                self.sweep_freq += offset;

                if self.sweep_freq < 0 {
                    self.sweep_freq = 0;
                } else if self.sweep_freq >= 2048 {
                    self.sweep_delay = 0;
                    self.sweep_freq  = 2048; // stop sound output
                }
            }
        }
    }

    fn write_register(&mut self, reg: usize, value: u8) {
        const TRIGGER: u8 = 0x80;
        match reg {
            0 => {
                self.sweep_period = ((value >> 4) & 7) as i32;
                self.sweep_shift  = (value & 7)  as i32;
                self.sweep_dir    = (value & 0x08) as i32;
            }
            1 => {
                self.env.osc.new_length = 64 - (value & 0x3f) as i32;
                self.env.osc.length     = self.env.osc.new_length;
                self.duty               = DUTY_TABLE[(value >> 6) as usize];
            }
            3 => {
                self.env.osc.frequency = (self.env.osc.frequency & !0xFF) + value as i32;
                self.env.osc.length    = self.env.osc.new_length;
            }
            4 => {
                self.env.osc.frequency = (value as i32 & 7) * 0x100
                    + (self.env.osc.frequency & 0xFF);
                self.env.osc.length    = self.env.osc.new_length;
                if (value & TRIGGER) != 0 {
                    self.sweep_freq = self.env.osc.frequency;
                    if self.has_sweep && self.sweep_period != 0 && self.sweep_shift != 0 {
                        self.sweep_delay = 1;
                        self.clock_sweep(); // no `osc` borrow active here
                    }
                }
            }
            _ => {}
        }
        self.env.osc.period = (2048 - self.env.osc.frequency) * 4;
        self.env.write_register(reg, value);
    }

    fn run(&mut self, mut time: GbTime, end_time: GbTime, synth: &BlipSynth,
           output: Option<&mut BlipBuffer>) {
        let osc = &mut self.env.osc;
        let disabled = !osc.enabled
            || (osc.length == 0 && osc.length_enabled)
            || osc.volume == 0
            || self.sweep_freq == 2048
            || osc.frequency == 0
            || osc.period < 27;

        if let Some(out) = output {
            if disabled {
                if osc.last_amp != 0 {
                    synth.offset(time, -osc.last_amp, out);
                    osc.last_amp = 0;
                }
                osc.delay = 0;
            } else {
                let mut amp = if self.phase < self.duty { osc.volume } else { -osc.volume };
                amp *= osc.global_volume;
                if amp != osc.last_amp {
                    synth.offset(time, amp - osc.last_amp, out);
                    osc.last_amp = amp;
                }

                time += osc.delay as GbTime;
                if time < end_time {
                    let duty  = self.duty;
                    let period = osc.period as GbTime;
                    let mut phase = self.phase;
                    amp *= 2;

                    loop {
                        phase = (phase + 1) & 7;
                        if phase == 0 || phase == duty {
                            amp = -amp;
                            synth.offset(time, amp, out);
                        }
                        time += period;
                        if time >= end_time { break; }
                    }

                    self.phase   = phase;
                    osc.last_amp = amp >> 1;
                }
                osc.delay = (time - end_time) as i32;
            }
        } else if disabled {
            osc.last_amp = 0;
            osc.delay    = 0;
        }
    }
}

// ---------------------------------------------------------------------------
// Gb_Wave — wavetable oscillator
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct GbWave {
    pub osc:                      GbOsc,
    pub volume_shift:             i32,
    pub wave_pos:                 usize,
    pub wave:                     [u8; 32],
    pub new_enabled:              bool,
    pub disable_on_zero_crossing: i32,  // -1=off, ≥0=countdown
}

impl GbWave {
    fn new() -> Self {
        GbWave {
            osc:                      GbOsc::new(),
            volume_shift:             0,
            wave_pos:                 0,
            wave:                     [0u8; 32],
            new_enabled:              false,
            disable_on_zero_crossing: -1,
        }
    }

    fn reset(&mut self) {
        self.volume_shift             = 0;
        self.wave_pos                 = 0;
        self.wave                     = [0u8; 32];
        self.new_enabled              = false;
        self.disable_on_zero_crossing = -1;
        self.osc.reset();
        self.osc.new_length = 0;
    }

    fn clock_length(&mut self) { self.osc.clock_length(); }

    fn write_register(&mut self, reg: usize, value: u8) {
        let osc = &mut self.osc;
        match reg {
            0 => {
                self.new_enabled = (value & 0x80) != 0;
                osc.enabled &= self.new_enabled;
            }
            1 => {
                osc.new_length = 256 - value as i32;
                osc.length     = osc.new_length;
            }
            2 => {
                osc.volume       = ((value >> 5) & 3) as i32;
                self.volume_shift = (osc.volume - 1) & 7; // silence=7
            }
            3 => {
                osc.frequency = (osc.frequency & !0xFF) + value as i32;
            }
            4 => {
                osc.frequency = (value as i32 & 7) * 0x100 + (osc.frequency & 0xFF);
                if self.new_enabled && (value & 0x80) != 0 {
                    self.wave_pos  = 0;
                    osc.length     = osc.new_length;
                    osc.enabled    = true;
                }
            }
            _ => {}
        }
        osc.period = (2048 - osc.frequency) * 2;
        osc.write_register(reg, value);
    }

    fn run(&mut self, mut time: GbTime, end_time: GbTime, synth: &BlipSynth,
           output: Option<&mut BlipBuffer>) {
        let osc = &mut self.osc;
        let disabled = !osc.enabled
            || (osc.length == 0 && osc.length_enabled)
            || osc.volume == 0
            || osc.frequency == 0
            || osc.period < 7;

        if let Some(out) = output {
            if disabled {
                if osc.last_amp != 0 {
                    synth.offset(time, -osc.last_amp, out);
                    osc.last_amp = 0;
                }
                osc.delay = 0;
            } else {
                let vol_factor = osc.global_volume * 2;

                let diff = (self.wave[self.wave_pos] >> self.volume_shift as u32) as i32
                    * vol_factor - osc.last_amp;
                if diff != 0 {
                    osc.last_amp += diff;
                    synth.offset(time, diff, out);
                }

                time += osc.delay as GbTime;
                if time < end_time {
                    let vol_shift = self.volume_shift as u32;
                    let period    = osc.period as GbTime;
                    let doz       = self.disable_on_zero_crossing;
                    let mut wave_pos = self.wave_pos;

                    loop {
                        wave_pos = (wave_pos + 1) % 32;
                        let amp   = (self.wave[wave_pos] >> vol_shift) as i32 * vol_factor;
                        let delta = amp - osc.last_amp;

                        if doz >= 0 {
                            let crosszero = (osc.last_amp > 7 && amp <= 7)
                                || (osc.last_amp < 7 && amp >= 7)
                                || amp == 7
                                || doz == 0;
                            if crosszero {
                                osc.enabled = false;
                                self.disable_on_zero_crossing = -1;
                                synth.offset(time, 7, out);
                                self.wave_pos = wave_pos;
                                osc.delay = (time + period - end_time) as i32;
                                return;
                            } else {
                                self.disable_on_zero_crossing -= 1;
                            }
                        } else if delta != 0 {
                            osc.last_amp = amp;
                            synth.offset(time, delta, out);
                        }

                        time += period;
                        if time >= end_time { break; }
                    }

                    self.wave_pos = wave_pos;
                }
                osc.delay = (time - end_time) as i32;
            }
        } else if disabled {
            osc.last_amp = 0;
            osc.delay    = 0;
        }
    }
}

// ---------------------------------------------------------------------------
// Gb_Noise — LFSR noise channel
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct GbNoise {
    pub env: GbEnv,
    pub bits: u32,
    pub tap:  i32,
}

impl GbNoise {
    fn new() -> Self {
        GbNoise {
            env:  GbEnv::new(),
            bits: 1,
            tap:  14,
        }
    }

    fn reset(&mut self) {
        self.bits = 1;
        self.tap  = 14;
        self.env.reset();
    }

    fn clock_length(&mut self)   { self.env.osc.clock_length(); }
    fn clock_envelope(&mut self) { self.env.clock_envelope(); }

    fn write_register(&mut self, reg: usize, value: u8) {
        let osc = &mut self.env.osc;
        match reg {
            1 => {
                osc.new_length = 64 - (value & 0x3f) as i32;
                osc.length     = osc.new_length;
            }
            2 => {
                // Noise-specific envelope quirk
                let temp = self.env.osc.volume;
                self.env.write_register(reg, value);
                if (value & 0xF8) != 0 {
                    self.env.osc.volume = temp;
                }
                return;
            }
            3 => {
                self.tap = 14 - (value as i32 & 8);
                let divisor = (value as i32 & 7) * 16;
                let divisor = if divisor == 0 { 8 } else { divisor };
                osc.period = divisor << (value >> 4) as i32;
            }
            4 => {
                if (value & 0x80) != 0 {
                    self.bits   = !0u32;
                    osc.length  = osc.new_length;
                }
            }
            _ => {}
        }
        self.env.write_register(reg, value);
    }

    fn run(&mut self, mut time: GbTime, end_time: GbTime, synth: &BlipSynth,
           output: Option<&mut BlipBuffer>) {
        let osc = &mut self.env.osc;
        let disabled = !osc.enabled
            || (osc.length == 0 && osc.length_enabled)
            || osc.volume == 0;

        if let Some(out) = output {
            if disabled {
                if osc.last_amp != 0 {
                    synth.offset(time, -osc.last_amp, out);
                    osc.last_amp = 0;
                }
                osc.delay = 0;
            } else {
                let mut amp = if (self.bits & 1) != 0 { -osc.volume } else { osc.volume };
                amp *= osc.global_volume;
                if amp != osc.last_amp {
                    synth.offset(time, amp - osc.last_amp, out);
                    osc.last_amp = amp;
                }

                time += osc.delay as GbTime;
                if time < end_time {
                    // Use resampled time arithmetic to avoid multiply in loop
                    let resampled_period = out.resampled_duration(osc.period as i64);
                    let mut resampled_time = out.resampled_time(time);
                    let tap  = self.tap as u32;
                    let mask = !(1u32 << tap);
                    let mut bits = self.bits;
                    amp *= 2;
                    let period = osc.period as GbTime;

                    loop {
                        let feedback = bits;
                        bits >>= 1;
                        let feedback = 1 & (feedback ^ bits);
                        time += period;
                        bits = (feedback << tap) | (bits & mask);
                        if feedback != 0 {
                            amp = -amp;
                            synth.offset_resampled(resampled_time, amp, out);
                        }
                        resampled_time = resampled_time.wrapping_add(resampled_period);
                        if time >= end_time { break; }
                    }

                    self.bits    = bits;
                    osc.last_amp = amp >> 1;
                }
                osc.delay = (time - end_time) as i32;
            }
        } else if disabled {
            osc.last_amp = 0;
            osc.delay    = 0;
        }
    }
}

// ---------------------------------------------------------------------------
// Gb_Apu — top-level APU
// ---------------------------------------------------------------------------

pub struct GbApu {
    pub square1: GbSquare,
    pub square2: GbSquare,
    pub wave:    GbWave,
    pub noise:   GbNoise,

    pub regs: [u8; REGISTER_COUNT],

    next_frame_time: GbTime,
    last_time:       GbTime,
    frame_count:     i32,
    stereo_found:    bool,

    /// Shared synth for Square 1 & 2: quality=3 (blip_good_quality), range=210.
    pub square_synth: BlipSynth,
    /// Shared synth for Wave & Noise: quality=2 (blip_med_quality), range=210.
    pub other_synth:  BlipSynth,
}

/// Helper: get a mutable reference to the buffer matching `select`.
/// Returns `None` for `OUT_NONE`.
fn pick_buf<'a>(
    select: u8,
    center: &'a mut BlipBuffer,
    left:   &'a mut BlipBuffer,
    right:  &'a mut BlipBuffer,
) -> Option<&'a mut BlipBuffer> {
    match select {
        OUT_NONE   => None,
        OUT_RIGHT  => Some(right),
        OUT_LEFT   => Some(left),
        _          => Some(center),   // OUT_CENTER (3) or any other
    }
}

impl GbApu {
    pub fn new() -> Self {
        let mut apu = GbApu {
            square1: GbSquare::new(true),
            square2: GbSquare::new(false),
            wave:    GbWave::new(),
            noise:   GbNoise::new(),
            regs:    [0u8; REGISTER_COUNT],
            next_frame_time: 0,
            last_time:       0,
            frame_count:     0,
            stereo_found:    false,
            // quality=3, range = 15*7*2 = 210
            square_synth: BlipSynth::new(3, 210),
            // quality=2, range = 15*7*2 = 210
            other_synth:  BlipSynth::new(2, 210),
        };
        // volume(1.0) → vol = 1.0 * 0.60 / 4 = 0.15; synth.volume(0.15)
        apu.set_volume(1.0);
        apu.reset();
        apu
    }

    pub fn treble_eq(&mut self, eq: &BlipEq) {
        self.square_synth.treble_eq(eq);
        self.other_synth.treble_eq(eq);
    }

    /// Set overall volume (1.0 = full).
    pub fn set_volume(&mut self, vol: f64) {
        let v = vol * 0.60 / OSC_COUNT as f64;
        self.square_synth.volume(v);
        self.other_synth.volume(v);
    }

    /// Reset all oscillators and internal state.
    pub fn reset(&mut self) {
        self.next_frame_time = 0;
        self.last_time       = 0;
        self.frame_count     = 0;
        self.stereo_found    = false;

        self.square1.reset();
        self.square2.reset();
        self.wave.reset();
        self.noise.reset();

        self.regs.fill(0);
    }

    /// Set all oscillators to use the given stereo buffer arrangement.
    /// Mirrors Gb_Apu::output(center, left, right).
    pub fn output(&mut self, select: u8) {
        self.square1.env.osc.output_select = select;
        self.square2.env.osc.output_select = select;
        self.wave.osc.output_select        = select;
        self.noise.env.osc.output_select   = select;
    }

    /// Set a single oscillator's output select.
    pub fn osc_output(&mut self, index: usize, select: u8) {
        match index {
            0 => self.square1.env.osc.output_select = select,
            1 => self.square2.env.osc.output_select = select,
            2 => self.wave.osc.output_select        = select,
            3 => self.noise.env.osc.output_select   = select,
            _ => {}
        }
    }

    pub fn stop_wave(&mut self)  { self.wave.disable_on_zero_crossing = 32; }
    pub fn reset_stop_wave(&mut self) { self.wave.disable_on_zero_crossing = -1; }

    /// Run all oscillators up to `end_time`, clock frame sequencer.
    fn run_until(
        &mut self,
        end_time: GbTime,
        center: &mut BlipBuffer,
        left:   &mut BlipBuffer,
        right:  &mut BlipBuffer,
    ) {
        assert!(end_time >= self.last_time);
        if end_time == self.last_time { return; }

        loop {
            let time = self.next_frame_time.min(end_time);

                // Run each oscillator sequentially to avoid simultaneous mutable borrows.
            // square1, square2 use square_synth; wave, noise use other_synth.
            // Run them sequentially to avoid simultaneous mutable borrows.
            {
                let select = self.square1.env.osc.output_select;
                if select != OUT_NONE && select != OUT_CENTER { self.stereo_found = true; }
                let buf = pick_buf(select, center, left, right);
                self.square1.run(self.last_time, time, &self.square_synth, buf);
            }
            {
                let select = self.square2.env.osc.output_select;
                if select != OUT_NONE && select != OUT_CENTER { self.stereo_found = true; }
                let buf = pick_buf(select, center, left, right);
                self.square2.run(self.last_time, time, &self.square_synth, buf);
            }
            {
                let select = self.wave.osc.output_select;
                if select != OUT_NONE && select != OUT_CENTER { self.stereo_found = true; }
                let buf = pick_buf(select, center, left, right);
                self.wave.run(self.last_time, time, &self.other_synth, buf);
            }
            {
                let select = self.noise.env.osc.output_select;
                if select != OUT_NONE && select != OUT_CENTER { self.stereo_found = true; }
                let buf = pick_buf(select, center, left, right);
                self.noise.run(self.last_time, time, &self.other_synth, buf);
            }

            self.last_time = time;
            if time == end_time { break; }

            // Advance to next frame sequencer tick (256 Hz = 16384 clocks)
            self.next_frame_time += 4_194_304 / 256;

            // 256 Hz: clock length counters
            self.square1.clock_length();
            self.square2.clock_length();
            self.wave.clock_length();
            self.noise.clock_length();

            self.frame_count = (self.frame_count + 1) & 3;
            if self.frame_count == 0 {
                // 64 Hz: clock envelopes
                self.square1.clock_envelope();
                self.square2.clock_envelope();
                self.noise.clock_envelope();
            }

            if (self.frame_count & 1) != 0 {
                // 128 Hz: clock sweep (odd frames)
                self.square1.clock_sweep();
            }
        }
    }

    /// End the current frame, return true if any oscillator used stereo output.
    pub fn end_frame(
        &mut self,
        end_time: GbTime,
        center: &mut BlipBuffer,
        left:   &mut BlipBuffer,
        right:  &mut BlipBuffer,
    ) -> bool {
        if end_time > self.last_time {
            self.run_until(end_time, center, left, right);
        }

        self.next_frame_time -= end_time;
        self.last_time       -= end_time;

        let result = self.stereo_found;
        self.stereo_found = false;
        result
    }

    /// Write to an APU register at the given clock time.
    pub fn write_register(
        &mut self,
        time:  GbTime,
        addr:  GbAddr,
        data:  u8,
        center: &mut BlipBuffer,
        left:   &mut BlipBuffer,
        right:  &mut BlipBuffer,
    ) {
        let reg = addr.wrapping_sub(START_ADDR) as usize;
        if reg >= REGISTER_COUNT { return; }

        self.run_until(time, center, left, right);

        self.regs[reg] = data;

        if addr < 0xff24 {
            // Oscillator register
            let index = reg / 5;
            let local_reg = reg - index * 5;
            match index {
                0 => self.square1.write_register(local_reg, data),
                1 => self.square2.write_register(local_reg, data),
                2 => self.wave.write_register(local_reg, data),
                3 => self.noise.write_register(local_reg, data),
                _ => {}
            }
        } else if addr == 0xff24 {
            // Global volume
            let new_vol = (data & 7) as i32;
            let old_vol = self.square1.env.osc.global_volume;

            if old_vol != new_vol {
                let mut any_enabled = false;

                // Process each oscillator's last_amp rescaling
                macro_rules! rescale_osc {
                    ($osc:expr) => {{
                        if $osc.enabled {
                            if $osc.last_amp != 0 {
                                let new_amp = $osc.last_amp * new_vol / old_vol;
                                let buf = pick_buf($osc.output_select, center, left, right);
                                if let Some(b) = buf {
                                    self.square_synth.offset(time, new_amp - $osc.last_amp, b);
                                }
                                $osc.last_amp = new_amp;
                            }
                            any_enabled |= $osc.volume != 0;
                        }
                        $osc.global_volume = new_vol;
                    }};
                }

                rescale_osc!(self.square1.env.osc);
                rescale_osc!(self.square2.env.osc);
                rescale_osc!(self.wave.osc);
                rescale_osc!(self.noise.env.osc);

                if !any_enabled {
                    // C++: if (square1.outputs[3]) square_synth.offset(..., square1.outputs[3])
                    // outputs[3] is always center (set by apu.output()), regardless of output_select.
                    self.square_synth.offset(time, (new_vol - old_vol) * 15 * 2, center);
                }
            }
        } else if addr == 0xff25 || addr == 0xff26 {
            let mask: i32 = if (self.regs[0xff26 - START_ADDR as usize] & 0x80) != 0 { !0 } else { 0 };
            let flags = (self.regs[0xff25 - START_ADDR as usize] as i32) & mask;

            // Process each oscillator's output routing change
            macro_rules! reroute_osc {
                ($osc:expr, $i:expr) => {{
                    $osc.enabled = $osc.enabled && (mask != 0);
                    let bits = flags >> $i;
                    let old_select = $osc.output_select;
                    let new_select = (((bits >> 3) & 2) | (bits & 1)) as u8;
                    $osc.output_select = new_select;
                    if new_select != old_select && $osc.last_amp != 0 {
                        if old_select != OUT_NONE {
                            let buf = pick_buf(old_select, center, left, right);
                            if let Some(b) = buf {
                                self.square_synth.offset(time, -$osc.last_amp, b);
                            }
                        }
                        $osc.last_amp = 0;
                    }
                }};
            }

            reroute_osc!(self.square1.env.osc, 0);
            reroute_osc!(self.square2.env.osc, 1);
            reroute_osc!(self.wave.osc,        2);
            reroute_osc!(self.noise.env.osc,   3);
        } else if addr >= 0xff30 {
            // Wave RAM
            let index = ((addr & 0x0f) * 2) as usize;
            self.wave.wave[index]     = data >> 4;
            self.wave.wave[index + 1] = data & 0x0f;
        }
    }

    /// Read from an APU register.
    pub fn read_register(
        &mut self,
        time: GbTime,
        addr: GbAddr,
        center: &mut BlipBuffer,
        left:   &mut BlipBuffer,
        right:  &mut BlipBuffer,
    ) -> u8 {
        self.run_until(time, center, left, right);

        let mut data = self.regs[(addr - START_ADDR) as usize];
        if addr == 0xff26 {
            data &= 0xf0;
            for i in 0..OSC_COUNT {
                let (enabled, length, length_enabled) = match i {
                    0 => (self.square1.env.osc.enabled,
                          self.square1.env.osc.length,
                          self.square1.env.osc.length_enabled),
                    1 => (self.square2.env.osc.enabled,
                          self.square2.env.osc.length,
                          self.square2.env.osc.length_enabled),
                    2 => (self.wave.osc.enabled,
                          self.wave.osc.length,
                          self.wave.osc.length_enabled),
                    _ => (self.noise.env.osc.enabled,
                          self.noise.env.osc.length,
                          self.noise.env.osc.length_enabled),
                };
                if enabled && (length > 0 || !length_enabled) {
                    data |= 1 << i;
                }
            }
        }
        data
    }
}

impl Default for GbApu {
    fn default() -> Self { Self::new() }
}

// ---------------------------------------------------------------------------
// GoldHarness — mirrors the C++ GoldHarness for gold tests
// ---------------------------------------------------------------------------

use crate::blip::StereoBuffer;

const WAVE_PRESET_0: [u8; 32] = [
     0,  2,  4,  6,  8, 10, 12, 14, 15, 15, 15, 14, 14, 13, 13, 12,
    12, 11, 10,  9,  8,  7,  6,  5,  4,  4,  3,  3,  2,  2,  1,  1
];

pub struct GoldHarness {
    pub apu:  GbApu,
    pub sbuf: StereoBuffer,
    pub time: GbTime,
}

impl GoldHarness {
    pub fn new() -> Self {
        GoldHarness {
            apu:  GbApu::new(),
            sbuf: StereoBuffer::new(),
            time: 0,
        }
    }

    pub fn init(&mut self) {
        self.time = 0;
        let eq = BlipEq::new(-20.0);
        self.apu.treble_eq(&eq);

        self.sbuf.bass_freq(461);
        self.sbuf.clock_rate(4_194_304);
        self.sbuf.set_sample_rate(44_100);

        // Route all oscillators to center (default)
        self.apu.output(OUT_CENTER);

        // Load wave preset 0
        self.write_reg(0xff1A, 0x00); // wave disable
        for s in 0..16usize {
            let high = WAVE_PRESET_0[s * 2];
            let low  = WAVE_PRESET_0[s * 2 + 1];
            self.write_reg(0xff30 + s as u32, (low | (high << 4)) as u32);
        }
        self.write_reg(0xff1A, 0x80); // wave enable
        self.write_reg(0xff26, 0x8f); // APU enable, all channels on
    }

    pub fn write_reg(&mut self, addr: u32, val: u32) {
        self.time += 4;
        let (center, left, right) = sbuf_split(&mut self.sbuf);
        self.apu.write_register(self.time, addr, val as u8, center, left, right);
    }

    pub fn render(&mut self, n_pairs: usize, out: &mut Vec<i16>) {
        let mut done = 0;
        while done < n_pairs {
            let avail = self.sbuf.samples_avail() as usize;
            if avail > 0 {
                let want = (n_pairs - done).min(avail).min(512);
                let start = out.len();
                out.resize(start + want * 2, 0);
                let got = self.sbuf.read_samples(&mut out[start..], want);
                out.truncate(start + got * 2);
                done += got;
            } else {
                self.time = 0;
                let (center, left, right) = sbuf_split(&mut self.sbuf);
                let stereo = self.apu.end_frame(1024, center, left, right);
                self.sbuf.end_frame(1024, stereo);
            }
        }
    }
}

pub fn sbuf_split(sbuf: &mut StereoBuffer)
    -> (&mut BlipBuffer, &mut BlipBuffer, &mut BlipBuffer)
{
    let (s0, rest): (&mut [BlipBuffer], &mut [BlipBuffer]) = sbuf.bufs.split_at_mut(1);
    let (s1, s2):   (&mut [BlipBuffer], &mut [BlipBuffer]) = rest.split_at_mut(1);
    (&mut s0[0], &mut s1[0], &mut s2[0])
}
