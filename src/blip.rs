// Port of Blargg's Blip_Buffer 0.3.4 + Blip_Synth + Stereo_Buffer
// Copyright (C) 2003-2005  Shay Green (original C++ Blip_Buffer)
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
// Bit-exact with the C++ original for gold test compatibility

// ---------------------------------------------------------------------------
// Constants (matching Blip_Buffer.h / Blip_Synth.h)
// ---------------------------------------------------------------------------

const BLIP_BUFFER_ACCURACY: u32 = 16;
const BLIP_RES_BITS: usize = 5;
const BLIP_RES: usize = 1 << BLIP_RES_BITS; // 32
const MAX_RES: usize = BLIP_RES;             // 32 (== 1 << blip_res_bits_)
const WIDEST_IMPULSE: usize = 24;
const SAMPLE_OFFSET: i64 = 0x7F7F;
const ACCUM_FRACT: u32 = 15;
const IMPULSE_BITS: u32 = 15;
const IMPULSE_AMP: i64 = 1 << IMPULSE_BITS;   // 32768
const IMPULSE_OFFSET: i64 = IMPULSE_AMP / 2;  // 16384

// ---------------------------------------------------------------------------
// BlipEq — treble EQ parameters
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct BlipEq {
    pub treble: f64,
    pub cutoff: i64,
    pub sample_rate: i64,
}

impl BlipEq {
    /// Construct with treble dB only (cutoff=0, sample_rate=44100).
    pub fn new(treble: f64) -> Self {
        BlipEq { treble, cutoff: 0, sample_rate: 44100 }
    }
    pub fn new_full(treble: f64, cutoff: i64, sample_rate: i64) -> Self {
        BlipEq { treble, cutoff, sample_rate }
    }
}

// ---------------------------------------------------------------------------
// BlipBuffer — resampling sample buffer
// ---------------------------------------------------------------------------

pub struct BlipBuffer {
    /// Resampling factor = samples_per_sec / clocks_per_sec * 2^16.
    pub factor: u64,
    /// Current resampled time offset; samples_avail = offset >> 16.
    pub offset: u64,
    /// Sample data, biased by SAMPLE_OFFSET.  Size = buffer_size + WIDEST_IMPULSE + 2.
    pub buffer: Vec<u16>,
    pub buffer_size: usize,
    pub reader_accum: i64,
    pub bass_shift: i32,
    samples_per_sec: i64,
    clocks_per_sec: i64,
    bass_freq_: i32,
    length_: i32,
}

impl BlipBuffer {
    pub fn new() -> Self {
        BlipBuffer {
            factor: !0u64,
            offset: 0,
            buffer: Vec::new(),
            buffer_size: 0,
            reader_accum: 0,
            bass_shift: 0,
            samples_per_sec: 44100,
            clocks_per_sec: 0,
            bass_freq_: 16,
            length_: 0,
        }
    }

    pub fn set_sample_rate(&mut self, new_rate: i64, msec_length: Option<i64>) {
        // Max usable size (avoids overflow in fixed-point time math)
        let max_size =
            ((u32::MAX >> BLIP_BUFFER_ACCURACY) as usize) + 1 - WIDEST_IMPULSE - 64;
        let new_size = if let Some(msec) = msec_length {
            let s = ((new_rate * (msec + 1) + 999) / 1000) as usize;
            assert!(s <= max_size, "requested buffer length exceeds limit");
            s
        } else {
            max_size
        };

        if self.buffer_size != new_size {
            // count_clocks_extra = 2
            self.buffer = vec![0u16; new_size + WIDEST_IMPULSE + 2];
            self.buffer_size = 0;
            self.offset = 0;
        }

        self.buffer_size = new_size;
        self.length_ = (new_size as i64 * 1000 / new_rate - 1) as i32;
        self.samples_per_sec = new_rate;
        if self.clocks_per_sec != 0 {
            self.clock_rate(self.clocks_per_sec);
        }
        self.bass_freq(self.bass_freq_);
        self.clear(true);
    }

    pub fn clock_rate(&mut self, cps: i64) {
        self.clocks_per_sec = cps;
        self.factor = self.compute_factor(cps);
    }

    fn compute_factor(&self, clock_rate: i64) -> u64 {
        ((self.samples_per_sec as f64) / (clock_rate as f64)
            * ((1u64 << BLIP_BUFFER_ACCURACY) as f64)
            + 0.5) as u64
    }

    pub fn bass_freq(&mut self, freq: i32) {
        self.bass_freq_ = freq;
        if freq == 0 {
            self.bass_shift = 31; // 32+ is UB in C++ shifts
            return;
        }
        let s = 1.0 + (1.442695041
            * (0.124 * self.samples_per_sec as f64 / freq as f64).ln())
        .floor();
        self.bass_shift = s.clamp(0.0, 24.0) as i32;
    }

    pub fn clear(&mut self, entire_buffer: bool) {
        let count = if entire_buffer {
            self.buffer_size
        } else {
            self.samples_avail() as usize
        };
        self.offset = 0;
        self.reader_accum = 0;
        let fill_len = count + WIDEST_IMPULSE;
        for v in &mut self.buffer[..fill_len] {
            *v = SAMPLE_OFFSET as u16;
        }
    }

    /// Advance time by `t` source clocks (makes samples available).
    #[inline]
    pub fn end_frame(&mut self, t: i64) {
        self.offset += t as u64 * self.factor;
    }

    pub fn samples_avail(&self) -> i64 {
        (self.offset >> BLIP_BUFFER_ACCURACY) as i64
    }

    pub fn remove_silence(&mut self, count: i64) {
        self.offset -= (count as u64) << BLIP_BUFFER_ACCURACY;
    }

    pub fn remove_samples(&mut self, count: usize) {
        if count == 0 {
            return;
        }
        self.remove_silence(count as i64);
        let copy_extra = 1;
        let remain = self.samples_avail() as usize + WIDEST_IMPULSE + copy_extra;
        // copy_within handles both overlapping and non-overlapping cases
        self.buffer.copy_within(count..count + remain, 0);
        let fill_start = remain;
        for v in &mut self.buffer[fill_start..fill_start + count] {
            *v = SAMPLE_OFFSET as u16;
        }
    }

    /// Convert source clock time to resampled time.
    #[inline]
    pub fn resampled_time(&self, t: i64) -> u64 {
        t as u64 * self.factor + self.offset
    }

    #[inline]
    pub fn resampled_duration(&self, t: i64) -> u64 {
        t as u64 * self.factor
    }

    pub fn output_latency() -> usize {
        WIDEST_IMPULSE / 2
    }

    /// Read up to `max_samples` mono samples into `dest`.
    /// Returns number of samples read.
    pub fn read_samples(&mut self, dest: &mut [i16], max_samples: usize) -> usize {
        let count = (self.samples_avail() as usize).min(max_samples).min(dest.len());
        if count == 0 {
            return 0;
        }
        let bass_shift = self.bass_shift;
        let mut accum = self.reader_accum;

        for i in 0..count {
            let s = accum >> (ACCUM_FRACT as i64);
            accum -= accum >> bass_shift;
            accum += ((self.buffer[i] as i64) - SAMPLE_OFFSET) << (ACCUM_FRACT as i64);
            dest[i] = if (s as i16) as i64 != s {
                (0x7FFFi64 - (s >> 24)) as i16
            } else {
                s as i16
            };
        }

        self.reader_accum = accum;
        self.remove_samples(count);
        count
    }

}

impl Default for BlipBuffer {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// BlipSynth — band-limited transition synthesizer
// ---------------------------------------------------------------------------

/// Internal impulse table for BlipSynth.
///
/// Layout of `data` (u16 elements):
///   [0,  width*BLIP_RES*2)      — phase table (64 phases × width u16 each)
///   [width*64, width*64+base_n) — base impulse (17 phases × width u16 each)
///
/// where base_n = width * (BLIP_RES/2 + 1) = width * 17.
struct Impulse {
    data: Vec<u16>,    // u16 flat storage (see layout above)
    width: usize,      // impulse width in samples
    fine_bits: i32,    // 0 for normal mode, >0 for fine mode
    res: usize,        // resolution = BLIP_RES = 32
    generate: bool,    // true → need to run treble_eq before volume_unit
    volume_unit_: f64, // current volume_unit (-1 = not set)
    eq: BlipEq,
    /// Packed DC offset for subtraction in offset_resampled.
    /// = 0x10001 * floor(volume_unit * 0x10000 + 0.5)
    pub offset: u32,
}

fn fine_bits_for(abs_range: i32) -> i32 {
    if abs_range <= 64 { 2 }
    else if abs_range <= 128 { 3 }
    else if abs_range <= 256 { 4 }
    else if abs_range <= 512 { 5 }
    else if abs_range <= 1024 { 6 }
    else if abs_range <= 2048 { 7 }
    else { 8 }
}

impl Impulse {
    fn new(width: usize, abs_range: i32, fine_mode: bool) -> Self {
        let res = BLIP_RES;
        let fine_bits = if fine_mode { fine_bits_for(abs_range) } else { 0 };
        let impulse_size = width / 2 * (if fine_mode { 2 } else { 1 });
        let base_size = width / 2 * (res / 2 + 1);
        let total_u32 = impulse_size * res * 2 + base_size;
        // Each u32 = 2 u16 values in the flat data array.
        let data = vec![0u16; total_u32 * 2];
        Impulse {
            data,
            width,
            fine_bits,
            res,
            generate: true,
            volume_unit_: -1.0,
            eq: BlipEq::new(-8.87), // placeholder; will be overwritten
            offset: 0,
        }
    }

    /// u16 index of base impulse start.
    #[inline]
    fn base_start(&self) -> usize {
        // width * res * 2 u16 entries for the phase table
        self.width * self.res * 2
    }

    /// Read two consecutive u16s as a packed u32 (little-endian pair).
    #[inline]
    fn read_pair(data: &[u16], u16_idx: usize) -> u32 {
        (data[u16_idx] as u32) | ((data[u16_idx + 1] as u32) << 16)
    }

    /// Write a packed u32 as two consecutive u16s.
    #[inline]
    fn write_pair(data: &mut [u16], u16_idx: usize, val: u32) {
        data[u16_idx]     = (val & 0xFFFF) as u16;
        data[u16_idx + 1] = (val >> 16)    as u16;
    }

    /// Set treble EQ and recompute the base impulse via DSF synthesis.
    /// Mirrors Blip_Impulse_::treble_eq().
    pub fn treble_eq(&mut self, new_eq: &BlipEq) {
        if !self.generate
            && new_eq.treble == self.eq.treble
            && new_eq.cutoff == self.eq.cutoff
            && new_eq.sample_rate == self.eq.sample_rate
        {
            return; // already up to date
        }
        self.generate = false;
        self.eq = new_eq.clone();

        let mut treble = (10.0f64).powf(self.eq.treble / 20.0);
        if treble < 0.000005 { treble = 0.000005; }

        let treble_freq = 22050.0f64;
        let sample_rate = self.eq.sample_rate as f64;
        let pt = treble_freq * 2.0 / sample_rate;
        let mut cutoff = self.eq.cutoff as f64 * 2.0 / sample_rate;
        if cutoff >= pt * 0.95 || cutoff >= 0.95 {
            cutoff = 0.5;
            treble = 1.0;
        }

        let n_harm = 4096.0f64;
        let rolloff = treble.powf(1.0 / (n_harm * pt - n_harm * cutoff));
        let rescale = 1.0 / rolloff.powf(n_harm * cutoff);

        let pow_a_n  = rescale * rolloff.powf(n_harm);
        let pow_a_nc = rescale * rolloff.powf(n_harm * cutoff);

        let to_angle = std::f64::consts::PI / 2.0 / n_harm / MAX_RES as f64;

        // DSF synthesis into float buffer
        let size = MAX_RES * (self.width - 2) / 2;
        let mut fbuf = vec![0.0f32; MAX_RES * (WIDEST_IMPULSE - 2) / 2];
        let mut total = 0.0f64;

        for i in (0..size).rev() {
            let angle = (i * 2 + 1) as f64 * to_angle;
            let cos_angle    = angle.cos();
            let cos_nc_angle = (n_harm * cutoff * angle).cos();
            let cos_nc1      = ((n_harm * cutoff - 1.0) * angle).cos();

            let b = 2.0 - 2.0 * cos_angle;
            let a = 1.0 - cos_angle - cos_nc_angle + cos_nc1;

            let d = 1.0 + rolloff * (rolloff - 2.0 * cos_angle);
            let c = pow_a_n  * rolloff * ((n_harm - 1.0) * angle).cos()
                  - pow_a_n  *            (n_harm        * angle).cos()
                  - pow_a_nc * rolloff * cos_nc1
                  + pow_a_nc *           cos_nc_angle;

            let mut y = (a * d + c * b) / (b * d);

            if self.width > 12 {
                let window = (n_harm / 1.25 / WIDEST_IMPULSE as f64 * angle).cos();
                y *= window * window;
            }

            total += y as f32 as f64; // match C++ which does total += (float)y
            fbuf[i] = y as f32;
        }

        let factor = IMPULSE_AMP as f64 * 0.5 / total;

        // Integrate into the base impulse table
        let base = self.base_start();
        let step = MAX_RES / self.res;          // = 1
        let mut off = if self.res > 1 { MAX_RES } else { MAX_RES / 2 };
        let mut imp_pos = base;

        let width = self.width;
        for _ in 0..=(self.res / 2) {            // res/2 + 1 = 17 iterations
            for w in (-(width as i32 / 2))..(width as i32 / 2) {
                let mut sum = 0.0f64;
                for i in (0..MAX_RES).rev() {
                    let mut index = w * MAX_RES as i32 + off as i32 + i as i32;
                    if index < 0 { index = -index - 1; }
                    if (index as usize) < size {
                        sum += fbuf[index as usize] as f64;
                    }
                }
                self.data[imp_pos] =
                    (sum * factor + IMPULSE_OFFSET as f64 + 0.5).floor() as u16;
                imp_pos += 1;
            }
            off = off.wrapping_sub(step);
        }

        // Re-apply volume if it was already set
        if self.volume_unit_ >= 0.0 {
            let unit = self.volume_unit_;
            self.volume_unit_ = -1.0;
            self.volume_unit(unit);
        }
    }

    /// Scale the base impulse by `unit` into the phase table.
    /// Mirrors Blip_Impulse_::scale_impulse() → writes to impulses (phase table).
    fn scale_impulse(&mut self, unit: i32) {
        // offset_val = unit<<15 - 16384*unit + 16384 = unit*16384 + 16384
        let offset_val: i64 = ((unit as i64) << IMPULSE_BITS)
            - IMPULSE_OFFSET * unit as i64
            + (1 << (IMPULSE_BITS - 1));

        let base   = self.base_start();
        let width  = self.width;
        let res    = self.res;

        // First pass: write res/2+1 = 17 phases (width u16 each)
        let mut imp_pos = 0usize;
        let mut fimp    = base;
        for _ in 0..=(res / 2) {
            let mut error = unit as i64;
            for nn in 0..width {
                let a = ((self.data[fimp + nn] as i64 * unit as i64 + offset_val)
                    >> IMPULSE_BITS) as i32;
                error -= (a - unit) as i64;
                self.data[imp_pos + nn] = a as u16;
            }
            // Add rounding error to middle sample of this phase
            let mid = width / 2;
            let mid_idx = if imp_pos + mid > 0 { imp_pos + mid - 1 } else { 0 };
            let cur = self.data[mid_idx] as i64 + error;
            self.data[mid_idx] = cur as u16;
            imp_pos += width;
            fimp    += width;
        }

        // Mirror second half if res > 2
        if res > 2 {
            // C++: const imp_t* rev = imp - width - 1;  (rev points one past last written phase)
            //      *imp++ = *--rev  (pre-decrement before reading, so first read is at rev-1)
            // After first pass imp_pos = (res/2+1)*width.
            // First read in C++ is at: imp_pos - width - 1 - 1 = imp_pos - width - 2.
            let mut rev2 = imp_pos.wrapping_sub(width + 2);

            let count = (res / 2 - 1) * width - 1;
            for _ in 0..count {
                self.data[imp_pos] = self.data[rev2];
                imp_pos += 1;
                rev2     = rev2.wrapping_sub(1);
            }
            self.data[imp_pos] = unit as u16;
            imp_pos += 1;
        }
        // imp_pos is now at res*width

        // Copy to odd-offset table
        self.data[imp_pos] = unit as u16;
        imp_pos += 1;
        // memcpy(imp, imp_in, (res*width - 1) * sizeof u16)
        let copy_len = res * width - 1;
        self.data.copy_within(0..copy_len, imp_pos);
    }

    /// Set volume unit and rebuild phase table.
    /// Mirrors Blip_Impulse_::volume_unit().
    pub fn volume_unit(&mut self, new_unit: f64) {
        if new_unit == self.volume_unit_ {
            return;
        }
        if self.generate {
            // Need to compute treble_eq first with defaults
            let eq = BlipEq::new_full(-8.87, 8800, 44100);
            self.treble_eq(&eq);
        }
        self.volume_unit_ = new_unit;
        let packed =
            (0x10001u64 * (new_unit * 65536.0_f64 + 0.5).floor() as u64) as u32;
        self.offset = packed;

        if self.fine_bits != 0 {
            self.fine_volume_unit();
        } else {
            let unit = (self.offset & 0xFFFF) as i32;
            self.scale_impulse(unit);
        }
    }

    /// Fine-mode volume unit (not used by PAPU since range=210 ≤ 512).
    fn fine_volume_unit(&mut self) {
        // C++: uses stack temp, scales two tables, merges them interleaved.
        // PAPU only uses range=210 so fine_mode=false; this path is never taken.
        // Implement as a simple "scale twice and merge" using a temp vec.
        let fine_bits = self.fine_bits;
        let unit_lo = (self.offset & 0xFFFF) as i32;
        let unit_hi = ((unit_lo as i64) << fine_bits) as i32;

        let width = self.width;
        let res   = self.res;
        let phase_entries = width * res * 2; // u16 entries in one phase table

        // Scale unit_hi into temp array
        let mut temp = vec![0u16; phase_entries + width];
        // Use a scratch Impulse pointing at temp for scale_impulse
        // C++ does: scale_impulse((offset & 0xffff) << fine_bits, temp)
        // scale_impulse is const in C++ — it reads from self.data[base..] and writes to the given pointer.
        // We implement by calling our method that writes to self.data[0..], then copy out to temp.
        let saved = self.data[..phase_entries].to_vec();
        self.scale_impulse(unit_hi);
        temp[..phase_entries].copy_from_slice(&self.data[..phase_entries]);

        // Now scale unit_lo into self.data[..phase_entries] — writes second table
        // C++: scale_impulse(offset & 0xffff, impulses + res*2*width)
        // We write to the second half of the data array (starting at phase_entries)
        // by temporarily rotating data so scale writes start there.
        // Simpler: save, scale unit_lo, that's the imp2 table.
        self.scale_impulse(unit_lo);

        // Merge: interleave imp2 (self.data) and temp pairs
        // C++: for n = res/2*2*width: write imp2[0,1], src2[0,1], repeat
        let mut merged = vec![0u16; phase_entries];
        let mut pi = 0; // phase index
        let mut si = 0; // src2 index
        let n = res / 2 * 2 * width;
        let mut dst = 0;
        let mut i = 0;
        while i < n {
            merged[dst]     = self.data[pi];
            merged[dst + 1] = self.data[pi + 1];
            merged[dst + 2] = temp[si];
            merged[dst + 3] = temp[si + 1];
            pi  += 2;
            si  += 2;
            dst += 4;
            i   += 1;
        }
        self.data[..phase_entries].copy_from_slice(&merged);
        drop(saved); // original data was overwritten by scale operations
    }
}

// ---------------------------------------------------------------------------
// BlipSynth — public API
// ---------------------------------------------------------------------------

pub struct BlipSynth {
    imp: Impulse,
    abs_range: i32,
}

impl BlipSynth {
    /// Create a synth with given quality (1–5) and abs_range.
    pub fn new(quality: usize, abs_range: i32) -> Self {
        assert!((1..=5).contains(&quality));
        let width = if quality < 5 { quality * 4 } else { WIDEST_IMPULSE };
        let fine_mode = abs_range > 512; // range is always positive in PAPU
        BlipSynth {
            imp: Impulse::new(width, abs_range, fine_mode),
            abs_range,
        }
    }

    /// Set treble EQ.
    pub fn treble_eq(&mut self, eq: &BlipEq) {
        self.imp.treble_eq(eq);
    }

    /// Set volume (v / abs_range = volume_unit).
    pub fn volume(&mut self, v: f64) {
        self.imp.volume_unit(v / self.abs_range as f64);
    }

    /// Set volume_unit directly.
    pub fn volume_unit(&mut self, unit: f64) {
        self.imp.volume_unit(unit);
    }

    /// Add a band-limited transition of amplitude `delta` at clock time `t`
    /// into `buf`.
    pub fn offset(&self, t: i64, delta: i32, buf: &mut BlipBuffer) {
        let rt = t as u64 * buf.factor + buf.offset;
        self.offset_resampled(rt, delta, buf);
    }

    /// Add a transition at resampled time `time` (pre-multiplied by buf.factor).
    pub fn offset_resampled(&self, time: u64, delta: i32, buf: &mut BlipBuffer) {
        let width = self.imp.width;
        let res   = self.imp.res;
        // Width of widest impulse minus our width, divided by 2, gives the
        // buffer offset from the sample position.
        let const_offset = WIDEST_IMPULSE / 2 - width / 2;

        // Integer sample index (rounded down to even).
        let sample_index = ((time >> BLIP_BUFFER_ACCURACY) as usize) & !1usize;

        // Sub-sample phase: (time >> (BLIP_BUFFER_ACCURACY - BLIP_RES_BITS)) & mask
        let shift = BLIP_BUFFER_ACCURACY - BLIP_RES_BITS as u32;
        let mask  = (res * 2 - 1) as u64;
        let phase = ((time >> shift) & mask) as usize;

        // Impulse phase data starts at u16 index: phase * width
        let imp_start = phase * width;

        // Buffer position: const_offset + sample_index (u16 index)
        let buf_start = const_offset + sample_index;

        // DC bias to subtract (packed u32)
        let dc_offset: u32 = self.imp.offset.wrapping_mul(delta as u32);

        if self.imp.fine_bits == 0 {
            // Normal mode: width/4 iterations, 4 u16 (= 2 pairs) per iteration
            for n in 0..(width / 4) {
                let bi = buf_start  + n * 4;
                let ii = imp_start  + n * 4;

                let t0 = Impulse::read_pair(&buf.buffer, bi)
                    .wrapping_sub(dc_offset)
                    .wrapping_add(
                        Impulse::read_pair(&self.imp.data, ii)
                            .wrapping_mul(delta as u32),
                    );
                let t1 = Impulse::read_pair(&buf.buffer, bi + 2)
                    .wrapping_sub(dc_offset)
                    .wrapping_add(
                        Impulse::read_pair(&self.imp.data, ii + 2)
                            .wrapping_mul(delta as u32),
                    );

                Impulse::write_pair(&mut buf.buffer, bi,     t0);
                Impulse::write_pair(&mut buf.buffer, bi + 2, t1);
            }
        } else {
            // Fine mode (not used by PAPU, but implemented)
            let fine_bits = self.imp.fine_bits;
            let sub_range: i32 = 1 << fine_bits;
            let mut d = delta + sub_range / 2;
            let delta2 = (d & (sub_range - 1)) - sub_range / 2;
            d >>= fine_bits;

            let impulse_size = width / 2 * 2; // fine mode: double entries per phase

            for n in 0..(width / 4) {
                let bi = buf_start + n * 4;
                let ii = imp_start + n * 8; // 4 entries per iteration in fine mode

                let t0 = Impulse::read_pair(&buf.buffer, bi)
                    .wrapping_sub(dc_offset)
                    .wrapping_add(
                        Impulse::read_pair(&self.imp.data, ii)
                            .wrapping_mul(delta2 as u32),
                    )
                    .wrapping_add(
                        Impulse::read_pair(&self.imp.data, ii + 2)
                            .wrapping_mul(d as u32),
                    );
                let t1 = Impulse::read_pair(&buf.buffer, bi + 2)
                    .wrapping_sub(dc_offset)
                    .wrapping_add(
                        Impulse::read_pair(&self.imp.data, ii + 4)
                            .wrapping_mul(delta2 as u32),
                    )
                    .wrapping_add(
                        Impulse::read_pair(&self.imp.data, ii + 6)
                            .wrapping_mul(d as u32),
                    );

                Impulse::write_pair(&mut buf.buffer, bi,     t0);
                Impulse::write_pair(&mut buf.buffer, bi + 2, t1);
            }
            let _ = impulse_size; // suppress unused warning
        }
    }
}

// ---------------------------------------------------------------------------
// BlipReader — used in StereoBuffer mixing
// ---------------------------------------------------------------------------

struct BlipReader<'a> {
    buf: &'a [u16],
    pos: usize,
    accum: i64,
}

impl<'a> BlipReader<'a> {
    fn begin(blip: &'a BlipBuffer) -> (Self, i32) {
        (
            BlipReader { buf: &blip.buffer, pos: 0, accum: blip.reader_accum },
            blip.bass_shift,
        )
    }

    #[inline]
    fn read(&self) -> i32 {
        (self.accum >> ACCUM_FRACT as i64) as i32
    }

    #[inline]
    fn next(&mut self, bass_shift: i32) {
        self.accum -= self.accum >> bass_shift;
        self.accum += ((self.buf[self.pos] as i64) - SAMPLE_OFFSET) << (ACCUM_FRACT as i64);
        self.pos += 1;
    }

    fn end_accum(self) -> i64 {
        self.accum
    }
}

// ---------------------------------------------------------------------------
// StereoBuffer — three Blip_Buffers mixed to stereo
// ---------------------------------------------------------------------------

/// Stereo output buffer: center, left, right channels mixed as L=center+left,
/// R=center+right.  Matches Stereo_Buffer in Multi_Buffer.h/.cpp.
pub struct StereoBuffer {
    pub bufs: [BlipBuffer; 3], // 0=center, 1=left, 2=right
    stereo_added: bool,
    was_stereo: bool,
}

impl StereoBuffer {
    pub fn new() -> Self {
        StereoBuffer {
            bufs: [BlipBuffer::new(), BlipBuffer::new(), BlipBuffer::new()],
            stereo_added: false,
            was_stereo: false,
        }
    }

    pub fn center(&mut self) -> &mut BlipBuffer { &mut self.bufs[0] }
    pub fn left(&mut self)   -> &mut BlipBuffer { &mut self.bufs[1] }
    pub fn right(&mut self)  -> &mut BlipBuffer { &mut self.bufs[2] }

    pub fn set_sample_rate(&mut self, rate: i64) {
        for b in &mut self.bufs { b.set_sample_rate(rate, None); }
    }

    pub fn clock_rate(&mut self, rate: i64) {
        for b in &mut self.bufs { b.clock_rate(rate); }
    }

    pub fn bass_freq(&mut self, freq: i32) {
        for b in &mut self.bufs { b.bass_freq(freq); }
    }

    pub fn clear(&mut self) {
        self.stereo_added = false;
        self.was_stereo = false;
        for b in &mut self.bufs { b.clear(true); }
    }

    pub fn end_frame(&mut self, clock_count: i64, added_stereo: bool) {
        for b in &mut self.bufs { b.end_frame(clock_count); }
        self.stereo_added |= added_stereo;
    }

    pub fn samples_avail(&self) -> i64 {
        self.bufs[0].samples_avail()
    }

    /// Read up to `count` stereo pairs into `out` (interleaved L,R).
    /// Returns number of stereo pairs read.
    pub fn read_samples(&mut self, out: &mut [i16], count: usize) -> usize {
        let avail = self.bufs[0].samples_avail() as usize;
        let count = count.min(avail);
        if count == 0 {
            return 0;
        }

        if self.stereo_added || self.was_stereo {
            self.mix_stereo(out, count);
            self.bufs[0].remove_samples(count);
            self.bufs[1].remove_samples(count);
            self.bufs[2].remove_samples(count);
        } else {
            self.mix_mono(out, count);
            self.bufs[0].remove_samples(count);
            self.bufs[1].remove_silence(count as i64);
            self.bufs[2].remove_silence(count as i64);
        }

        if self.bufs[0].samples_avail() == 0 {
            self.was_stereo = self.stereo_added;
            self.stereo_added = false;
        }

        count
    }

    fn mix_stereo(&mut self, out: &mut [i16], count: usize) {
        // Split borrows: borrow all three bufs independently.
        // (Rust won't let us borrow the array elements separately from a method,
        // so we split the slice manually.)
        let (center_b, rest) = self.bufs.split_at_mut(1);
        let (left_b,  right_b) = rest.split_at_mut(1);

        let (mut left,  _bass)  = BlipReader::begin(&left_b[0]);
        let (mut right, _bass2) = BlipReader::begin(&right_b[0]);
        let (mut center, _bass3) = BlipReader::begin(&center_b[0]);
        // Use center's bass_shift (all three share the same setting)
        // Re-read bass_shift from center buffer
        let bass_shift = center_b[0].bass_shift;

        for i in 0..count {
            let c = center.read();
            let l = c as i64 + left.read() as i64;
            let r = c as i64 + right.read() as i64;
            center.next(bass_shift);

            let li = i * 2;
            out[li] = if (l as i16) as i64 != l {
                (0x7FFFi64 - (l >> 24)) as i16
            } else {
                l as i16
            };
            out[li + 1] = if (r as i16) as i64 != r {
                (0x7FFFi64 - (r >> 24)) as i16
            } else {
                r as i16
            };

            left.next(bass_shift);
            right.next(bass_shift);
        }

        // Consume readers to release their immutable borrows, then write accums back.
        let c_accum = center.end_accum();
        let r_accum = right.end_accum();
        let l_accum = left.end_accum();
        center_b[0].reader_accum = c_accum;
        right_b[0].reader_accum  = r_accum;
        left_b[0].reader_accum   = l_accum;
    }

    fn mix_mono(&mut self, out: &mut [i16], count: usize) {
        let (mut reader, bass_shift) = BlipReader::begin(&self.bufs[0]);

        for i in 0..count {
            let s = reader.read() as i64;
            reader.next(bass_shift);
            let li = i * 2;
            let samp = if (s as i16) as i64 != s {
                (0x7FFFi64 - (s >> 24)) as i16
            } else {
                s as i16
            };
            out[li]     = samp;
            out[li + 1] = samp;
        }

        self.bufs[0].reader_accum = reader.end_accum();
    }
}

impl Default for StereoBuffer {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blip_buffer_silence() {
        let mut buf = BlipBuffer::new();
        buf.clock_rate(4194304);
        buf.set_sample_rate(44100, None);
        buf.bass_freq(461);
        buf.end_frame(1024);
        let avail = buf.samples_avail() as usize;
        let mut out = vec![0i16; avail];
        buf.read_samples(&mut out, avail);
        assert!(out.iter().all(|&s| s == 0), "silent buffer should produce zero output");
    }
}
