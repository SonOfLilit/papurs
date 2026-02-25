// VST2 plugin shell wrapping PapuProcessor.
// Uses the `vst` 0.4 crate (VST 2.4 API).
//
// Phase 14: VST Plugin Shell

#![allow(deprecated)] // vst crate marks Plugin trait deprecated

use std::sync::Arc;

use vst::prelude::*;
use vst::util::AtomicFloat;
use vst::plugin_main;

use crate::engine::{MidiEvent, MidiKind, Params, PapuProcessor};

// ---------------------------------------------------------------------------
// Parameter table — mirrors PAPUAudioProcessor::addExtParam calls (39 params)
// ---------------------------------------------------------------------------

pub const PARAM_COUNT: usize = 39;

pub struct ParamDef {
    pub id:      &'static str,
    pub name:    &'static str,
    pub min:     f32,
    pub max:     f32,
    pub default: f32,
}

impl ParamDef {
    const fn new(id: &'static str, name: &'static str,
                 min: f32, max: f32, default: f32) -> Self {
        ParamDef { id, name, min, max, default }
    }

    pub fn to_normalized(&self, actual: f32) -> f32 {
        let range = self.max - self.min;
        if range.abs() < f32::EPSILON { return 0.0; }
        ((actual - self.min) / range).clamp(0.0, 1.0)
    }

    pub fn to_actual(&self, norm: f32) -> f32 {
        self.min + norm.clamp(0.0, 1.0) * (self.max - self.min)
    }

    pub fn to_actual_int(&self, norm: f32) -> i32 {
        self.to_actual(norm).round() as i32
    }

    pub fn default_normalized(&self) -> f32 {
        self.to_normalized(self.default)
    }
}

/// All 39 plugin parameters, in order, matching the C++ PAPUAudioProcessor.
pub static PARAM_DEFS: [ParamDef; PARAM_COUNT] = [
    // ── Pulse 1 (0-10) ───────────────────────────────────────────────────
    ParamDef::new("OL1",          "Pulse 1 OL",          0.0,   1.0,    1.0),
    ParamDef::new("OR1",          "Pulse 1 OR",          0.0,   1.0,    1.0),
    ParamDef::new("duty1",        "Pulse 1 Duty",        0.0,   3.0,    0.0),
    ParamDef::new("A1",           "Pulse 1 A",           0.0,   7.0,    1.0),
    ParamDef::new("R1",           "Pulse 1 R",           0.0,   7.0,    1.0),
    ParamDef::new("tune1",        "Pulse 1 Tune",      -48.0,  48.0,    0.0),
    ParamDef::new("fine1",        "Pulse 1 Fine",     -100.0, 100.0,    0.0),
    ParamDef::new("sweep1",       "Pulse 1 Sweep",      -7.0,   7.0,    0.0),
    ParamDef::new("shift1",       "Pulse 1 Shift",       0.0,   7.0,    0.0),
    ParamDef::new("rate1",        "Pulse 1 VibRate",     0.0,  15.0,    5.0),
    ParamDef::new("amt1",         "Pulse 1 VibAmt",      0.0, 100.0,    0.0),
    // ── Pulse 2 (11-19) ──────────────────────────────────────────────────
    ParamDef::new("OL2",          "Pulse 2 OL",          0.0,   1.0,    0.0),
    ParamDef::new("OR2",          "Pulse 2 OR",          0.0,   1.0,    0.0),
    ParamDef::new("duty2",        "Pulse 2 Duty",        0.0,   3.0,    0.0),
    ParamDef::new("A2",           "Pulse 2 A",           0.0,   7.0,    1.0),
    ParamDef::new("R2",           "Pulse 2 R",           0.0,   7.0,    1.0),
    ParamDef::new("tune2",        "Pulse 2 Tune",      -48.0,  48.0,    0.0),
    ParamDef::new("fine2",        "Pulse 2 Fine",     -100.0, 100.0,    0.0),
    ParamDef::new("rate2",        "Pulse 2 VibRate",     0.0,  15.0,    5.0),
    ParamDef::new("amt2",         "Pulse 2 VibAmt",      0.0, 100.0,    0.0),
    // ── Noise (20-26) ────────────────────────────────────────────────────
    ParamDef::new("OLN",          "Noise OL",            0.0,   1.0,    0.0),
    ParamDef::new("ORL",          "Noise OR",            0.0,   1.0,    0.0),
    ParamDef::new("AN",           "Noise A",             0.0,   7.0,    1.0),
    ParamDef::new("AR",           "Noise R",             0.0,   7.0,    1.0),
    ParamDef::new("shiftN",       "Noise Shift",         0.0,  13.0,    0.0),
    ParamDef::new("stepN",        "Noise Step",          0.0,   1.0,    0.0),
    ParamDef::new("ratioN",       "Noise Ratio",         0.0,   7.0,    0.0),
    // ── Wave (27-33) ─────────────────────────────────────────────────────
    ParamDef::new("OLW",          "Wave OL",             0.0,   1.0,    0.0),
    ParamDef::new("ORW",          "Wave OR",             0.0,   1.0,    0.0),
    ParamDef::new("waveform",     "Waveform",            0.0,  14.0,    0.0),
    ParamDef::new("tunewave",     "Wave Tune",         -48.0,  48.0,    0.0),
    ParamDef::new("finewave",     "Wave Fine",        -100.0, 100.0,    0.0),
    ParamDef::new("ratewave",     "Wave VibRate",        0.0,  15.0,    5.0),
    ParamDef::new("amtwave",      "Wave VibAmt",         0.0, 100.0,    0.0),
    // ── Global (34-38) ───────────────────────────────────────────────────
    ParamDef::new("channelsplit", "Channel Split",       0.0,   1.0,    0.0),
    ParamDef::new("trebeq",       "Treble EQ",         -50.0,  50.0,  -30.0),
    ParamDef::new("bassf",        "Bass Frequency",     15.0, 600.0,  461.0),
    ParamDef::new("output",       "Output",              0.0,   7.0,    7.0),
    ParamDef::new("param",        "Voices",              1.0,   8.0,    1.0),
];

// ---------------------------------------------------------------------------
// PapuPluginParams — shared between audio and UI threads via Arc
// ---------------------------------------------------------------------------

pub struct PapuPluginParams {
    values: Box<[AtomicFloat; PARAM_COUNT]>,
}

impl PapuPluginParams {
    pub fn new() -> Self {
        let values = Box::new(std::array::from_fn(|i| {
            AtomicFloat::new(PARAM_DEFS[i].default_normalized())
        }));
        PapuPluginParams { values }
    }

    pub fn get_norm(&self, i: usize) -> f32 {
        self.values[i].get()
    }

    pub fn get_actual_float(&self, i: usize) -> f32 {
        PARAM_DEFS[i].to_actual(self.get_norm(i))
    }

    pub fn get_actual_int(&self, i: usize) -> i32 {
        PARAM_DEFS[i].to_actual_int(self.get_norm(i))
    }

    pub fn get_actual_bool(&self, i: usize) -> bool {
        self.get_actual_int(i) != 0
    }

    /// Build engine `Params` from the current plugin parameter values.
    pub fn to_engine_params(&self) -> Params {
        Params {
            pulse1_ol:       self.get_actual_bool(0),
            pulse1_or:       self.get_actual_bool(1),
            pulse1_duty:     self.get_actual_int(2),
            pulse1_a:        self.get_actual_int(3),
            pulse1_r:        self.get_actual_int(4),
            pulse1_tune:     self.get_actual_int(5),
            pulse1_fine:     self.get_actual_int(6),
            pulse1_sweep:    self.get_actual_int(7),
            pulse1_shift:    self.get_actual_int(8),
            pulse1_vib_rate: self.get_actual_float(9),
            pulse1_vib_amt:  self.get_actual_float(10),
            pulse2_ol:       self.get_actual_bool(11),
            pulse2_or:       self.get_actual_bool(12),
            pulse2_duty:     self.get_actual_int(13),
            pulse2_a:        self.get_actual_int(14),
            pulse2_r:        self.get_actual_int(15),
            pulse2_tune:     self.get_actual_int(16),
            pulse2_fine:     self.get_actual_int(17),
            pulse2_vib_rate: self.get_actual_float(18),
            pulse2_vib_amt:  self.get_actual_float(19),
            noise_ol:        self.get_actual_bool(20),
            noise_or:        self.get_actual_bool(21),
            noise_a:         self.get_actual_int(22),
            noise_r:         self.get_actual_int(23),
            noise_shift:     self.get_actual_int(24),
            noise_step:      self.get_actual_int(25),
            noise_ratio:     self.get_actual_int(26),
            wave_ol:         self.get_actual_bool(27),
            wave_or:         self.get_actual_bool(28),
            wave_index:      self.get_actual_int(29) as u8,
            wave_tune:       self.get_actual_int(30),
            wave_fine:       self.get_actual_int(31),
            wave_vib_rate:   self.get_actual_float(32),
            wave_vib_amt:    self.get_actual_float(33),
            channel_split:   self.get_actual_bool(34),
            treble:          self.get_actual_float(35) as f64,
            bass:            self.get_actual_int(36),
            output:          self.get_actual_int(37),
        }
    }
}

impl Default for PapuPluginParams {
    fn default() -> Self { Self::new() }
}

impl PluginParameters for PapuPluginParams {
    fn get_parameter_name(&self, index: i32) -> String {
        let i = index as usize;
        if i < PARAM_COUNT { PARAM_DEFS[i].id.to_string() }
        else { format!("Param {index}") }
    }

    fn get_parameter(&self, index: i32) -> f32 {
        let i = index as usize;
        if i < PARAM_COUNT { self.get_norm(i) } else { 0.0 }
    }

    fn set_parameter(&self, index: i32, value: f32) {
        let i = index as usize;
        if i < PARAM_COUNT {
            self.values[i].set(value.clamp(0.0, 1.0));
        }
    }

    fn get_parameter_text(&self, index: i32) -> String {
        let i = index as usize;
        if i < PARAM_COUNT { format!("{:.3}", self.get_actual_float(i)) }
        else { String::new() }
    }

    fn get_parameter_label(&self, _index: i32) -> String { String::new() }
}

// ---------------------------------------------------------------------------
// PapuPlugin — the VST plugin struct
// ---------------------------------------------------------------------------

pub struct PapuPlugin {
    params:       Arc<PapuPluginParams>,
    processor:    PapuProcessor,
    sample_rate:  f64,
    /// MIDI events queued during `process_events`, drained in `process`.
    pub pending_midi: Vec<MidiEvent>,
}

impl PapuPlugin {
    /// Push a raw 3-byte MIDI message (for use from `process_events` and tests).
    pub fn handle_raw_midi(&mut self, data: [u8; 3], delta_frames: i32) {
        let status  = data[0] & 0xF0;
        let channel = (data[0] & 0x0F) + 1; // 1-based
        let pos     = delta_frames;
        let kind = match status {
            0x90 if data[2] > 0 => Some(MidiKind::NoteOn(data[1])),
            0x80 | 0x90         => Some(MidiKind::NoteOff(data[1])),
            0xE0 => {
                let raw = (data[2] as i32) << 7 | data[1] as i32;
                Some(MidiKind::PitchBend(raw))
            }
            0xB0 if data[1] == 123 => Some(MidiKind::AllNotesOff),
            _ => None,
        };
        if let Some(kind) = kind {
            self.pending_midi.push(MidiEvent { pos, channel, kind });
        }
    }
}

impl Plugin for PapuPlugin {
    fn new(_host: HostCallback) -> Self {
        let params    = Arc::new(PapuPluginParams::new());
        let voices    = (params.get_actual_int(38) as usize).max(1);
        let mut processor = PapuProcessor::new(voices);
        processor.prepare(44_100.0);
        PapuPlugin {
            params,
            processor,
            sample_rate: 44_100.0,
            pending_midi: Vec::new(),
        }
    }

    fn get_info(&self) -> Info {
        Info {
            name:          "PAPU".to_string(),
            vendor:        "FigBug".to_string(),
            unique_id:     0x5041_5055_i32, // 'PAPU'
            category:      Category::Synth,
            inputs:        0,
            outputs:       2,
            parameters:    PARAM_COUNT as i32,
            initial_delay: 0,
            ..Info::default()
        }
    }

    fn set_sample_rate(&mut self, rate: f32) {
        self.sample_rate = rate as f64;
        let voices = (self.params.get_actual_int(38) as usize).max(1);
        self.processor = PapuProcessor::new(voices);
        self.processor.prepare(self.sample_rate);
    }

    fn process_events(&mut self, events: &vst::api::Events) {
        for event in events.events() {
            if let vst::event::Event::Midi(ev) = event {
                self.handle_raw_midi(ev.data, ev.delta_frames);
            }
        }
    }

    fn process(&mut self, buffer: &mut AudioBuffer<f32>) {
        let block_size = buffer.samples() as i32;
        let params     = self.params.to_engine_params();
        let events: Vec<MidiEvent> = self.pending_midi.drain(..).collect();

        let float_buf  = self.processor.process_block(block_size, &params, &events);

        let (_, mut outputs) = buffer.split();
        if outputs.len() >= 2 {
            let n         = block_size as usize;
            let left_out  = outputs.get_mut(0);
            let right_out = outputs.get_mut(1);
            for i in 0..n.min(left_out.len()) {
                left_out[i]  = float_buf[i];
                right_out[i] = float_buf[i + n];
            }
        }
    }

    fn get_parameter_object(&mut self) -> Arc<dyn PluginParameters> {
        Arc::clone(&self.params) as Arc<dyn PluginParameters>
    }

    fn can_do(&self, can_do: CanDo) -> Supported {
        match can_do {
            CanDo::ReceiveMidiEvent => Supported::Yes,
            _ => Supported::Maybe,
        }
    }
}

plugin_main!(PapuPlugin);
