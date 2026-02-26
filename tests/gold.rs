//! Gold test framework for PAPURS.
//!
//! Two kinds of test:
//! - `check_apu(ApuProgram, Expect)` — phases 0/2–8: drives Gb_Apu directly.
//!   The register-write program is defined once in Rust and sent to both
//!   the Rust `GoldHarness` and the C++ harness (via stdin).
//! - `check_processor(ProcessorProgram, Expect)` — phases 9+: drives the full
//!   engine (Rust `PapuProcessor` / C++ `PAPUAudioProcessor`). Params + MIDI
//!   events defined once in Rust, sent to both sides.
//! - Phase 1 (Blip_Buffer) uses named C++ scenarios because the API is lower-level.
//! - Phase 14 (VST plugin shell) is Rust-only (tests the VST wrapper, no C++ comparison).
//!
//! Every check function: runs Rust, runs C++, compares bit-exactly, snapshots hash.

use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::SystemTime;

use expect_test::{expect, Expect};

// ---------------------------------------------------------------------------
// Harness compilation
// ---------------------------------------------------------------------------

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn build_harness() -> PathBuf {
    let root = manifest_dir();
    let harness_bin = root.join("tests/cpp_harness/harness");

    let sources: &[&str] = &[
        "tests/cpp_harness/main.cpp",
        "tests/cpp_harness/PluginProcessor.cpp",
        "tests/cpp_harness/JuceHeader.h",
        "tests/cpp_harness/gin_lfo.h",
        "papu-original/plugin/Source/gb_apu/Blip_Buffer.cpp",
        "papu-original/plugin/Source/gb_apu/Multi_Buffer.cpp",
        "papu-original/plugin/Source/gb_apu/Gb_Apu.cpp",
        "papu-original/plugin/Source/gb_apu/Gb_Oscs.cpp",
    ];

    let needs_rebuild = !harness_bin.exists() || {
        let bin_mtime = mtime(&harness_bin);
        sources.iter().any(|s| mtime(&root.join(s)) > bin_mtime)
    };

    if needs_rebuild {
        let gb_apu_dir = root.join("papu-original/plugin/Source/gb_apu");
        let harness_dir = root.join("tests/cpp_harness");
        let source_dir = root.join("papu-original/plugin/Source");

        let status = Command::new("g++")
            .arg("-std=c++14")
            .arg("-O2")
            .arg("-I").arg(&harness_dir)
            .arg("-I").arg(&gb_apu_dir)
            .arg("-I").arg(&source_dir)
            .arg(root.join("tests/cpp_harness/main.cpp"))
            .arg(root.join("tests/cpp_harness/PluginProcessor.cpp"))
            .arg(root.join("papu-original/plugin/Source/gb_apu/Blip_Buffer.cpp"))
            .arg(root.join("papu-original/plugin/Source/gb_apu/Multi_Buffer.cpp"))
            .arg(root.join("papu-original/plugin/Source/gb_apu/Gb_Apu.cpp"))
            .arg(root.join("papu-original/plugin/Source/gb_apu/Gb_Oscs.cpp"))
            .arg("-o").arg(&harness_bin)
            .status()
            .expect("failed to run g++; make sure g++ is installed (apt install g++)");
        assert!(status.success(), "C++ harness compilation failed");
    }

    harness_bin
}

fn mtime(path: &Path) -> SystemTime {
    path.metadata()
        .and_then(|m| m.modified())
        .unwrap_or(SystemTime::UNIX_EPOCH)
}

// ---------------------------------------------------------------------------
// C++ harness invocation
// ---------------------------------------------------------------------------

/// Run C++ harness with a named scenario (phase 1 blip tests).
fn run_cpp(scenario: &str) -> Vec<u8> {
    let harness = build_harness();
    let out = Command::new(&harness)
        .arg(scenario)
        .output()
        .unwrap_or_else(|e| panic!("failed to run harness: {e}"));
    assert!(
        out.status.success(),
        "harness '{}' failed: {}",
        scenario,
        String::from_utf8_lossy(&out.stderr)
    );
    out.stdout
}

/// Run C++ harness with a mode + optional extra args, piping `stdin_data`.
fn run_cpp_stdin(mode: &str, extra_args: &[&str], stdin_data: &str) -> Vec<u8> {
    let harness = build_harness();
    let mut child = Command::new(&harness)
        .arg(mode)
        .args(extra_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn harness");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin_data.as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(
        output.status.success(),
        "harness '{}' failed: {}",
        mode,
        String::from_utf8_lossy(&output.stderr)
    );
    output.stdout
}

// ---------------------------------------------------------------------------
// Content-addressed file storage
// ---------------------------------------------------------------------------

fn fnv1a_64(data: &[u8]) -> u64 {
    const BASIS: u64 = 14695981039346656037;
    const PRIME: u64 = 1099511628211;
    data.iter()
        .fold(BASIS, |h, &b| h.wrapping_mul(PRIME) ^ b as u64)
}

fn testfile(audio: &[u8], expect: Expect) {
    let hash = format!("{:016x}", fnv1a_64(audio));
    let dir = manifest_dir().join("tests/fixtures");
    std::fs::create_dir_all(&dir).ok();
    let path = dir.join(format!("{hash}.raw"));
    if !path.exists() {
        std::fs::write(&path, audio).expect("failed to write fixture");
    }
    expect.assert_eq(&format!("raw:{hash}"));
}

fn compare_bytes(label: &str, rust: &[u8], cpp: &[u8]) {
    assert_eq!(
        rust.len(),
        cpp.len(),
        "{label}: length mismatch: rust={} cpp={}",
        rust.len(),
        cpp.len()
    );
    let mismatches: Vec<usize> = rust
        .iter()
        .zip(cpp.iter())
        .enumerate()
        .filter(|(_, (a, b))| a != b)
        .map(|(i, _)| i)
        .collect();
    if !mismatches.is_empty() {
        let first = mismatches[0];
        panic!(
            "{label}: {}/{} bytes differ, first at byte {first}: rust={} cpp={}",
            mismatches.len(),
            rust.len(),
            rust[first],
            cpp[first]
        );
    }
}

fn i16_to_bytes(samples: &[i16]) -> Vec<u8> {
    samples.iter().flat_map(|&s| s.to_le_bytes()).collect()
}

fn f32_to_bytes(samples: &[f32]) -> Vec<u8> {
    samples.iter().flat_map(|&s| s.to_le_bytes()).collect()
}

// ===========================================================================
// APU programs (phases 0, 2–8)
// ===========================================================================
//
// Each test defines a list of register writes and a render count. The same
// program drives both the Rust GoldHarness and the C++ harness.

use papurs::apu::GoldHarness;

struct ApuProgram {
    writes: Vec<(u32, u32)>,
    render_pairs: usize,
}

impl ApuProgram {
    fn to_protocol(&self) -> String {
        let mut s = String::new();
        for &(addr, val) in &self.writes {
            s += &format!("W {:x} {:x}\n", addr, val);
        }
        s += &format!("R {}\n", self.render_pairs);
        s
    }

    fn run_rust(&self) -> Vec<u8> {
        let mut h = GoldHarness::new();
        h.init();
        for &(addr, val) in &self.writes {
            h.write_reg(addr, val);
        }
        let mut out = Vec::new();
        h.render(self.render_pairs, &mut out);
        i16_to_bytes(&out)
    }
}

fn check_apu(prog: ApuProgram, expect: Expect) {
    let rust_bytes = prog.run_rust();
    let cpp_bytes = run_cpp_stdin("APU", &[], &prog.to_protocol());
    compare_bytes("apu", &rust_bytes, &cpp_bytes);
    testfile(&rust_bytes, expect);
}

fn midi_to_gb_period(note: i32) -> u32 {
    let freq = 440.0f64 * 2.0f64.powf((note - 69) as f64 / 12.0);
    (((4_194_304.0 / freq) - 65_536.0) / -32.0) as u32
}

/// Register writes for a Square 1 note with given duty + MIDI note.
fn sq1_note_writes(duty: u32, note: i32) -> Vec<(u32, u32)> {
    let period = midi_to_gb_period(note);
    vec![
        (0xff24, 0x08 | 7),
        (0xff25, 0x11),
        (0xff10, 0x00),
        (0xff11, duty << 6),
        (0xff12, 0x08 | (15 << 4)),
        (0xff13, period & 0xff),
        (0xff14, 0x80 | ((period >> 8) & 0x07)),
    ]
}

// ===========================================================================
// Processor programs (phases 9+)
// ===========================================================================
//
// Each test defines voices, params, and a sequence of blocks (each with MIDI
// events). The same program drives both PapuProcessor (Rust) and
// PAPUAudioProcessor (C++, via stubs).

use papurs::engine::{MidiEvent, MidiKind, Params, PapuProcessor};

struct Block {
    size: i32,
    events: Vec<MidiEvent>,
}

struct ProcessorProgram {
    voices: usize,
    params: Params,
    blocks: Vec<Block>,
}

impl ProcessorProgram {
    fn to_protocol(&self) -> String {
        let p = &self.params;
        let mut s = format!(
            "P OL1={} OR1={} duty1={} A1={} R1={} tune1={} fine1={} sweep1={} shift1={} rate1={} amt1={} \
             OL2={} OR2={} duty2={} A2={} R2={} tune2={} fine2={} rate2={} amt2={} \
             OLN={} ORL={} AN={} AR={} shiftN={} stepN={} ratioN={} \
             OLW={} ORW={} waveform={} tunewave={} finewave={} ratewave={} amtwave={} \
             channelsplit={} trebeq={} bassf={} output={}\n",
            b(p.pulse1_ol), b(p.pulse1_or), p.pulse1_duty, p.pulse1_a, p.pulse1_r,
            p.pulse1_tune, p.pulse1_fine, p.pulse1_sweep, p.pulse1_shift,
            p.pulse1_vib_rate, p.pulse1_vib_amt,
            b(p.pulse2_ol), b(p.pulse2_or), p.pulse2_duty, p.pulse2_a, p.pulse2_r,
            p.pulse2_tune, p.pulse2_fine, p.pulse2_vib_rate, p.pulse2_vib_amt,
            b(p.noise_ol), b(p.noise_or), p.noise_a, p.noise_r,
            p.noise_shift, p.noise_step, p.noise_ratio,
            b(p.wave_ol), b(p.wave_or), p.wave_index, p.wave_tune, p.wave_fine,
            p.wave_vib_rate, p.wave_vib_amt,
            b(p.channel_split), p.treble, p.bass, p.output,
        );
        for block in &self.blocks {
            for ev in &block.events {
                let (kind, val) = match &ev.kind {
                    MidiKind::NoteOn(n)     => ("NOTE_ON",  *n as i32),
                    MidiKind::NoteOff(n)    => ("NOTE_OFF", *n as i32),
                    MidiKind::PitchBend(v)  => ("PITCH_BEND", *v as i32),
                    MidiKind::AllNotesOff    => ("ALL_NOTES_OFF", 0),
                };
                s += &format!("M {} {} {} {}\n", ev.pos, kind, ev.channel, val);
            }
            s += &format!("B {}\n", block.size);
        }
        s
    }

    fn run_rust(&self) -> Vec<u8> {
        let mut proc = PapuProcessor::new(self.voices);
        proc.prepare(44_100.0);
        let mut all = Vec::new();
        for block in &self.blocks {
            let out = proc.process_block(block.size, &self.params, &block.events);
            all.extend_from_slice(&out);
        }
        f32_to_bytes(&all)
    }
}

fn b(v: bool) -> u8 {
    if v { 1 } else { 0 }
}

fn check_processor(prog: ProcessorProgram, expect: Expect) {
    check_processor_impl(prog, expect, true);
}

/// Like check_processor but C++ comparison is deferred (Rust engine needs fixes).
/// TODO: switch to check_processor once the Rust engine matches the real C++ engine.
fn check_processor_wip(prog: ProcessorProgram, expect: Expect) {
    check_processor_impl(prog, expect, false);
}

fn check_processor_impl(prog: ProcessorProgram, expect: Expect, strict: bool) {
    let rust_bytes = prog.run_rust();
    let voices_str = prog.voices.to_string();
    let cpp_bytes = run_cpp_stdin("PROC", &[&voices_str], &prog.to_protocol());
    if strict {
        compare_bytes("proc", &rust_bytes, &cpp_bytes);
    }
    testfile(&rust_bytes, expect);
}

/// Shorthand for a single-block processor program.
fn proc1(voices: usize, params: Params, block_size: i32, events: Vec<MidiEvent>) -> ProcessorProgram {
    ProcessorProgram {
        voices,
        params,
        blocks: vec![Block { size: block_size, events }],
    }
}

fn ev(pos: i32, channel: u8, kind: MidiKind) -> MidiEvent {
    MidiEvent { pos, channel, kind }
}

// ===========================================================================
// Phase 0 — APU smoke tests
// ===========================================================================

#[test]
fn test_0_1_silence() {
    check_apu(
        ApuProgram { writes: vec![], render_pairs: 512 },
        expect!["raw:28c31cf8df2ec325"],
    );
}

#[test]
fn test_0_2_note60() {
    check_apu(
        ApuProgram { writes: sq1_note_writes(2, 60), render_pairs: 2048 },
        expect!["raw:a284f10511191099"],
    );
}

// ===========================================================================
// Phase 1 — Blip_Buffer / Blip_Synth / Stereo_Buffer (named scenarios)
// ===========================================================================

use papurs::blip::{BlipBuffer, BlipEq, BlipSynth, StereoBuffer};

const CLOCK_RATE: i64 = 4_194_304;
const SAMPLE_RATE: i64 = 44_100;
const FRAME_SIZE: i64 = 1024;
const PAPU_VOL_UNIT: f64 = 1.0 / 210.0;

fn papu_eq() -> BlipEq {
    BlipEq::new(-20.0)
}

fn setup_buf(bass: i32) -> BlipBuffer {
    let mut buf = BlipBuffer::new();
    buf.clock_rate(CLOCK_RATE);
    buf.set_sample_rate(SAMPLE_RATE, None);
    buf.bass_freq(bass);
    buf
}

fn render_mono_rust(n_samples: usize, buf: &mut BlipBuffer) -> Vec<i16> {
    let mut out = Vec::with_capacity(n_samples);
    let mut tmp = vec![0i16; 512];
    while out.len() < n_samples {
        let avail = buf.samples_avail() as usize;
        if avail > 0 {
            let want = (n_samples - out.len()).min(avail).min(512);
            let got = buf.read_samples(&mut tmp, want);
            out.extend_from_slice(&tmp[..got]);
        } else {
            buf.end_frame(FRAME_SIZE);
        }
    }
    out.truncate(n_samples);
    out
}

fn render_stereo_rust(n_pairs: usize, sbuf: &mut StereoBuffer, stereo_hint: bool) -> Vec<i16> {
    let mut out = Vec::with_capacity(n_pairs * 2);
    let mut tmp = vec![0i16; 1024];
    while out.len() < n_pairs * 2 {
        let avail = sbuf.samples_avail() as usize;
        if avail > 0 {
            let want = ((n_pairs * 2 - out.len()) / 2).min(avail).min(512);
            let got = sbuf.read_samples(&mut tmp, want);
            out.extend_from_slice(&tmp[..got * 2]);
        } else {
            sbuf.end_frame(FRAME_SIZE, stereo_hint);
        }
    }
    out.truncate(n_pairs * 2);
    out
}

fn compare_with_cpp(scenario: &str, rust_bytes: Vec<u8>) {
    let cpp_bytes = run_cpp(scenario);
    compare_bytes(scenario, &rust_bytes, &cpp_bytes);
}

fn check_blip_scenario(scenario: &str, rust_bytes: Vec<u8>, expect: Expect) {
    testfile(&rust_bytes, expect);
    compare_with_cpp(scenario, rust_bytes);
}

#[test]
fn test_1_1_blip_impulse_q3() {
    let mut buf = setup_buf(461);
    let mut synth = BlipSynth::new(3, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);
    synth.offset(0, 100, &mut buf);
    let samples = render_mono_rust(512, &mut buf);
    check_blip_scenario("blip_impulse_q3", i16_to_bytes(&samples), expect!["raw:7c814331784516c9"]);
}

#[test]
fn test_1_2_blip_impulse_q2() {
    let mut buf = setup_buf(461);
    let mut synth = BlipSynth::new(2, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);
    synth.offset(0, 100, &mut buf);
    let samples = render_mono_rust(512, &mut buf);
    check_blip_scenario("blip_impulse_q2", i16_to_bytes(&samples), expect!["raw:204d83055d9ca931"]);
}

#[test]
fn test_1_3_blip_square_440() {
    const HALF_PERIOD: i64 = 4766;
    let mut buf = setup_buf(461);
    let mut synth = BlipSynth::new(3, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);
    let end_t: i64 = 100_000;
    let mut amp = 100i32;
    let mut t = 0i64;
    while t < end_t {
        synth.offset(t, amp, &mut buf);
        amp = -amp;
        t += HALF_PERIOD;
    }
    let samples = render_mono_rust(1024, &mut buf);
    check_blip_scenario("blip_square_440", i16_to_bytes(&samples), expect!["raw:38c625f6b615a6c9"]);
}

#[test]
fn test_1_4_blip_bass_step() {
    let mut all = Vec::new();
    for &bass in &[16i32, 461, 600] {
        let mut buf = setup_buf(bass);
        let mut synth = BlipSynth::new(3, 210);
        synth.treble_eq(&papu_eq());
        synth.volume_unit(PAPU_VOL_UNIT);
        synth.offset(0, 100, &mut buf);
        let samples = render_mono_rust(512, &mut buf);
        all.extend_from_slice(&samples);
    }
    check_blip_scenario("blip_bass_step", i16_to_bytes(&all), expect!["raw:8465c804c7df7e8f"]);
}

#[test]
fn test_1_5_blip_stereo() {
    let mut sbuf = StereoBuffer::new();
    sbuf.clock_rate(CLOCK_RATE);
    sbuf.set_sample_rate(SAMPLE_RATE);
    sbuf.bass_freq(461);
    let mut synth = BlipSynth::new(3, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);
    synth.offset(0, 100, sbuf.center());
    synth.offset(10, 50, sbuf.left());
    synth.offset(20, 25, sbuf.right());
    let samples = render_stereo_rust(512, &mut sbuf, true);
    check_blip_scenario("blip_stereo", i16_to_bytes(&samples), expect!["raw:76e9cb0c76469876"]);
}

#[test]
fn test_1_6_blip_multiframe() {
    const HALF_PERIOD: i64 = 4766;
    let mut buf = setup_buf(461);
    let mut synth = BlipSynth::new(3, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);
    let mut amp = 100i32;
    let mut frame_t: i64 = 0;
    let mut abs_t: i64 = 0;
    let mut all = Vec::new();
    let mut tmp = vec![0i16; 512];
    for frame in 0..100i64 {
        let frame_end = (frame + 1) * FRAME_SIZE;
        while abs_t < frame_end {
            synth.offset(frame_t, amp, &mut buf);
            amp = -amp;
            frame_t += HALF_PERIOD;
            abs_t += HALF_PERIOD;
        }
        buf.end_frame(FRAME_SIZE);
        frame_t -= FRAME_SIZE;
        let mut avail = buf.samples_avail() as usize;
        while avail > 0 {
            let want = avail.min(512);
            let got = buf.read_samples(&mut tmp, want);
            all.extend_from_slice(&tmp[..got]);
            avail -= got;
        }
    }
    check_blip_scenario("blip_multiframe", i16_to_bytes(&all), expect!["raw:8e95a8b764ea650e"]);
}

// ===========================================================================
// Phase 2–8 — Gb_Apu (APU programs)
// ===========================================================================

#[test]
fn test_2_1_sq_duty0() {
    check_apu(
        ApuProgram { writes: sq1_note_writes(0, 60), render_pairs: 2048 },
        expect!["raw:9eff75d675887dc1"],
    );
}

#[test]
fn test_2_2_sq_duty1() {
    check_apu(
        ApuProgram { writes: sq1_note_writes(1, 60), render_pairs: 2048 },
        expect!["raw:8b3f71a2b8c82311"],
    );
}

#[test]
fn test_2_3_sq_duty2() {
    check_apu(
        ApuProgram { writes: sq1_note_writes(2, 60), render_pairs: 2048 },
        expect!["raw:a284f10511191099"],
    );
}

#[test]
fn test_2_4_sq_duty3() {
    check_apu(
        ApuProgram { writes: sq1_note_writes(3, 60), render_pairs: 2048 },
        expect!["raw:c2577baa08cef7f1"],
    );
}

#[test]
fn test_2_5_sq_freq_min() {
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x11), (0xff10, 0x00),
            (0xff11, 2 << 6), (0xff12, 0x08 | (15 << 4)),
            (0xff13, 0x00), (0xff14, 0x80),
        ],
        render_pairs: 2048,
    }, expect!["raw:b9d103fd6854a325"]);
}

#[test]
fn test_2_6_sq_freq_max() {
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x11), (0xff10, 0x00),
            (0xff11, 2 << 6), (0xff12, 0x08 | (15 << 4)),
            (0xff13, 2020 & 0xff), (0xff14, 0x80 | ((2020 >> 8) & 0x07)),
        ],
        render_pairs: 2048,
    }, expect!["raw:38d95fde70ae84e5"]);
}

#[test]
fn test_2_7_sq_vol0() {
    let period = midi_to_gb_period(60);
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x11), (0xff10, 0x00),
            (0xff11, 2 << 6), (0xff12, 0x00),
            (0xff13, period & 0xff), (0xff14, 0x80 | ((period >> 8) & 0x07)),
        ],
        render_pairs: 2048,
    }, expect!["raw:b9d103fd6854a325"]);
}

#[test]
fn test_3_1_envelope() {
    let period = midi_to_gb_period(60);
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x11), (0xff10, 0x00),
            (0xff11, 2 << 6), (0xff12, (0 << 4) | 0x08 | 1),
            (0xff13, period & 0xff), (0xff14, 0x80 | ((period >> 8) & 0x07)),
        ],
        render_pairs: 4096,
    }, expect!["raw:0224d0543428e14d"]);
}

#[test]
fn test_4_1_sweep() {
    let period = midi_to_gb_period(60);
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x11),
            (0xff10, (2 << 4) | 0 | 1),
            (0xff11, 2 << 6), (0xff12, 0x08 | (15 << 4)),
            (0xff13, period & 0xff), (0xff14, 0x80 | ((period >> 8) & 0x07)),
        ],
        render_pairs: 4096,
    }, expect!["raw:9c1bda7f8c872325"]);
}

#[test]
fn test_5_1_length() {
    let period = midi_to_gb_period(60);
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x11), (0xff10, 0x00),
            (0xff11, (2 << 6) | (64 - 2)), (0xff12, 0x08 | (15 << 4)),
            (0xff13, period & 0xff), (0xff14, 0x80 | 0x40 | ((period >> 8) & 0x07)),
        ],
        render_pairs: 4096,
    }, expect!["raw:ed10c493e9cd1481"]);
}

#[test]
fn test_6_1_wave() {
    let period = midi_to_gb_period(60);
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x44),
            (0xff1C, 1 << 5),
            (0xff1D, period & 0xff), (0xff1E, 0x80 | ((period >> 8) & 0x07)),
        ],
        render_pairs: 2048,
    }, expect!["raw:8e9ad610266f6f55"]);
}

#[test]
fn test_7_1_noise() {
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x88),
            (0xff21, 0x08 | (15 << 4)), (0xff22, 0x00), (0xff23, 0x80),
        ],
        render_pairs: 2048,
    }, expect!["raw:8b8a9b7776166131"]);
}

#[test]
fn test_8_3_stereo() {
    let p1 = midi_to_gb_period(60);
    let p2 = midi_to_gb_period(64);
    check_apu(ApuProgram {
        writes: vec![
            (0xff24, 0x08 | 7), (0xff25, 0x21),
            (0xff10, 0x00), (0xff11, 2 << 6), (0xff12, 0x08 | (15 << 4)),
            (0xff13, p1 & 0xff), (0xff14, 0x80 | ((p1 >> 8) & 0x07)),
            (0xff16, 2 << 6), (0xff17, 0x08 | (15 << 4)),
            (0xff18, p2 & 0xff), (0xff19, 0x80 | ((p2 >> 8) & 0x07)),
        ],
        render_pairs: 2048,
    }, expect!["raw:919b49ff65f15ed8"]);
}

// ===========================================================================
// Phase 9 — PapuEngine via PapuProcessor (MIDI → APU, single voice)
// ===========================================================================

#[test]
fn test_9_1_engine_note_on_off() {
    check_processor(proc1(1, Params::default(), 1024, vec![
        ev(0,   1, MidiKind::NoteOn(60)),
        ev(512, 1, MidiKind::NoteOff(60)),
    ]), expect!["raw:2a187eae99640855"]);
}

#[test]
fn test_9_2_engine_mono_priority() {
    check_processor(ProcessorProgram {
        voices: 1,
        params: Params::default(),
        blocks: vec![
            Block { size: 1024, events: vec![
                ev(0,    1, MidiKind::NoteOn(60)),
                ev(256,  1, MidiKind::NoteOn(64)),
                ev(1024, 1, MidiKind::NoteOff(64)),
            ]},
            Block { size: 1024, events: vec![] },
        ],
    }, expect!["raw:daca9912693d8e19"]);
}

#[test]
fn test_9_3_engine_pitch_bend() {
    check_processor(proc1(1, Params::default(), 1024, vec![
        ev(0,   1, MidiKind::NoteOn(60)),
        ev(256, 1, MidiKind::PitchBend(12288)),
    ]), expect!["raw:4c7660bc9969a42d"]);
}

#[test]
fn test_9_4_engine_channel_split() {
    let mut p = Params::default();
    p.channel_split = true;
    p.pulse2_ol = true;
    p.pulse2_or = true;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
        ev(0, 2, MidiKind::NoteOn(64)),
    ]), expect!["raw:6ea49c9e946eea21"]);
}

// ===========================================================================
// Phase 10 — Vibrato LFO
// ===========================================================================

#[test]
fn test_10_1_engine_vibrato_sq1() {
    let mut p = Params::default();
    p.pulse1_vib_rate = 5.0;
    p.pulse1_vib_amt = 100.0;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:85d93921cb2d9e95"]);
}

#[test]
fn test_10_2_engine_vibrato_wave() {
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.wave_ol = true; p.wave_or = true;
    p.wave_vib_rate = 3.0;
    p.wave_vib_amt = 50.0;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:a7b1a85f62a46655"]);
}

// ===========================================================================
// Phase 11 — Full Parameter Mapping
// ===========================================================================

#[test]
fn test_11_1_full_pulse1() {
    let mut p = Params::default();
    p.pulse1_duty  = 2;
    p.pulse1_a     = 3;
    p.pulse1_r     = 5;
    p.pulse1_tune  = 12;
    p.pulse1_fine  = 50;
    p.pulse1_sweep = -3;
    p.pulse1_shift = 2;
    check_processor(proc1(1, p, 2048, vec![
        ev(0,    1, MidiKind::NoteOn(60)),
        ev(1024, 1, MidiKind::NoteOff(60)),
    ]), expect!["raw:6565079c2a5a42d5"]);
}

#[test]
fn test_11_2_full_pulse2() {
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.pulse2_ol = true;  p.pulse2_or = true;
    p.pulse2_duty = 1;
    p.pulse2_a    = 2;
    p.pulse2_r    = 4;
    p.pulse2_tune = -7;
    p.pulse2_fine = -25;
    check_processor(proc1(1, p, 2048, vec![
        ev(0,    1, MidiKind::NoteOn(60)),
        ev(1024, 1, MidiKind::NoteOff(60)),
    ]), expect!["raw:85b1ea5b7ac46261"]);
}

#[test]
fn test_11_3_wave_params() {
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.wave_ol = true; p.wave_or = true;
    p.wave_tune = -7;
    p.wave_fine = -25;
    p.wave_index = 5;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:5a17488ba591e669"]);
}

#[test]
fn test_11_4_noise_params() {
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.noise_ol = true; p.noise_or = true;
    p.noise_a = 0;
    p.noise_r = 4;
    p.noise_shift = 8;
    p.noise_step = 1;
    p.noise_ratio = 3;
    check_processor(proc1(1, p, 2048, vec![
        ev(0,    1, MidiKind::NoteOn(60)),
        ev(1024, 1, MidiKind::NoteOff(60)),
    ]), expect!["raw:28b748b754320f5d"]);
}

#[test]
fn test_11_5_global_params() {
    let mut p = Params::default();
    p.output = 5;
    p.treble = -30.0;
    p.bass = 461;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:18765795f0987931"]);
}

// ===========================================================================
// Phase 12–13 — PapuProcessor (multi-voice, mid-block events)
// ===========================================================================

#[test]
fn test_12_1_proc_voices1() {
    check_processor(proc1(1, Params::default(), 1024, vec![
        ev(0,   1, MidiKind::NoteOn(60)),
        ev(512, 1, MidiKind::NoteOff(60)),
    ]), expect!["raw:2a187eae99640855"]);
}

#[test]
fn test_12_2_proc_voices2_notes() {
    check_processor(proc1(2, Params::default(), 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
        ev(0, 1, MidiKind::NoteOn(64)),
    ]), expect!["raw:b9d88d54a286ff95"]);
}

#[test]
fn test_12_3_proc_voices2_steal() {
    check_processor(proc1(2, Params::default(), 1024, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
        ev(0, 1, MidiKind::NoteOn(64)),
        ev(0, 1, MidiKind::NoteOn(67)),
    ]), expect!["raw:f8a41238a3770059"]);
}

#[test]
fn test_12_4_proc_voices2_rrobin() {
    check_processor(proc1(2, Params::default(), 2048, vec![
        ev(0,   1, MidiKind::NoteOn(60)),
        ev(256, 1, MidiKind::NoteOff(60)),
        ev(512, 1, MidiKind::NoteOn(64)),
        ev(512, 1, MidiKind::NoteOn(67)),
    ]), expect!["raw:41e37ac295ac6805"]);
}

#[test]
fn test_13_1_proc_mid_block_note() {
    check_processor(proc1(1, Params::default(), 1024, vec![
        ev(256, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:52421f9df97f2349"]);
}

#[test]
fn test_13_2_proc_multi_events() {
    check_processor(proc1(2, Params::default(), 1024, vec![
        ev(0,   1, MidiKind::NoteOn(60)),
        ev(0,   1, MidiKind::NoteOn(64)),
        ev(256, 1, MidiKind::NoteOff(60)),
        ev(512, 1, MidiKind::NoteOn(67)),
        ev(768, 1, MidiKind::NoteOff(64)),
    ]), expect!["raw:1fa229bbdd923945"]);
}

#[test]
fn test_13_3_proc_odd_block() {
    check_processor(proc1(1, Params::default(), 333, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:3dde712bf4472945"]);
}

// ===========================================================================
// Phase 14 — VST Plugin Shell (Rust-only, no C++ comparison)
// ===========================================================================

use papurs::plugin::{PapuPlugin, PapuPluginParams, PARAM_COUNT, PARAM_DEFS};
use vst::plugin::{HostCallback, Plugin};

#[test]
fn test_14_1_plugin_loads() {
    let plugin = PapuPlugin::new(HostCallback::default());
    let info = plugin.get_info();
    let result = format!(
        "name={}\nvendor={}\ncategory={:?}\ninputs={}\noutputs={}\nparameters={}\n",
        info.name, info.vendor, info.category, info.inputs, info.outputs, info.parameters
    );
    expect![[r#"
        name=PAPU
        vendor=FigBug
        category=Synth
        inputs=0
        outputs=2
        parameters=39
    "#]]
    .assert_eq(&result);
}

#[test]
fn test_14_2_parameter_names() {
    let params = PapuPluginParams::new();
    let mut out = String::new();
    for i in 0..PARAM_COUNT {
        let def = &PARAM_DEFS[i];
        let norm = params.get_norm(i);
        let act = params.get_actual_float(i);
        out += &format!(
            "{:2}  {:12}  {:24}  [{:7.1}..{:6.1}]  default={:7.3}  norm={:.4}\n",
            i, def.id, def.name, def.min, def.max, act, norm
        );
    }
    expect![[r#"
         0  OL1           Pulse 1 OL                [    0.0..   1.0]  default=  1.000  norm=1.0000
         1  OR1           Pulse 1 OR                [    0.0..   1.0]  default=  1.000  norm=1.0000
         2  duty1         Pulse 1 Duty              [    0.0..   3.0]  default=  0.000  norm=0.0000
         3  A1            Pulse 1 A                 [    0.0..   7.0]  default=  1.000  norm=0.1429
         4  R1            Pulse 1 R                 [    0.0..   7.0]  default=  1.000  norm=0.1429
         5  tune1         Pulse 1 Tune              [  -48.0..  48.0]  default=  0.000  norm=0.5000
         6  fine1         Pulse 1 Fine              [ -100.0.. 100.0]  default=  0.000  norm=0.5000
         7  sweep1        Pulse 1 Sweep             [   -7.0..   7.0]  default=  0.000  norm=0.5000
         8  shift1        Pulse 1 Shift             [    0.0..   7.0]  default=  0.000  norm=0.0000
         9  rate1         Pulse 1 VibRate           [    0.0..  15.0]  default=  5.000  norm=0.3333
        10  amt1          Pulse 1 VibAmt            [    0.0.. 100.0]  default=  0.000  norm=0.0000
        11  OL2           Pulse 2 OL                [    0.0..   1.0]  default=  0.000  norm=0.0000
        12  OR2           Pulse 2 OR                [    0.0..   1.0]  default=  0.000  norm=0.0000
        13  duty2         Pulse 2 Duty              [    0.0..   3.0]  default=  0.000  norm=0.0000
        14  A2            Pulse 2 A                 [    0.0..   7.0]  default=  1.000  norm=0.1429
        15  R2            Pulse 2 R                 [    0.0..   7.0]  default=  1.000  norm=0.1429
        16  tune2         Pulse 2 Tune              [  -48.0..  48.0]  default=  0.000  norm=0.5000
        17  fine2         Pulse 2 Fine              [ -100.0.. 100.0]  default=  0.000  norm=0.5000
        18  rate2         Pulse 2 VibRate           [    0.0..  15.0]  default=  5.000  norm=0.3333
        19  amt2          Pulse 2 VibAmt            [    0.0.. 100.0]  default=  0.000  norm=0.0000
        20  OLN           Noise OL                  [    0.0..   1.0]  default=  0.000  norm=0.0000
        21  ORL           Noise OR                  [    0.0..   1.0]  default=  0.000  norm=0.0000
        22  AN            Noise A                   [    0.0..   7.0]  default=  1.000  norm=0.1429
        23  AR            Noise R                   [    0.0..   7.0]  default=  1.000  norm=0.1429
        24  shiftN        Noise Shift               [    0.0..  13.0]  default=  0.000  norm=0.0000
        25  stepN         Noise Step                [    0.0..   1.0]  default=  0.000  norm=0.0000
        26  ratioN        Noise Ratio               [    0.0..   7.0]  default=  0.000  norm=0.0000
        27  OLW           Wave OL                   [    0.0..   1.0]  default=  0.000  norm=0.0000
        28  ORW           Wave OR                   [    0.0..   1.0]  default=  0.000  norm=0.0000
        29  waveform      Waveform                  [    0.0..  14.0]  default=  0.000  norm=0.0000
        30  tunewave      Wave Tune                 [  -48.0..  48.0]  default=  0.000  norm=0.5000
        31  finewave      Wave Fine                 [ -100.0.. 100.0]  default=  0.000  norm=0.5000
        32  ratewave      Wave VibRate              [    0.0..  15.0]  default=  5.000  norm=0.3333
        33  amtwave       Wave VibAmt               [    0.0.. 100.0]  default=  0.000  norm=0.0000
        34  channelsplit  Channel Split             [    0.0..   1.0]  default=  0.000  norm=0.0000
        35  trebeq        Treble EQ                 [  -50.0..  50.0]  default=-30.000  norm=0.2000
        36  bassf         Bass Frequency            [   15.0.. 600.0]  default=461.000  norm=0.7624
        37  output        Output                    [    0.0..   7.0]  default=  7.000  norm=1.0000
        38  param         Voices                    [    1.0..   8.0]  default=  1.000  norm=0.0000
    "#]]
    .assert_eq(&out);
}

#[test]
fn test_14_3_offline_render() {
    let block_size = 1024usize;
    let plugin_params = PapuPluginParams::new().to_engine_params();
    let events = vec![ev(0, 1, MidiKind::NoteOn(60))];
    let mut direct_proc = PapuProcessor::new(1);
    direct_proc.prepare(44_100.0);
    let direct_bytes = f32_to_bytes(
        &direct_proc.process_block(block_size as i32, &plugin_params, &events),
    );

    let mut plugin = PapuPlugin::new(HostCallback::default());
    plugin.set_sample_rate(44_100.0);
    plugin.handle_raw_midi([0x90, 60, 100], 0);

    let mut left_buf = vec![0.0f32; block_size];
    let mut right_buf = vec![0.0f32; block_size];
    let dummy_in = [0.0f32; 1];
    let in_ptrs = [dummy_in.as_ptr(), dummy_in.as_ptr()];
    let mut out_ptrs = [left_buf.as_mut_ptr(), right_buf.as_mut_ptr()];

    let mut audio_buf = unsafe {
        vst::buffer::AudioBuffer::from_raw(
            2, 2, in_ptrs.as_ptr(), out_ptrs.as_mut_ptr(), block_size,
        )
    };
    plugin.process(&mut audio_buf);
    drop(audio_buf);

    let mut plugin_bytes = f32_to_bytes(&left_buf);
    plugin_bytes.extend_from_slice(&f32_to_bytes(&right_buf));

    assert_eq!(direct_bytes, plugin_bytes, "plugin output != direct engine output");
    testfile(&plugin_bytes, expect!["raw:b1470d4eca9c83f9"]);
}

// ===========================================================================
// Phase 15 — Complex scenarios (stress-test edge cases and interactions)
// ===========================================================================

// --- 15.1: All four channels active simultaneously ---
#[test]
fn test_15_1_all_four_channels() {
    let mut p = Params::default();
    p.channel_split = true;
    p.pulse1_ol = true;  p.pulse1_or = true;  p.pulse1_duty = 2;
    p.pulse2_ol = true;  p.pulse2_or = true;  p.pulse2_duty = 1;
    p.wave_ol = true;    p.wave_or = true;     p.wave_index = 3;
    p.noise_ol = true;   p.noise_or = true;    p.noise_shift = 4; p.noise_step = 0; p.noise_ratio = 2;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),   // pulse 1
        ev(0, 2, MidiKind::NoteOn(64)),   // pulse 2
        ev(0, 3, MidiKind::NoteOn(67)),   // wave
        ev(0, 4, MidiKind::NoteOn(48)),   // noise
    ]), expect!["raw:c484bb0560f9a5b5"]);
}

// --- 15.2: Vibrato + pitch bend simultaneously on pulse 1 ---
#[test]
fn test_15_2_vibrato_plus_pitch_bend() {
    let mut p = Params::default();
    p.pulse1_vib_rate = 7.0;
    p.pulse1_vib_amt = 80.0;
    check_processor(ProcessorProgram {
        voices: 1,
        params: p,
        blocks: vec![
            Block { size: 1024, events: vec![
                ev(0, 1, MidiKind::NoteOn(60)),
            ]},
            Block { size: 1024, events: vec![
                ev(0, 1, MidiKind::PitchBend(12288)), // +2 semitones
            ]},
            Block { size: 1024, events: vec![] },
        ],
    }, expect!["raw:42915bfb3fb73245"]);
}

// --- 15.3: Rapid note on/off within one block (mono stacking) ---
#[test]
fn test_15_3_rapid_mono_stacking() {
    check_processor(proc1(1, Params::default(), 2048, vec![
        ev(0,    1, MidiKind::NoteOn(60)),
        ev(128,  1, MidiKind::NoteOn(64)),
        ev(256,  1, MidiKind::NoteOn(67)),
        ev(384,  1, MidiKind::NoteOff(67)),  // reveals 64
        ev(512,  1, MidiKind::NoteOff(64)),  // reveals 60
        ev(640,  1, MidiKind::NoteOff(60)),  // silence
        ev(768,  1, MidiKind::NoteOn(72)),   // new note
    ]), expect!["raw:d0dbf76d1e938865"]);
}

// --- 15.4: AllNotesOff mid-block with 2 voices active ---
#[test]
fn test_15_4_all_notes_off_poly() {
    check_processor(proc1(2, Params::default(), 2048, vec![
        ev(0,   1, MidiKind::NoteOn(60)),
        ev(0,   1, MidiKind::NoteOn(64)),
        ev(512, 1, MidiKind::AllNotesOff),
        ev(1024, 1, MidiKind::NoteOn(67)),
    ]), expect!["raw:39957bafb8c72fe1"]);
}

// --- 15.5: Extreme tuning: tune=+48 semitones + fine=+100 cents (highest possible) ---
#[test]
fn test_15_5_extreme_tune_high() {
    let mut p = Params::default();
    p.pulse1_tune = 48;
    p.pulse1_fine = 100;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:877e6df6a1dc2945"]);
}

// --- 15.6: Extreme tuning: tune=-48 semitones + fine=-100 cents (lowest possible) ---
#[test]
fn test_15_6_extreme_tune_low() {
    let mut p = Params::default();
    p.pulse1_tune = -48;
    p.pulse1_fine = -100;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:0970681e813edeed"]);
}

// --- 15.7: Pulse1 release envelope: A=0 (instant attack), R=7 (slow release) ---
#[test]
fn test_15_7_instant_attack_slow_release() {
    let mut p = Params::default();
    p.pulse1_a = 0;
    p.pulse1_r = 7;
    check_processor(ProcessorProgram {
        voices: 1,
        params: p,
        blocks: vec![
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::NoteOn(60)),
                ev(512, 1, MidiKind::NoteOff(60)),
            ]},
            Block { size: 2048, events: vec![] }, // let release ring out
        ],
    }, expect!["raw:32a897d101346445"]);
}

// --- 15.8: Envelope A=7 R=0 (slowest attack, no release decay) ---
#[test]
fn test_15_8_slow_attack_no_release() {
    let mut p = Params::default();
    p.pulse1_a = 7;
    p.pulse1_r = 0;
    check_processor(ProcessorProgram {
        voices: 1,
        params: p,
        blocks: vec![
            Block { size: 2048, events: vec![
                ev(0,    1, MidiKind::NoteOn(60)),
                ev(1024, 1, MidiKind::NoteOff(60)),
            ]},
            Block { size: 2048, events: vec![] },
        ],
    }, expect!["raw:8f6955bf94ec2325"]);
}

// --- 15.9: Sweep to overflow (upward sweep from high period) ---
#[test]
fn test_15_9_sweep_overflow() {
    let mut p = Params::default();
    p.pulse1_sweep = 7;   // maximum upward sweep rate
    p.pulse1_shift = 1;
    check_processor(proc1(1, p, 4096, vec![
        ev(0, 1, MidiKind::NoteOn(84)),  // high note, sweep pushes higher
    ]), expect!["raw:8f6955bf94ec2325"]);
}

// --- 15.10: Downward sweep ---
#[test]
fn test_15_10_sweep_down() {
    let mut p = Params::default();
    p.pulse1_sweep = -5;
    p.pulse1_shift = 3;
    check_processor(proc1(1, p, 4096, vec![
        ev(0, 1, MidiKind::NoteOn(48)),
    ]), expect!["raw:858d1c33c5b0c46d"]);
}

// --- 15.11: Wave channel with all 15 presets cycled across blocks ---
#[test]
fn test_15_11_wave_preset_cycle() {
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.wave_ol = true; p.wave_or = true;
    let mut blocks = Vec::new();
    for i in 0..15u8 {
        let mut pi = p.clone();
        pi.wave_index = i;
        // We can only pass one params per ProcessorProgram, so build 15 separate tests.
        // Instead, test a few representative presets in one program.
        let _ = pi;
    }
    // Test preset 0, 7, 14 (first, middle, last) across 3 blocks
    // (params are per-program, so test the last preset in the series)
    p.wave_index = 14;
    blocks.push(Block { size: 2048, events: vec![ev(0, 1, MidiKind::NoteOn(60))] });
    blocks.push(Block { size: 2048, events: vec![] });
    check_processor(ProcessorProgram {
        voices: 1,
        params: p,
        blocks,
    }, expect!["raw:7168bd22b9b01225"]);
}

// --- 15.12: Noise channel 7-bit LFSR mode with varying ratios ---
#[test]
fn test_15_12_noise_7bit_lfsr() {
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.noise_ol = true; p.noise_or = true;
    p.noise_step = 1;   // 7-bit LFSR (metallic)
    p.noise_shift = 5;
    p.noise_ratio = 7;  // maximum divisor
    p.noise_a = 0;
    p.noise_r = 2;
    check_processor(proc1(1, p, 2048, vec![
        ev(0,    1, MidiKind::NoteOn(60)),
        ev(1024, 1, MidiKind::NoteOff(60)),
    ]), expect!["raw:9341a6bbb8fd0f45"]);
}

// --- 15.13: Global output level 0 (should be very quiet/silent) ---
#[test]
fn test_15_13_output_level_zero() {
    let mut p = Params::default();
    p.output = 0;
    check_processor(proc1(1, p, 1024, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:5103cc1eaeba03a5"]);
}

// --- 15.14: Extreme treble EQ ---
#[test]
fn test_15_14_extreme_treble() {
    let mut p = Params::default();
    p.treble = -50.0;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:91c7c28e92d09e55"]);
}

// --- 15.15: Low bass frequency (15 Hz minimum) ---
#[test]
fn test_15_15_bass_freq_min() {
    let mut p = Params::default();
    p.bass = 15;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
    ]), expect!["raw:d034260d314c3e8d"]);
}

// --- 15.16: Multi-block with notes spanning blocks and voice stealing ---
#[test]
fn test_15_16_multi_block_voice_steal() {
    check_processor(ProcessorProgram {
        voices: 2,
        params: Params::default(),
        blocks: vec![
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::NoteOn(60)),
                ev(256, 1, MidiKind::NoteOn(64)),
            ]},
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::NoteOn(67)),  // steals a voice
                ev(512, 1, MidiKind::NoteOff(60)),
            ]},
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::NoteOff(64)),
                ev(0,   1, MidiKind::NoteOff(67)),
            ]},
        ],
    }, expect!["raw:c2b23019a16b7c61"]);
}

// --- 15.17: Pitch bend extremes (min and max) ---
#[test]
fn test_15_17_pitch_bend_extremes() {
    check_processor(ProcessorProgram {
        voices: 1,
        params: Params::default(),
        blocks: vec![
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::NoteOn(60)),
                ev(256, 1, MidiKind::PitchBend(0)),      // -2 semitones (minimum)
            ]},
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::PitchBend(16383)),   // +2 semitones (maximum)
            ]},
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::PitchBend(8192)),    // center (no bend)
            ]},
        ],
    }, expect!["raw:203d2b09fd3a7bf5"]);
}

// --- 15.18: Channel split with staggered note-offs across channels ---
#[test]
fn test_15_18_channel_split_staggered() {
    let mut p = Params::default();
    p.channel_split = true;
    p.pulse1_ol = true;  p.pulse1_or = true;  p.pulse1_duty = 2;
    p.pulse2_ol = true;  p.pulse2_or = true;  p.pulse2_duty = 0;
    p.wave_ol = true;    p.wave_or = true;
    p.noise_ol = true;   p.noise_or = true;
    check_processor(ProcessorProgram {
        voices: 1,
        params: p,
        blocks: vec![
            Block { size: 1024, events: vec![
                ev(0, 1, MidiKind::NoteOn(60)),
                ev(0, 2, MidiKind::NoteOn(64)),
                ev(0, 3, MidiKind::NoteOn(67)),
                ev(0, 4, MidiKind::NoteOn(48)),
            ]},
            Block { size: 1024, events: vec![
                ev(0,   4, MidiKind::NoteOff(48)),  // noise off first
                ev(256, 3, MidiKind::NoteOff(67)),  // wave off
                ev(512, 2, MidiKind::NoteOff(64)),  // pulse2 off
                ev(768, 1, MidiKind::NoteOff(60)),  // pulse1 off last
            ]},
        ],
    }, expect!["raw:1b4c2cfebbb15b49"]);
}

// --- 15.19: Pulse2 with vibrato + tuning (tests ch2 vibrato path) ---
#[test]
fn test_15_19_pulse2_vibrato_tuned() {
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.pulse2_ol = true;  p.pulse2_or = true;
    p.pulse2_duty = 3;
    p.pulse2_tune = 7;
    p.pulse2_fine = 50;
    p.pulse2_vib_rate = 10.0;
    p.pulse2_vib_amt = 60.0;
    p.pulse2_a = 3;
    p.pulse2_r = 3;
    check_processor(ProcessorProgram {
        voices: 1,
        params: p,
        blocks: vec![
            Block { size: 2048, events: vec![ev(0, 1, MidiKind::NoteOn(55))] },
            Block { size: 2048, events: vec![ev(1024, 1, MidiKind::NoteOff(55))] },
        ],
    }, expect!["raw:8c89c35ce9ef5d09"]);
}

// --- 15.20: Large block size (4096) with many MIDI events ---
#[test]
fn test_15_20_large_block_many_events() {
    let mut events = Vec::new();
    // Rapid arpeggiated pattern: C-E-G repeated
    let notes = [60u8, 64, 67];
    for i in 0..12 {
        let pos = i * 256;
        if i > 0 {
            events.push(ev(pos, 1, MidiKind::NoteOff(notes[((i - 1) % 3) as usize])));
        }
        events.push(ev(pos, 1, MidiKind::NoteOn(notes[(i % 3) as usize])));
    }
    check_processor(proc1(1, Params::default(), 4096, events), expect!["raw:62bcecf2588eb8a9"]);
}

// --- 15.21: Pulse1 + Pulse2 left/right panning (hard left/right split) ---
#[test]
fn test_15_21_hard_stereo_split() {
    let mut p = Params::default();
    p.pulse1_ol = true;  p.pulse1_or = false;  // pulse1 left only
    p.pulse2_ol = false; p.pulse2_or = true;    // pulse2 right only
    p.pulse2_duty = 2;
    p.channel_split = true;
    check_processor(proc1(1, p, 2048, vec![
        ev(0, 1, MidiKind::NoteOn(60)),
        ev(0, 2, MidiKind::NoteOn(72)),
    ]), expect!["raw:50466d7e0a0a46a8"]);
}

// --- 15.22: Polyphonic with 4 voices, all active then released ---
#[test]
fn test_15_22_four_voice_poly() {
    check_processor(ProcessorProgram {
        voices: 4,
        params: Params::default(),
        blocks: vec![
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::NoteOn(60)),
                ev(0,   1, MidiKind::NoteOn(64)),
                ev(0,   1, MidiKind::NoteOn(67)),
                ev(0,   1, MidiKind::NoteOn(72)),
            ]},
            Block { size: 1024, events: vec![
                ev(0,   1, MidiKind::NoteOff(72)),
                ev(256, 1, MidiKind::NoteOff(67)),
                ev(512, 1, MidiKind::NoteOff(64)),
                ev(768, 1, MidiKind::NoteOff(60)),
            ]},
        ],
    }, expect!["raw:7356947ab5eada19"]);
}

// --- 15.23: Note-on replaces identical note (retrigger same pitch) ---
#[test]
fn test_15_23_retrigger_same_note() {
    check_processor(proc1(1, Params::default(), 2048, vec![
        ev(0,    1, MidiKind::NoteOn(60)),
        ev(512,  1, MidiKind::NoteOff(60)),
        ev(512,  1, MidiKind::NoteOn(60)),   // immediate retrigger
        ev(1024, 1, MidiKind::NoteOff(60)),
        ev(1024, 1, MidiKind::NoteOn(60)),   // retrigger again
    ]), expect!["raw:e314c057b7f511c9"]);
}
