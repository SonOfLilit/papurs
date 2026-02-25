//! Gold test framework for PAPURS.
//!
//! Compiles the C++ reference harness once, then runs scenarios through it to
//! produce reference audio. Each scenario's output is stored as a content-
//! addressed file in `tests/fixtures/` (gitignored) and the hash is snapshotted
//! with expect_test. When the Rust implementation is complete, these tests will
//! also run the Rust engine and compare bit-exactly against the C++ reference.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

use expect_test::{expect, Expect};

// ---------------------------------------------------------------------------
// Harness compilation
// ---------------------------------------------------------------------------

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Compile the C++ harness binary if any source is newer than the binary.
/// Returns the path to the compiled binary.
fn build_harness() -> PathBuf {
    let root = manifest_dir();
    let harness_bin = root.join("tests/cpp_harness/harness");

    let sources: &[&str] = &[
        "tests/cpp_harness/main.cpp",
        "papu-original/plugin/Source/gb_apu/Blip_Buffer.cpp",
        "papu-original/plugin/Source/gb_apu/Multi_Buffer.cpp",
        "papu-original/plugin/Source/gb_apu/Gb_Apu.cpp",
        "papu-original/plugin/Source/gb_apu/Gb_Oscs.cpp",
    ];

    // Rebuild if binary doesn't exist or any source is newer
    let needs_rebuild = !harness_bin.exists() || {
        let bin_mtime = mtime(&harness_bin);
        sources.iter().any(|s| mtime(&root.join(s)) > bin_mtime)
    };

    if needs_rebuild {
        let gb_apu_dir = root.join("papu-original/plugin/Source/gb_apu");
        let source_dir = root.join("papu-original/plugin/Source");

        let mut cmd = Command::new("g++");
        cmd.arg("-std=c++14")
            .arg("-O2")
            .arg("-I").arg(&gb_apu_dir)
            .arg("-I").arg(&source_dir);
        for s in sources {
            cmd.arg(root.join(s));
        }
        cmd.arg("-o").arg(&harness_bin);

        let status = cmd.status().expect(
            "failed to run g++; make sure g++ is installed (apt install g++)"
        );
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
// Running scenarios
// ---------------------------------------------------------------------------

/// Run the C++ harness for `scenario`, return raw i16 LE stereo samples.
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

// ---------------------------------------------------------------------------
// Content-addressed file storage
// ---------------------------------------------------------------------------

/// FNV-1a 64-bit hash — deterministic, no external deps.
fn fnv1a_64(data: &[u8]) -> u64 {
    const BASIS: u64 = 14695981039346656037;
    const PRIME: u64 = 1099511628211;
    data.iter().fold(BASIS, |h, &b| h.wrapping_mul(PRIME) ^ b as u64)
}

/// Write `audio` to a content-addressed file in `tests/fixtures/` (gitignored)
/// and pass `"raw:{hash}"` to `expect` for snapshot testing.
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

// ---------------------------------------------------------------------------
// Test 0.1 — Silence
// ---------------------------------------------------------------------------

fn check_silence(expect: Expect) {
    let audio = run_cpp("silence");
    assert_eq!(
        audio.len(),
        512 * 2 * 2, // 512 stereo pairs × 2 channels × 2 bytes
        "expected 512 stereo pairs"
    );
    testfile(&audio, expect);
}

#[test]
fn test_0_1_silence() {
    check_silence(expect!["raw:28c31cf8df2ec325"]);
}

// ---------------------------------------------------------------------------
// Test 0.2 — Single square wave note (MIDI 60, default params)
// ---------------------------------------------------------------------------

fn check_note60(expect: Expect) {
    let audio = run_cpp("note60");
    assert_eq!(
        audio.len(),
        2048 * 2 * 2, // 2048 stereo pairs × 2 channels × 2 bytes
        "expected 2048 stereo pairs"
    );
    testfile(&audio, expect);
}

#[test]
fn test_0_2_note60() {
    check_note60(expect!["raw:eabaec97e5716a7d"]);
}

// ---------------------------------------------------------------------------
// Phase 1 gold tests — Blip_Buffer / Blip_Synth / Stereo_Buffer
// ---------------------------------------------------------------------------
//
// Each test runs the C++ harness to get reference bytes, then runs the same
// scenario through the Rust Blip port and compares bit-exactly.

use papurs::blip::{BlipBuffer, BlipEq, BlipSynth, StereoBuffer};

const CLOCK_RATE:  i64 = 4_194_304;
const SAMPLE_RATE: i64 = 44_100;
const FRAME_SIZE:  i64 = 1024;
const PAPU_VOL_UNIT: f64 = 1.0 / 210.0;

fn papu_eq() -> BlipEq { BlipEq::new(-20.0) }

fn setup_buf(bass: i32) -> BlipBuffer {
    let mut buf = BlipBuffer::new();
    buf.clock_rate(CLOCK_RATE);
    buf.set_sample_rate(SAMPLE_RATE, None);
    buf.bass_freq(bass);
    buf
}

/// Render up to `n_samples` mono samples, calling end_frame(FRAME_SIZE) as needed.
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

/// Render up to `n_pairs` stereo pairs, calling end_frame as needed.
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

fn i16_to_bytes(samples: &[i16]) -> Vec<u8> {
    samples.iter().flat_map(|&s| s.to_le_bytes()).collect()
}

fn compare_with_cpp(scenario: &str, rust_bytes: Vec<u8>) {
    let cpp_bytes = run_cpp(scenario);
    assert_eq!(
        cpp_bytes.len(), rust_bytes.len(),
        "length mismatch for '{scenario}': cpp={} rust={}",
        cpp_bytes.len(), rust_bytes.len()
    );
    let mismatches: Vec<usize> = cpp_bytes.iter().zip(rust_bytes.iter())
        .enumerate()
        .filter(|(_, (a, b))| a != b)
        .map(|(i, _)| i)
        .collect();
    if !mismatches.is_empty() {
        let first = mismatches[0];
        panic!(
            "'{scenario}': {}/{} bytes differ, first mismatch at byte {first}: cpp={} rust={}",
            mismatches.len(), cpp_bytes.len(),
            cpp_bytes[first], rust_bytes[first]
        );
    }
}

fn check_blip_scenario(scenario: &str, rust_bytes: Vec<u8>, expect: Expect) {
    testfile(&rust_bytes, expect);
    compare_with_cpp(scenario, rust_bytes);
}

// ---- Test 1.1: blip_impulse_q3 ----

#[test]
fn test_1_1_blip_impulse_q3() {
    let mut buf = setup_buf(461);
    let mut synth = BlipSynth::new(3, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);
    synth.offset(0, 100, &mut buf);
    let samples = render_mono_rust(512, &mut buf);
    let bytes = i16_to_bytes(&samples);
    check_blip_scenario("blip_impulse_q3", bytes, expect!["raw:7c814331784516c9"]);
}

// ---- Test 1.2: blip_impulse_q2 ----

#[test]
fn test_1_2_blip_impulse_q2() {
    let mut buf = setup_buf(461);
    let mut synth = BlipSynth::new(2, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);
    synth.offset(0, 100, &mut buf);
    let samples = render_mono_rust(512, &mut buf);
    let bytes = i16_to_bytes(&samples);
    check_blip_scenario("blip_impulse_q2", bytes, expect!["raw:204d83055d9ca931"]);
}

// ---- Test 1.3: blip_square_440 ----

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
    let bytes = i16_to_bytes(&samples);
    check_blip_scenario("blip_square_440", bytes, expect!["raw:38c625f6b615a6c9"]);
}

// ---- Test 1.4: blip_bass_step ----

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
    let bytes = i16_to_bytes(&all);
    check_blip_scenario("blip_bass_step", bytes, expect!["raw:8465c804c7df7e8f"]);
}

// ---- Test 1.5: blip_stereo ----

#[test]
fn test_1_5_blip_stereo() {
    let mut sbuf = StereoBuffer::new();
    sbuf.clock_rate(CLOCK_RATE);
    sbuf.set_sample_rate(SAMPLE_RATE);
    sbuf.bass_freq(461);

    let mut synth = BlipSynth::new(3, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);

    synth.offset(0,  100, sbuf.center());
    synth.offset(10,  50, sbuf.left());
    synth.offset(20,  25, sbuf.right());

    let samples = render_stereo_rust(512, &mut sbuf, true);
    let bytes = i16_to_bytes(&samples);
    check_blip_scenario("blip_stereo", bytes, expect!["raw:76e9cb0c76469876"]);
}

// ---- Test 1.6: blip_multiframe ----

#[test]
fn test_1_6_blip_multiframe() {
    const HALF_PERIOD: i64 = 4766;
    let mut buf = setup_buf(461);
    let mut synth = BlipSynth::new(3, 210);
    synth.treble_eq(&papu_eq());
    synth.volume_unit(PAPU_VOL_UNIT);

    let mut amp = 100i32;
    let mut frame_t: i64 = 0;
    let mut abs_t:   i64 = 0;
    let mut all = Vec::new();
    let mut tmp = vec![0i16; 512];

    for frame in 0..100i64 {
        let frame_end = (frame + 1) * FRAME_SIZE;
        while abs_t < frame_end {
            synth.offset(frame_t, amp, &mut buf);
            amp      = -amp;
            frame_t += HALF_PERIOD;
            abs_t   += HALF_PERIOD;
        }
        buf.end_frame(FRAME_SIZE);
        frame_t -= FRAME_SIZE;

        let mut avail = buf.samples_avail() as usize;
        while avail > 0 {
            let want = avail.min(512);
            let got  = buf.read_samples(&mut tmp, want);
            all.extend_from_slice(&tmp[..got]);
            avail -= got;
        }
    }

    let bytes = i16_to_bytes(&all);
    check_blip_scenario("blip_multiframe", bytes, expect!["raw:8e95a8b764ea650e"]);
}

// ---------------------------------------------------------------------------
// Phase 2-8 gold tests — Gb_Apu
// ---------------------------------------------------------------------------

use papurs::apu::GoldHarness;

fn midi_to_gb_period(note: i32) -> u32 {
    let freq = 440.0f64 * 2.0f64.powf((note - 69) as f64 / 12.0);
    (((4_194_304.0 / freq) - 65_536.0) / -32.0) as u32
}

fn render_harness(h: &mut GoldHarness, n_pairs: usize) -> Vec<u8> {
    let mut out = Vec::new();
    h.render(n_pairs, &mut out);
    i16_to_bytes(&out)
}

fn check_apu(scenario: &str, rust_bytes: Vec<u8>, expect: Expect) {
    testfile(&rust_bytes, expect);
    compare_with_cpp(scenario, rust_bytes);
}

// ---- Test 2.1–2.4: Square duty cycles ----

fn make_sq1_note(duty: u8, note: i32) -> GoldHarness {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x11);
    let period = midi_to_gb_period(note);
    h.write_reg(0xff10, 0x00);
    h.write_reg(0xff11, (duty as u32) << 6);
    h.write_reg(0xff12, 0x08 | (15 << 4));
    h.write_reg(0xff13, period & 0xff);
    h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
    h
}

#[test]
fn test_2_1_sq_duty0() {
    let bytes = render_harness(&mut make_sq1_note(0, 60), 2048);
    check_apu("apu_sq_duty0", bytes, expect!["raw:9eff75d675887dc1"]);
}
#[test]
fn test_2_2_sq_duty1() {
    let bytes = render_harness(&mut make_sq1_note(1, 60), 2048);
    check_apu("apu_sq_duty1", bytes, expect!["raw:8b3f71a2b8c82311"]);
}
#[test]
fn test_2_3_sq_duty2() {
    let bytes = render_harness(&mut make_sq1_note(2, 60), 2048);
    check_apu("apu_sq_duty2", bytes, expect!["raw:a284f10511191099"]);
}
#[test]
fn test_2_4_sq_duty3() {
    let bytes = render_harness(&mut make_sq1_note(3, 60), 2048);
    check_apu("apu_sq_duty3", bytes, expect!["raw:c2577baa08cef7f1"]);
}

// ---- Test 2.5: Freq min ----

#[test]
fn test_2_5_sq_freq_min() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x11);
    h.write_reg(0xff10, 0x00);
    h.write_reg(0xff11, 2 << 6);
    h.write_reg(0xff12, 0x08 | (15 << 4));
    h.write_reg(0xff13, 0x00);
    h.write_reg(0xff14, 0x80); // freq=0, period=8192
    let bytes = render_harness(&mut h, 2048);
    check_apu("apu_sq_freq_min", bytes, expect!["raw:b9d103fd6854a325"]);
}

// ---- Test 2.6: Freq max ----

#[test]
fn test_2_6_sq_freq_max() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x11);
    h.write_reg(0xff10, 0x00);
    h.write_reg(0xff11, 2 << 6);
    h.write_reg(0xff12, 0x08 | (15 << 4));
    h.write_reg(0xff13, 2020 & 0xff);
    h.write_reg(0xff14, 0x80 | ((2020 >> 8) & 0x07));
    let bytes = render_harness(&mut h, 2048);
    check_apu("apu_sq_freq_max", bytes, expect!["raw:38d95fde70ae84e5"]);
}

// ---- Test 2.7: Volume=0 ----

#[test]
fn test_2_7_sq_vol0() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x11);
    let period = midi_to_gb_period(60);
    h.write_reg(0xff10, 0x00);
    h.write_reg(0xff11, 2 << 6);
    h.write_reg(0xff12, 0x00); // volume=0
    h.write_reg(0xff13, period & 0xff);
    h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
    let bytes = render_harness(&mut h, 2048);
    check_apu("apu_sq_vol0", bytes, expect!["raw:b9d103fd6854a325"]);
}

// ---- Test 3.1: Envelope (attack) ----

#[test]
fn test_3_1_envelope() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x11);
    let period = midi_to_gb_period(60);
    h.write_reg(0xff10, 0x00);
    h.write_reg(0xff11, 2 << 6);
    h.write_reg(0xff12, (0 << 4) | 0x08 | 1); // vol=0, dir=up, period=1
    h.write_reg(0xff13, period & 0xff);
    h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
    let bytes = render_harness(&mut h, 4096);
    check_apu("apu_envelope", bytes, expect!["raw:0224d0543428e14d"]);
}

// ---- Test 4.1: Sweep ----

#[test]
fn test_4_1_sweep() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x11);
    h.write_reg(0xff10, (2 << 4) | 0 | 1); // period=2, dir=up, shift=1
    h.write_reg(0xff11, 2 << 6);
    h.write_reg(0xff12, 0x08 | (15 << 4));
    let period = midi_to_gb_period(60);
    h.write_reg(0xff13, period & 0xff);
    h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
    let bytes = render_harness(&mut h, 4096);
    check_apu("apu_sweep", bytes, expect!["raw:9c1bda7f8c872325"]);
}

// ---- Test 5.1: Length counter ----

#[test]
fn test_5_1_length() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x11);
    let period = midi_to_gb_period(60);
    h.write_reg(0xff10, 0x00);
    h.write_reg(0xff11, (2 << 6) | (64 - 2)); // duty=2, length=2
    h.write_reg(0xff12, 0x08 | (15 << 4));
    h.write_reg(0xff13, period & 0xff);
    h.write_reg(0xff14, 0x80 | 0x40 | ((period >> 8) & 0x07)); // trigger+length_enable
    let bytes = render_harness(&mut h, 4096);
    check_apu("apu_length", bytes, expect!["raw:ed10c493e9cd1481"]);
}

// ---- Test 6.1: Wave channel ----

#[test]
fn test_6_1_wave() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x44); // wave → center (bit 2 high nibble = 0x44)
    let period = midi_to_gb_period(60);
    h.write_reg(0xff1C, 1 << 5); // volume = 100%
    h.write_reg(0xff1D, period & 0xff);
    h.write_reg(0xff1E, 0x80 | ((period >> 8) & 0x07));
    let bytes = render_harness(&mut h, 2048);
    check_apu("apu_wave", bytes, expect!["raw:8e9ad610266f6f55"]);
}

// ---- Test 7.1: Noise channel ----

#[test]
fn test_7_1_noise() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x88); // noise → center
    h.write_reg(0xff21, 0x08 | (15 << 4)); // vol=15, dir=up, period=0
    h.write_reg(0xff22, 0x00); // shift=0, step=0 (15-bit LFSR), divisor=0
    h.write_reg(0xff23, 0x80); // trigger
    let bytes = render_harness(&mut h, 2048);
    check_apu("apu_noise", bytes, expect!["raw:8b8a9b7776166131"]);
}

// ---------------------------------------------------------------------------
// Phase 9 gold tests — PapuEngine (MIDI → APU)
// ---------------------------------------------------------------------------

use papurs::engine::{MidiEvent, MidiKind, Params, PapuEngine, PapuProcessor};

fn make_engine() -> PapuEngine {
    let mut e = PapuEngine::new();
    e.prepare(44_100.0);
    e
}

fn run_engine(engine: &mut PapuEngine, params: &Params, block_size: i32, events: &[MidiEvent]) -> Vec<u8> {
    let mut out = Vec::<i16>::new();
    engine.process_block(block_size, params, events, &mut out);
    i16_to_bytes(&out)
}

fn check_engine(scenario: &str, rust_bytes: Vec<u8>, expect: Expect) {
    testfile(&rust_bytes, expect);
    compare_with_cpp(scenario, rust_bytes);
}

// ---- Test 9.1: engine_note_on_off ----

#[test]
fn test_9_1_engine_note_on_off() {
    let mut e = make_engine();
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 0,   channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 512, channel: 1, kind: MidiKind::NoteOff(60) },
    ];
    let bytes = run_engine(&mut e, &p, 1024, &events);
    check_engine("engine_note_on_off", bytes, expect!["raw:ebdfa5713a6e3e69"]);
}

// ---- Test 9.2: engine_mono_priority ----
// Notes 60 and 64: LIFO queue — 64 on top, release 64 → 60 resumes

#[test]
fn test_9_2_engine_mono_priority() {
    let mut e = make_engine();
    let p = Params::default();
    // Block 1: note 60 on at 0, note 64 on at 256, note 64 off at 1024 (end)
    let events1 = vec![
        MidiEvent { pos: 0,    channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 256,  channel: 1, kind: MidiKind::NoteOn(64) },
        MidiEvent { pos: 1024, channel: 1, kind: MidiKind::NoteOff(64) },
    ];
    let mut out = Vec::<i16>::new();
    e.process_block(1024, &p, &events1, &mut out);
    // Block 2: no events, note 60 should resume
    e.process_block(1024, &p, &[], &mut out);
    let bytes = i16_to_bytes(&out);
    check_engine("engine_mono_priority", bytes, expect!["raw:94156c1dbd39f33d"]);
}

// ---- Test 9.3: engine_pitch_bend ----

#[test]
fn test_9_3_engine_pitch_bend() {
    let mut e = make_engine();
    let p = Params::default();
    // pitch wheel 12288 → (12288-8192)/8192.0f32*2 = 1.0 semitone
    let events = vec![
        MidiEvent { pos: 0,   channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 256, channel: 1, kind: MidiKind::PitchBend(12288) },
    ];
    let bytes = run_engine(&mut e, &p, 1024, &events);
    check_engine("engine_pitch_bend", bytes, expect!["raw:2518094b051f8815"]);
}

// ---- Test 9.4: engine_channel_split ----

#[test]
fn test_9_4_engine_channel_split() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.channel_split = true;
    p.pulse2_ol = true;
    p.pulse2_or = true;
    let events = vec![
        MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 0, channel: 2, kind: MidiKind::NoteOn(64) },
    ];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_channel_split", bytes, expect!["raw:a15b71ee5e384fd1"]);
}

// ---------------------------------------------------------------------------
// Phase 11 gold tests — Full Parameter Mapping
// ---------------------------------------------------------------------------

// ---- Test 11.1: engine_full_pulse1 ----

#[test]
fn test_11_1_full_pulse1() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.pulse1_duty  = 2;
    p.pulse1_a     = 3;
    p.pulse1_r     = 5;
    p.pulse1_tune  = 12;
    p.pulse1_fine  = 50;
    p.pulse1_sweep = -3;
    p.pulse1_shift = 2;
    let events = vec![
        MidiEvent { pos: 0,    channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 1024, channel: 1, kind: MidiKind::NoteOff(60) },
    ];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_full_pulse1", bytes, expect!["raw:af5b71625c4f6459"]);
}

// ---- Test 11.2: engine_full_pulse2 ----

#[test]
fn test_11_2_full_pulse2() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.pulse2_ol = true;  p.pulse2_or = true;
    p.pulse2_duty = 1;
    p.pulse2_a    = 2;
    p.pulse2_r    = 4;
    p.pulse2_tune = -7;
    p.pulse2_fine = -25;
    let events = vec![
        MidiEvent { pos: 0,    channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 1024, channel: 1, kind: MidiKind::NoteOff(60) },
    ];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_full_pulse2", bytes, expect!["raw:570a1de27d6e35cd"]);
}

// ---- Test 11.3: engine_wave_params ----

#[test]
fn test_11_3_wave_params() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.pulse1_ol  = false; p.pulse1_or = false;
    p.wave_ol    = true;  p.wave_or   = true;
    p.wave_tune  = -7;
    p.wave_fine  = -25;
    p.wave_index = 5;
    let events = vec![MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) }];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_wave_params", bytes, expect!["raw:6a0df560742f38a5"]);
}

// ---- Test 11.4: engine_noise_params ----

#[test]
fn test_11_4_noise_params() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.pulse1_ol   = false; p.pulse1_or  = false;
    p.noise_ol    = true;  p.noise_or   = true;
    p.noise_a     = 0;
    p.noise_r     = 4;
    p.noise_shift = 8;
    p.noise_step  = 1;
    p.noise_ratio = 3;
    let events = vec![
        MidiEvent { pos: 0,    channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 1024, channel: 1, kind: MidiKind::NoteOff(60) },
    ];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_noise_params", bytes, expect!["raw:8acc0ff3f7a73655"]);
}

// ---- Test 11.5: engine_global_params ----

#[test]
fn test_11_5_global_params() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.output = 5;
    p.treble = -30.0;
    p.bass   = 461;
    let events = vec![MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) }];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_global_params", bytes, expect!["raw:c782702f89cce10d"]);
}

// ---------------------------------------------------------------------------
// Phase 10 gold tests — Vibrato LFO
// ---------------------------------------------------------------------------

// ---- Test 10.1: engine_vibrato_sq1 ----
// Pulse 1, note 60, vib rate=5Hz, amt=100 (depth=0.25), 2048 pairs.
// LFO advances 2048 samples → phase ≈ 0.2322 → output ≈ 0.2484 → +2.98 semitones.

#[test]
fn test_10_1_engine_vibrato_sq1() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.pulse1_vib_rate = 5.0;
    p.pulse1_vib_amt  = 100.0;
    let events = vec![MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) }];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_vibrato_sq1", bytes, expect!["raw:a14331e86b6fe279"]);
}

// ---- Test 10.2: engine_vibrato_wave ----
// Wave channel, note 60, vib rate=3Hz, amt=50 (depth=0.125), 2048 pairs.

#[test]
fn test_10_2_engine_vibrato_wave() {
    let mut e = make_engine();
    let mut p = Params::default();
    p.pulse1_ol = false; p.pulse1_or = false;
    p.wave_ol   = true;  p.wave_or   = true;
    p.wave_vib_rate = 3.0;
    p.wave_vib_amt  = 50.0;
    let events = vec![MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) }];
    let bytes = run_engine(&mut e, &p, 2048, &events);
    check_engine("engine_vibrato_wave", bytes, expect!["raw:1e06137e902c0e55"]);
}

// ---- Test 8.3: Stereo panning ----

#[test]
fn test_8_3_stereo() {
    let mut h = GoldHarness::new();
    h.init();
    h.write_reg(0xff24, 0x08 | 7);
    h.write_reg(0xff25, 0x21); // sq1=left (bit4), sq2=right (bit1)
    let period = midi_to_gb_period(60);
    h.write_reg(0xff10, 0x00);
    h.write_reg(0xff11, 2 << 6);
    h.write_reg(0xff12, 0x08 | (15 << 4));
    h.write_reg(0xff13, period & 0xff);
    h.write_reg(0xff14, 0x80 | ((period >> 8) & 0x07));
    let period2 = midi_to_gb_period(64);
    h.write_reg(0xff16, 2 << 6);
    h.write_reg(0xff17, 0x08 | (15 << 4));
    h.write_reg(0xff18, period2 & 0xff);
    h.write_reg(0xff19, 0x80 | ((period2 >> 8) & 0x07));
    let bytes = render_harness(&mut h, 2048);
    check_apu("apu_stereo", bytes, expect!["raw:919b49ff65f15ed8"]);
}

// ---------------------------------------------------------------------------
// Phase 12-13 gold tests — PapuProcessor (multi-voice)
// ---------------------------------------------------------------------------

fn f32_to_bytes(samples: &[f32]) -> Vec<u8> {
    samples.iter().flat_map(|&s| s.to_le_bytes()).collect()
}

fn make_processor(voices: usize) -> PapuProcessor {
    let mut p = PapuProcessor::new(voices);
    p.prepare(44_100.0);
    p
}

fn run_processor(proc: &mut PapuProcessor, params: &Params, block_size: i32, events: &[MidiEvent]) -> Vec<u8> {
    f32_to_bytes(&proc.process_block(block_size, params, events))
}

fn check_processor(scenario: &str, rust_bytes: Vec<u8>, expect: Expect) {
    testfile(&rust_bytes, expect);
    compare_with_cpp(scenario, rust_bytes);
}

// ---- Test 12.1: proc_voices1 ----
// 1 voice, note 60 on at 0, off at 512, block=1024. Output: f32 bytes.

#[test]
fn test_12_1_proc_voices1() {
    let mut proc = make_processor(1);
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 0,   channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 512, channel: 1, kind: MidiKind::NoteOff(60) },
    ];
    let bytes = run_processor(&mut proc, &p, 1024, &events);
    check_processor("proc_voices1", bytes, expect!["raw:2a187eae99640855"]);
}

// ---- Test 12.2: proc_voices2_notes ----
// 2 voices, notes 60+64 on at 0, block=2048. Voices accumulate their output.

#[test]
fn test_12_2_proc_voices2_notes() {
    let mut proc = make_processor(2);
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(64) },
    ];
    let bytes = run_processor(&mut proc, &p, 2048, &events);
    check_processor("proc_voices2_notes", bytes, expect!["raw:b9d88d54a286ff95"]);
}

// ---- Test 12.3: proc_voices2_steal ----
// 2 voices, 3 note-ons (3rd dropped), block=1024.

#[test]
fn test_12_3_proc_voices2_steal() {
    let mut proc = make_processor(2);
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(64) },
        MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(67) },
    ];
    let bytes = run_processor(&mut proc, &p, 1024, &events);
    check_processor("proc_voices2_steal", bytes, expect!["raw:f8a41238a3770059"]);
}

// ---- Test 12.4: proc_voices2_rrobin ----
// 2 voices, round-robin: 60 on (v0), off at 256, 64 on at 512 (v1), 67 on at 512 (v0).

#[test]
fn test_12_4_proc_voices2_rrobin() {
    let mut proc = make_processor(2);
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 0,   channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 256, channel: 1, kind: MidiKind::NoteOff(60) },
        MidiEvent { pos: 512, channel: 1, kind: MidiKind::NoteOn(64) },
        MidiEvent { pos: 512, channel: 1, kind: MidiKind::NoteOn(67) },
    ];
    let bytes = run_processor(&mut proc, &p, 2048, &events);
    check_processor("proc_voices2_rrobin", bytes, expect!["raw:41e37ac295ac6805"]);
}

// ---- Test 13.1: proc_mid_block_note ----
// 1 voice, note 60 on at pos=256. First 256 samples should be silent.

#[test]
fn test_13_1_proc_mid_block_note() {
    let mut proc = make_processor(1);
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 256, channel: 1, kind: MidiKind::NoteOn(60) },
    ];
    let bytes = run_processor(&mut proc, &p, 1024, &events);
    check_processor("proc_mid_block_note", bytes, expect!["raw:52421f9df97f2349"]);
}

// ---- Test 13.2: proc_multi_events ----
// 2 voices, multiple note-on/off events at different positions, block=1024.

#[test]
fn test_13_2_proc_multi_events() {
    let mut proc = make_processor(2);
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 0,   channel: 1, kind: MidiKind::NoteOn(60) },
        MidiEvent { pos: 0,   channel: 1, kind: MidiKind::NoteOn(64) },
        MidiEvent { pos: 256, channel: 1, kind: MidiKind::NoteOff(60) },
        MidiEvent { pos: 512, channel: 1, kind: MidiKind::NoteOn(67) },
        MidiEvent { pos: 768, channel: 1, kind: MidiKind::NoteOff(64) },
    ];
    let bytes = run_processor(&mut proc, &p, 1024, &events);
    check_processor("proc_multi_events", bytes, expect!["raw:1fa229bbdd923945"]);
}

// ---- Test 13.3: proc_odd_block ----
// 1 voice, note 60 on, block=333 (non-power-of-2).

#[test]
fn test_13_3_proc_odd_block() {
    let mut proc = make_processor(1);
    let p = Params::default();
    let events = vec![
        MidiEvent { pos: 0, channel: 1, kind: MidiKind::NoteOn(60) },
    ];
    let bytes = run_processor(&mut proc, &p, 333, &events);
    check_processor("proc_odd_block", bytes, expect!["raw:3dde712bf4472945"]);
}
