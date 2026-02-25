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
