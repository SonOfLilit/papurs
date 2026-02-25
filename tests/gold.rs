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
