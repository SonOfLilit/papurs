# Executive Decisions

## Phase 0: Gold Test Scaffolding

### C++ harness design

The harness (`tests/cpp_harness/main.cpp`) replicates PAPUEngine behavior:
- Initialization matches `prepareToPlay()`: treble_eq=-20, bass_freq=461, clock_rate=4194304, sample_rate=44100
- Wave RAM is loaded with preset 0 (waveIndex=0) as in prepareToPlay
- Register writes advance time by 4 clocks each (matches `PAPUEngine::clock()`)
- Rendering uses 1024-clock frames (matches `PAPUEngine::runUntil()`)
- Output: raw little-endian i16 stereo pairs to stdout

### Rust test framework

- Uses FNV-1a 64-bit hash (no external deps, deterministic) for content-addressed fixture filenames
- Fixture files stored in `tests/fixtures/{hash}.raw` (gitignored)
- Snapshots stored via `expect_test` as `"raw:{hash}"`
- C++ harness is compiled on-demand by the test runner (requires `g++`)
- Harness binary (`tests/cpp_harness/harness`) is gitignored

### Scenario simplification for Test 0.2

The `note60` scenario starts the note at time=0 of the first frame (all register writes happen before the first `end_frame` call). This differs slightly from PAPUEngine's `processBlock` which starts notes mid-block in response to MIDI events, but produces identical audio since:
1. The MIDI event is at position 0 in the block
2. The note-on register writes happen at times 4–96 (well before frame end at 1024)

The test verifies bit-exact C++ behavior and is used as the reference for Phase 1+.
