use papurs::engine::{MidiEvent, MidiKind, Params, PapuProcessor};
use std::io::Write;
use std::process::{Command, Stdio};

fn f32_to_bytes(samples: &[f32]) -> Vec<u8> {
    samples.iter().flat_map(|&s| s.to_le_bytes()).collect()
}

fn b(v: bool) -> u8 { if v { 1 } else { 0 } }

fn run_cpp(voices: usize, params: &Params, blocks: &[(i32, Vec<MidiEvent>)]) -> Vec<u8> {
    let p = params;
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
    for (block_size, events) in blocks {
        for ev in events {
            let (kind, val) = match &ev.kind {
                MidiKind::NoteOn(n)     => ("NOTE_ON",  *n as i32),
                MidiKind::NoteOff(n)    => ("NOTE_OFF", *n as i32),
                MidiKind::PitchBend(v)  => ("PITCH_BEND", *v as i32),
                MidiKind::AllNotesOff    => ("ALL_NOTES_OFF", 0),
            };
            s += &format!("M {} {} {} {}\n", ev.pos, kind, ev.channel, val);
        }
        s += &format!("B {}\n", block_size);
    }

    let harness_path = format!("{}/tests/cpp_harness/harness", env!("CARGO_MANIFEST_DIR"));
    let mut child = Command::new(&harness_path)
        .arg("PROC")
        .arg(voices.to_string())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn harness");
    child.stdin.take().unwrap().write_all(s.as_bytes()).unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(output.status.success(), "harness failed: {}", String::from_utf8_lossy(&output.stderr));
    output.stdout
}

fn ev(pos: i32, channel: u8, kind: MidiKind) -> MidiEvent {
    MidiEvent { pos, channel, kind }
}

fn to_f32(bytes: &[u8]) -> Vec<f32> {
    bytes.chunks_exact(4)
        .map(|c| f32::from_le_bytes([c[0], c[1], c[2], c[3]]))
        .collect()
}

fn compare_region(label: &str, rust: &[f32], cpp: &[f32]) {
    let n = rust.len().min(cpp.len());
    let diffs: Vec<usize> = (0..n).filter(|&i| rust[i] != cpp[i]).collect();
    if diffs.is_empty() {
        println!("  {}: {} samples, MATCH", label, n);
    } else {
        println!("  {}: {} samples, {} diffs, first at [{}]", label, n, diffs.len(), diffs[0]);
        // Show first 5 diffs
        for &i in diffs.iter().take(5) {
            println!("    [{:5}] rust={:14.8}  cpp={:14.8}", i, rust[i], cpp[i]);
        }
    }
}

fn main() {
    // test_9_2: 2 blocks of 1024
    {
        let events1 = vec![
            ev(0,    1, MidiKind::NoteOn(60)),
            ev(256,  1, MidiKind::NoteOn(64)),
            ev(1024, 1, MidiKind::NoteOff(64)),
        ];
        let events2: Vec<MidiEvent> = vec![];
        let params = Params::default();

        let mut proc = PapuProcessor::new(1);
        proc.prepare(44_100.0);
        let out1 = proc.process_block(1024, &params, &events1);
        let out2 = proc.process_block(1024, &params, &events2);

        let cpp_bytes = run_cpp(1, &params, &[
            (1024, events1),
            (1024, events2),
        ]);
        let cf32 = to_f32(&cpp_bytes);

        println!("\n=== test_9_2 ===");
        println!("Rust out1: {} f32 (block 1)", out1.len());
        println!("Rust out2: {} f32 (block 2)", out2.len());
        println!("C++ total: {} f32", cf32.len());

        // Block 1: out1[0..1024] = L, out1[1024..2048] = R
        // Block 2: out2[0..1024] = L, out2[1024..2048] = R
        // C++ output: cf32[0..1024] = block1 L, cf32[1024..2048] = block1 R,
        //             cf32[2048..3072] = block2 L, cf32[3072..4096] = block2 R

        compare_region("Block1 L", &out1[..1024], &cf32[..1024]);
        compare_region("Block1 R", &out1[1024..2048], &cf32[1024..2048]);
        compare_region("Block2 L", &out2[..1024], &cf32[2048..3072]);
        compare_region("Block2 R", &out2[1024..2048], &cf32[3072..4096]);

        // Extra: show what rust and cpp produce at sample 0 of each block
        println!("\n  Block1 L[0]: rust={:14.8}  cpp={:14.8}", out1[0], cf32[0]);
        println!("  Block1 L[255]: rust={:14.8}  cpp={:14.8}", out1[255], cf32[255]);
        println!("  Block1 L[256]: rust={:14.8}  cpp={:14.8}", out1[256], cf32[256]);

        // Show non-zero ranges in rust block 2
        let b2l = &out2[..1024];
        let b2l_nz: Vec<usize> = (0..1024).filter(|&i| b2l[i] != 0.0 && b2l[i] != -0.00003052_f32).collect();
        println!("\n  Rust Block2 L non-trivial samples: {}", b2l_nz.len());
        if !b2l_nz.is_empty() {
            for &i in b2l_nz.iter().take(5) {
                println!("    [{:5}] = {:14.8}", i, b2l[i]);
            }
        }

        // Show C++ block 2 L
        let cb2l = &cf32[2048..3072];
        let cb2l_nz: Vec<usize> = (0..1024).filter(|&i| cb2l[i] != 0.0 && cb2l[i].abs() > 0.001).collect();
        println!("  C++ Block2 L non-trivial samples: {}", cb2l_nz.len());
        if !cb2l_nz.is_empty() {
            for &i in cb2l_nz.iter().take(5) {
                println!("    [{:5}] = {:14.8}", i, cb2l[i]);
            }
        }

        // Check: does Rust block 1 L go silent at some point?
        let b1l = &out1[..1024];
        let last_active = (0..1024).rev().find(|&i| b1l[i].abs() > 0.001);
        println!("\n  Rust Block1 L: last sample with |v|>0.001: {:?}", last_active);
        let cb1l = &cf32[..1024];
        let cpp_last_active = (0..1024).rev().find(|&i| cb1l[i].abs() > 0.001);
        println!("  C++ Block1 L: last sample with |v|>0.001: {:?}", cpp_last_active);
    }
}
