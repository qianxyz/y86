use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::exit;

use y86::{decode, VM};

use clap::Parser;

#[derive(Parser)]
struct Cli {
    /// `.yo` object file
    object: PathBuf,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    let object = cli.object;

    let obj = fs::read_to_string(object)?;

    let mem = decode(obj.lines()).unwrap_or_else(|errors| {
        for e in errors {
            eprintln!("{e}");
        }
        exit(1)
    });

    let mut vm = VM::from_memory(mem);
    let original = vm.clone();

    vm.run();

    println!(
        "Stopped in {} steps at PC = {:#x}.  Status '{}', CC Z={} S={} O={}",
        vm.nsteps, vm.old_pc, vm.stat, vm.zf as u8, vm.sf as u8, vm.of as u8
    );

    println!("Changes to registers:");

    for ((new, old), name) in vm.registers.iter().zip(original.registers).zip([
        "%rax", "%rcx", "%rdx", "%rbx", "%rsp", "%rbp", "%rsi", "%rdi", "%r8", "%r9", "%r10",
        "%r11", "%r12", "%r13", "%r14",
    ]) {
        if *new != old {
            println!("{}:\t{:#016x}\t{:#016x}", name, old, new);
        }
    }

    println!();
    println!("Changes to memory:");

    for ((new, old), addr) in vm
        .mem
        .chunks_exact(8)
        .zip(original.mem.chunks_exact(8))
        .zip((0usize..).step_by(8))
    {
        if new != old {
            println!(
                "{:#03x}:\t{:#016x}\t{:#016x}",
                addr,
                u64::from_le_bytes(old.try_into().unwrap()),
                u64::from_le_bytes(new.try_into().unwrap()),
            );
        }
    }

    Ok(())
}
