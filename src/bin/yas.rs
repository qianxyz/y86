use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process::exit;

use y86::assemble;

use clap::Parser;

#[derive(Parser)]
struct Cli {
    /// `.ys` source file
    source: PathBuf,

    /// `.yo` output file
    #[arg(short, long)]
    output: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    let source = cli.source.clone();
    let output = cli.output.unwrap_or_else(|| {
        let mut p = cli.source.clone();
        p.set_extension("yo");
        p
    });

    let src = fs::read_to_string(source)?;

    let out = match assemble(src.lines()) {
        Ok(out) => out,
        Err(errors) => {
            for e in errors {
                eprintln!("{e}");
            }
            exit(1);
        }
    };

    fs::write(output, out.join("\n"))?;

    Ok(())
}
