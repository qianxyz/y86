use std::fs;
use std::path::PathBuf;

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

fn main() {
    let cli = Cli::parse();

    let source = cli.source.clone();
    let output = cli.output.unwrap_or_else(|| {
        let mut p = cli.source.clone();
        p.set_extension("yo");
        p
    });

    let src = fs::read_to_string(source).unwrap();

    // TODO: print error messages for errors
    let out = assemble(src.lines()).unwrap();

    fs::write(output, out.join("\n")).unwrap();
}
