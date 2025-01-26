use std::path::PathBuf;

use clap::{Parser, Subcommand};
use clap_verbosity_flag::Verbosity;

#[derive(Parser)]
struct Args {
    #[command(flatten)]
    verbose: Verbosity,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    Run {
        /// Path to the file TESC should compile
        file_name: PathBuf,

        /// Path to the directory TESC should emit build artifacts
        #[arg(short = 'o', long = None, default_value = "build")]
        output_dir: PathBuf,

        /// Whether or not to print IR
        #[arg(long = "print-ir", default_value_t = false)]
        print_ir: bool,

        /// Whether or not to emit ASM
        #[arg(long = "emit-asm", default_value_t = false)]
        emit_asm: bool,

        /// Whether or not to emit IR
        #[arg(long = "emit-ir", default_value_t = false)]
        emit_ir: bool,
    },
    Build {
        /// Path to the file TESC should compile
        file_name: PathBuf,

        /// Path to the directory TESC should emit build artifacts
        #[arg(short = 'o', long = None, default_value = "build")]
        output_dir: PathBuf,

        /// Whether or not to print IR
        #[arg(long = "print-ir", default_value_t = false)]
        print_ir: bool,

        /// Whether or not to emit ASM
        #[arg(long = "emit-asm", default_value_t = false)]
        emit_asm: bool,

        /// Whether or not to emit IR
        #[arg(long = "emit-ir", default_value_t = false)]
        emit_ir: bool,
    },
}

pub fn parse_args() -> Command {
    let args = Args::parse();

    env_logger::Builder::new()
        .filter_level(args.verbose.log_level_filter())
        .init();

    args.command
}
