use anyhow::{bail, Ok};
use clap::Parser;
use jeol_parser_core::parse_from_filepath;
use std::{
    fs::File,
    path::{Path, PathBuf},
};

#[derive(Parser, Debug)]
#[command(author, version, about = None, long_about = None, )]
struct Args {
    /// Path to the input file
    #[arg(short, long)]
    input: PathBuf,

    /// Path to the output file
    #[arg(short, long)]
    output: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let input_path = args.input;
    let to_file = args.output.is_some();
    let mut output_path = args.output.unwrap_or_default();

    match input_path.extension() {
        Some(v) => {
            if v.to_str().unwrap_or_default().to_lowercase() != "jdf" {
                bail!(format!(
                    "Wrong extension type. Only .jdf files are supported, got a .{} file instead.",
                    v.to_str().unwrap_or_default()
                ))
            }
        }
        None => {
            bail!("Input file has no valid extension");
        }
    }

    if to_file {
        match output_path.extension() {
            Some(v) => {
                let str_extension = v.to_str().unwrap_or_default().to_lowercase();
                if str_extension != "json" {
                    bail!(format!(
                        "Wrong output file extension. Only .json files are supported, got a .{} file instead.",
                        str_extension
                    ))
                }
            }
            None => {
                output_path.set_extension("json");
            }
        };
    }

    let res = parse_from_filepath(Path::new(&input_path))?;

    if to_file {
        serde_json::to_writer(&File::create(&output_path)?, &res)?;
    } else {
        print!("{:?}", res)
    }
    Ok(())
}
