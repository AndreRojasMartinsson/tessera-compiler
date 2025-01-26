use std::{
    env, fs,
    io::{self},
    path::{Path, PathBuf},
};

fn collect_files(dir: &Path) -> io::Result<Vec<PathBuf>> {
    let mut files = vec![];

    if dir.is_dir() {
        for entry in fs::read_dir(dir)? {
            let path = entry?.path();
            if path.is_dir() {
                files.extend(collect_files(&path)?);
            } else if path.is_file() {
                files.push(path);
            }
        }
    }

    Ok(files)
}

fn main() -> io::Result<()> {
    let std_root_dir = Path::new("include/");
    let mut std_files = collect_files(std_root_dir)?;

    std_files.sort_by_key(|path| path.components().count());

    let mut final_buf = String::new();
    let out_dir = &env::var_os("OUT_DIR").unwrap();
    let resolved_path = Path::new(out_dir).join("_std.tes");

    for file_path in std_files {
        let source = fs::read_to_string(&file_path)?;

        final_buf.push_str(&format!("{}\n", source));
    }

    fs::write(resolved_path, final_buf).unwrap();

    Ok(())
}
