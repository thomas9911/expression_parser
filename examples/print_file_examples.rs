use std::fs::{read, read_dir, DirEntry};
use std::io::Result;

const DIR: &'static str = "examples/file";

fn main() -> Result<()> {
    let paths = collect_paths()?;
    let mut output = String::new();

    for entry in paths {
        output.push_str(&format!("## {}\n\n", format_title(&entry)));
        let file_contents = String::from_utf8(read(entry.path())?).expect("invalid file contents");
        output.push_str(&format!("```\n{}\n```\n\n", file_contents))
    }

    println!("{}", output);
    Ok(())
}

fn collect_paths() -> Result<Vec<DirEntry>> {
    let mut paths = read_dir(DIR)?.try_fold::<_, _, Result<Vec<_>>>(Vec::new(), |mut acc, x| {
        acc.push(x?);
        Ok(acc)
    })?;
    paths.sort_by_key(|x| x.path());
    Ok(paths)
}

fn format_title(entry: &DirEntry) -> String {
    let path = entry.path();
    let mut x = path
        .file_stem()
        .unwrap()
        .to_str()
        .expect("file name is not utf8")
        .to_string();
    if let Some(first_char) = x.get_mut(..1) {
        first_char.make_ascii_uppercase()
    };
    return x;
}
