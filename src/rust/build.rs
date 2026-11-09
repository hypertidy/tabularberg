// Record the resolved icechunk crate version (from Cargo.lock) so the
// session handoff header names exactly what was compiled.
use std::{env, fs, path::Path};

fn main() {
    let dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let lock = Path::new(&dir).join("Cargo.lock");
    println!("cargo:rerun-if-changed={}", lock.display());
    let txt = fs::read_to_string(&lock).unwrap_or_default();
    let mut ver = String::from("unknown");
    let mut in_ic = false;
    for line in txt.lines() {
        if line == "[[package]]" {
            in_ic = false;
        } else if line == "name = \"icechunk\"" {
            in_ic = true;
        } else if in_ic && line.starts_with("version = ") {
            ver = line.trim_start_matches("version = ").trim_matches('"').to_string();
            break;
        }
    }
    println!("cargo:rustc-env=ICECHUNK_CRATE_VERSION={ver}");
}
