# Writes src/Makevars from src/Makevars.in (src/Makevars.win from
# src/Makevars.win.in on Windows, via configure.win).
# ICECHUNK_FEATURES: minimal | cloud (default) | full
# DEBUG: non-empty -> debug build. NOT_CRAN: keep target dir, use the
# user's CARGO_HOME (no re-download on a flaky network).
# WebAssembly (webR, for which r-universe builds binaries) is not supported:
# Icechunk's network stack needs tokio's `net` feature and `mio` stops with
# "This wasm target is unsupported by mio" on wasm32-unknown-emscripten. So
# there, src/Makevars builds only src/wasm/init.c (no Rust needed) and the
# package installs without its native library. Checked before anything else.
if (identical(R.version$platform, "wasm32-unknown-emscripten")) {
  message("tabularberg does not support WebAssembly (webR): building without its Rust library.")
  writeLines(c("# Written by tools/config.R for WebAssembly (webR): no Rust library.",
               "OBJECTS = wasm/init.o"), "src/Makevars")
  quit(save = "no", status = 0L)
}
feat <- Sys.getenv("ICECHUNK_FEATURES", "cloud")
if (!feat %in% c("minimal", "cloud", "full")) stop("ICECHUNK_FEATURES must be minimal, cloud or full")
is_debug <- nzchar(Sys.getenv("DEBUG"))
not_cran <- nzchar(Sys.getenv("NOT_CRAN")) || is_debug
vendor <- file.exists("src/rust/vendor.tar.xz")
rustc <- tryCatch(system2("rustc", "--version", stdout = TRUE), error = function(e) "")
message("icechunk: ", if (length(rustc)) rustc[1] else "rustc not found", "; features = ", feat)
win <- .Platform$OS.type == "windows"
mk <- if (win) "src/Makevars.win" else "src/Makevars"
txt <- readLines(paste0(mk, ".in"))
txt <- gsub("@FEATURES@", feat, txt, fixed = TRUE)
txt <- gsub("@PROFILE@", if (is_debug) "" else "--release", txt, fixed = TRUE)
txt <- gsub("@LIBDIR@", if (is_debug) "debug" else "release", txt, fixed = TRUE)
txt <- gsub("@OFFLINE@", if (!not_cran && vendor) "--offline -j 2" else "", txt, fixed = TRUE)
# Keep the Rust target dir (incremental rebuilds) only for a development
# install: NOT_CRAN set and the package sits in a git checkout. Builds from
# a tarball (R CMD check, r-universe, CRAN) always clean up, whatever
# NOT_CRAN says (r-lib's check action sets NOT_CRAN=true, and a leftover
# target dir makes check scan the static library and build outputs).
in_checkout <- dir.exists(".git") || dir.exists("../.git")
keep_target <- not_cran && in_checkout
# macOS: C code built by crates (aws-lc) otherwise targets the SDK version,
# which can be newer than the version R links for, and check WARNs ("built
# for newer 'macOS' version (26.5) than being linked (26.0)"). Unless the
# builder set a target, use rustc's own default, as the Rust code does.
mac_target <- ""
if (Sys.info()[["sysname"]] == "Darwin" && !nzchar(Sys.getenv("MACOSX_DEPLOYMENT_TARGET"))) {
  dt <- tryCatch(system2("rustc", c("--print", "deployment-target"), stdout = TRUE, stderr = FALSE),
                 error = function(e) character(), warning = function(w) character())
  # rustc prints "MACOSX_DEPLOYMENT_TARGET=11.0" (older: "deployment_target=11.0")
  dt <- sub("^[^=]*=", "", grep("^(MACOSX_DEPLOYMENT_TARGET|deployment_target)=", dt, value = TRUE))
  if (length(dt) && nzchar(dt[1])) {
    message("icechunk: MACOSX_DEPLOYMENT_TARGET=", dt[1], " (rustc default)")
    mac_target <- paste0("export MACOSX_DEPLOYMENT_TARGET=", dt[1], " && ")
  }
}
txt <- gsub("@MACOS_TARGET@", mac_target, txt, fixed = TRUE)
txt <- gsub("@CLEAN_TARGET@", if (!keep_target) "$(TARGET_DIR)" else "", txt, fixed = TRUE)
writeLines(txt, mk)
