# Writes src/Makevars from src/Makevars.in.
# ICECHUNK_FEATURES: minimal | cloud (default) | full
# DEBUG: non-empty -> debug build. NOT_CRAN: keep target dir, use the
# user's CARGO_HOME (no re-download on a flaky network).
feat <- Sys.getenv("ICECHUNK_FEATURES", "cloud")
if (!feat %in% c("minimal", "cloud", "full")) stop("ICECHUNK_FEATURES must be minimal, cloud or full")
is_debug <- nzchar(Sys.getenv("DEBUG"))
not_cran <- nzchar(Sys.getenv("NOT_CRAN")) || is_debug
vendor <- file.exists("src/rust/vendor.tar.xz")
rustc <- tryCatch(system2("rustc", "--version", stdout = TRUE), error = function(e) "")
message("icechunk: ", if (length(rustc)) rustc[1] else "rustc not found", "; features = ", feat)
txt <- readLines("src/Makevars.in")
txt <- gsub("@FEATURES@", feat, txt, fixed = TRUE)
txt <- gsub("@PROFILE@", if (is_debug) "" else "--release", txt, fixed = TRUE)
txt <- gsub("@LIBDIR@", if (is_debug) "debug" else "release", txt, fixed = TRUE)
txt <- gsub("@OFFLINE@", if (!not_cran && vendor) "--offline -j 2" else "", txt, fixed = TRUE)
# Under R CMD check (which sets _R_CHECK_PACKAGE_NAME_) always remove the
# Rust target dir after the build, whatever NOT_CRAN says: check otherwise
# scans the leftover static library and build outputs (r-lib's check action
# sets NOT_CRAN=true).
checking <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))
txt <- gsub("@CLEAN_TARGET@", if (!not_cran || checking) "$(TARGET_DIR)" else "", txt, fixed = TRUE)
writeLines(txt, "src/Makevars")
