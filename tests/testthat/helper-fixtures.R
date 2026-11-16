fixture_root <- function() {
  p <- Sys.getenv("ICECHUNK_R_FIXTURES", "")
  if (!nzchar(p)) p <- normalizePath(file.path(testthat::test_path(), "..", "..", "fixtures"), mustWork = FALSE)
  p
}
skip_if_no_fixtures <- function() {
  if (!file.exists(file.path(fixture_root(), "manifest.json"))) skip("fixtures not built (see fixtures/make_fixtures.py)")
}
manifest <- function() jsonlite::fromJSON(file.path(fixture_root(), "manifest.json"), simplifyVector = FALSE)
fx_path <- function(name) file.path(fixture_root(), manifest()$fixtures[[name]]$path)
fx_keys <- function(name) jsonlite::fromJSON(file.path(fixture_root(), "expected", name, "keys.json"), simplifyVector = FALSE)
sha <- function(raw) paste(as.character(openssl::sha256(raw)))
list2 <- function(prefix) stats::setNames(list(icechunkr::ic_cred_local()), prefix)
