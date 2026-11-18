// Shared library of the build without Rust, for WebAssembly (webR).
// tools/config.R writes a src/Makevars that compiles only this file on
// webR, where Icechunk cannot be built (see tools/config.R). No routines
// are registered, so the .Call() wrappers in R/extendr-wrappers.R do not
// resolve there.

#include <R.h>
#include <R_ext/Rdynload.h>

void R_init_tabularberg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
