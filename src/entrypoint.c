// Forward routine registration from C to Rust so the linker keeps the
// static library.
void R_init_icechunkr_extendr(void *dll);

void R_init_icechunkr(void *dll) {
    R_init_icechunkr_extendr(dll);
}
