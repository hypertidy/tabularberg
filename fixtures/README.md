# tabularberg test fixtures

Small Icechunk repositories written by icechunk-python, which the tests in
`tests/testthat/test-fixtures-read.R` read back and compare byte for byte.
Outputs (`repos/`, `expected/`, `manifest.json`) are regenerated, not
committed. Without them those tests skip.

From the repository root (this is what CI runs):

```
uv run --no-project --python 3.12 --with "icechunk==2.3.*" --with "zarr>=3" --with numpy --with h5py \
  python -I fixtures/make_fixtures.py
uv run --no-project --python 3.12 --with "icechunk<2" --with "zarr>=3" --with numpy \
  python -I fixtures/make_fixtures.py --v1
```

Run the 2.x step first; `--v1` adds `native_v1` to the same manifest.
`devtools::test()` finds `fixtures/` by itself; under `R CMD check` set
`ICECHUNK_R_FIXTURES` to this directory's absolute path.

| fixture | what |
|---|---|
| native_v2 | `bench_f32` (64 one-MiB float32 chunks, no codec; values `arange(4096*4096) % 1000`), `bench_f32_zstd`, `int16_fill` (sparse, fill -999), `uint8_2d` (ragged edges), `sharded_3d`, `nested/vec`, `strings` |
| native_v1 | format v1, written by icechunk 1.x |
| native_spec1_by_v2 | format v1, written by icechunk 2.3 |
| inline_v2 | chunks inline in manifests (threshold 4096 B) |
| virtual_v2 | virtual refs into local files (`repos/virtual_sources/`: raw .bin and a contiguous NetCDF4 variable); container `file://<abs path>/` |
| history_v2 | c1, c2 (tag `v1`), c3 amended, branch `dev` from c2 |

`expected/<fixture>/keys.json`: sha256 and length of every Zarr key, read
through icechunk-python's store, for every branch and tag. That is the
byte-level ground truth. `expected/<fixture>/*.npy`: decoded arrays.

## Remote, real-world (read-only, public)

source.coop `s3://ausantarctic/ausantarctic-scratch/` (endpoint
`https://data.source.coop`, anonymous, region `us-west-2`), built by
raad.virtualize. Repos at `<product>/<dataset>/icechunk/`:

access-esm/access-esm1-6.historical.ocean-3d-temp-1mon-mean,
bluelink/bluelink-daily-{eta_t,mld,salt,temp,tx_trans_int_z,ty_trans_int_z,u,v,w},
ghrsst/ghrsst-mur-cog-analysed_sst,
oceancolour/esacci-oc-v6.0-{5day,monthly}-{chlor_a,chlor_a_log10_bias,chlor_a_log10_rmsd},
oisst/oisst-v2.1-daily, seaice/asi-amsr2-v5.4-daily-south,
seaice/g02202-v6-daily-{north,south}.
