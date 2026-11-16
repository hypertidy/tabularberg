# icechunkr

<!-- badges: start -->
<!-- badges: end -->

R access to [Icechunk](https://icechunk.io) repositories: transactional,
versioned Zarr v3 stores. icechunkr binds the Rust `icechunk` crate
(2.3.1) through [extendr](https://extendr.rs/), and offers two ways
in:

- **Icechunk itself:** repositories, branches, tags and history;
  read-only and writable sessions; Zarr keys; virtual chunk references
  with container authorisation and checksums; commits, rebase and
  conflict handling.
- **The store as tables:** a DBI driver that presents a repository's
  metadata as SQL tables (`arrays`, `sources`, `nodes`, `ancestry`, and
  one `refs_<array>` table per array, a row per chunk). These can be
  queried with SQLite, DuckDB or dbplyr, and appending to them writes
  Icechunk commits.

Data is decoded elsewhere: icechunkr hands a session to
[pizzarr](https://github.com/zarr-developers/pizzarr) (`ic_pizzarr_store()`),
or GDAL reads the same repository (`/vsiicechunk/`).

Experimental. The API may change.

## Install

icechunkr needs a Rust toolchain, rustc 1.95 or newer (the icechunk
crate's minimum).

```r
# install.packages("pak")
pak::pak("hypertidy/icechunkr")
```

Storage backends are chosen at build time with `ICECHUNK_FEATURES`:

| Tier | Storage |
|---|---|
| `minimal` | local file system, HTTP(S) |
| `cloud` (default) | + S3, GCS, Azure |
| `full` | + the AWS SDK S3 backend |

`ic_compiled_features()` reports what a build has.

## A public store as tables

No credentials are needed for this store: NOAA OISST v2.1 daily sea
surface temperature, with each chunk a virtual reference into NOAA's
netCDF files on AWS.

```r
library(icechunkr)
library(DBI)

con <- dbConnect(Icechunk(), "https://data.source.coop/ausantarctic/ausantarctic-scratch/oisst/oisst-v2.1-daily/icechunk")
dbListTables(con)
#>  [1] "meta"      "arrays"    "sources"   "branches"  "tags"      "ancestry"  "nodes"     "refs_anom"
#>  [9] "refs_err"  "refs_ice"  "refs_lat"  "refs_lon"  "refs_sst"  "refs_time" "refs_zlev"

# where is 2024-02-01's sst chunk? Only that chunk's reference is read.
dbGetQuery(con, 'SELECT "offset", size, path FROM refs_sst WHERE dim_0 = 15493')
#>   offset   size                                                 path
#> 1  47587 671857 https://noaa-cdr-sea-surface-temp-optimum-interpolation-pds.s3.amazonaws.com/data/v2.1/avhrr/202402/oisst-avhrr-v02r01.20240201.nc

# how much netCDF does the sst array reference?
dbGetQuery(con, "SELECT count(*) AS chunks, sum(size) / 1e9 AS gigabytes FROM refs_sst")
#>   chunks gigabytes
#> 1  16482  11.03335
```

Filters on chunk indices (`dim_k`) are pushed down to Icechunk, through
joins too. Only the covering chunk references are read, and on stores
with split manifests only the covering manifests. The
`vignette("icechunkr")` vignette continues from here. It covers dbplyr,
a local versioned catalogue with tags and commits, and decoding a day of
data through pizzarr.

## The Icechunk API

```r
repo <- ic_repo_create(tempfile())
s <- ic_session_writable(repo, "main")
ic_set(s, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{}}'))
snap <- ic_commit(s, "an empty group")
ic_ancestry(repo, branch = "main")
```

| Area | Functions |
|---|---|
| Storage | `ic_storage_local()`, `_http()`, `_s3()`, `_gcs()`, `_azure()`, `_memory()` |
| Repositories | `ic_repo_open()`, `ic_repo_create()`, `ic_repo_containers()`, `ic_branches()`, `ic_tags()`, `ic_ancestry()`, `ic_diff()` |
| Sessions | `ic_session_readonly()`, `ic_session_writable()`, `ic_get()`, `ic_get_many()`, `ic_set()`, `ic_list()`, `ic_delete()` |
| Virtual references | `ic_set_virtual_refs()`, `ic_chunk_refs()`, `ic_container()` |
| Commits | `ic_commit()`, `ic_amend()`, `ic_rebase()`, `ic_discard()`, `ic_session_merge()` |
| Byte-reference tables | `ic_to_byterefs()`, `ic_repo_from_byterefs()`, `ic_byterefs_read()`, `ic_write_byterefs()`, `ic_refs_arrow()` |
| SQL | `dbConnect(Icechunk(), ...)`, the DBI generics, dbplyr |
| Other packages | `ic_pizzarr_store()`, `ic_session_bytes()` (pizzarr's `IcechunkStore`) |

Virtual chunk containers must be authorised before their chunks are read:
`authorize = "https://"` covers every declared `https://` container, and
`ic_repo_containers()` lists them.

## Related

- [pizzarr](https://github.com/zarr-developers/pizzarr) decodes arrays.
  The [mdsumner/pizzarr](https://github.com/mdsumner/pizzarr) fork
  (branch `icechunk-handoff`) adds `IcechunkStore` for its zarrs backend,
  and the netCDF-4 shuffle and zlib codecs for its R decoder.
- GDAL's Zarr driver reads Icechunk repositories (`/vsiicechunk/`).
