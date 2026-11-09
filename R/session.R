#' Sessions
#'
#' A session is a view of one snapshot. Read-only sessions are pinned to a
#' branch tip, tag or snapshot id; writable sessions start from a branch tip
#' and accumulate changes until [ic_commit()].
#'
#' @param repo an `icechunk_repo`.
#' @param branch,tag,snapshot select one version (default branch "main").
#' @return an `icechunk_session`.
#' @export
ic_session_readonly <- function(repo, branch = NULL, tag = NULL, snapshot = NULL) {
  .session(.ic(ic_session_readonly_(repo$ptr, .chr(branch), .chr(tag), .chr(snapshot))))
}

#' @rdname ic_session_readonly
#' @export
ic_session_writable <- function(repo, branch = "main") {
  .session(.ic(ic_session_writable_(repo$ptr, branch)))
}

.session <- function(ptr) structure(list(ptr = ptr), class = "icechunk_session")

#' @rdname ic_session_readonly
#' @param session an `icechunk_session`.
#' @export
ic_session_info <- function(session) ic_session_info_(session$ptr)

#' @export
print.icechunk_session <- function(x, ...) {
  i <- ic_session_info(x)
  cat("<icechunk_session> ", i$mode, if (i$is_fork) " (fork)", "\n", sep = "")
  cat("  snapshot: ", i$snapshot_id, "\n", sep = "")
  if (nzchar(i$branch)) cat("  branch: ", i$branch, "\n", sep = "")
  if (i$has_uncommitted_changes) cat("  uncommitted changes\n")
  invisible(x)
}

#' Zarr key access
#'
#' Raw bytes by Zarr key (`"zarr.json"`, `"temp/zarr.json"`,
#' `"temp/c/0/0/0"`). Decoding is a Zarr reader's job (pizzarr, GDAL).
#'
#' @param session an `icechunk_session`.
#' @param key,keys Zarr keys.
#' @param offset,length optional byte range.
#' @param value a raw vector.
#' @param prefix key prefix; must be a group or array path (icechunk refuses
#'   partial prefixes such as `"x/c"`); `recursive = FALSE` lists one level.
#' @param recursive list all keys below `prefix`.
#' @return `ic_get()` a raw vector or `NULL` if absent (an absent chunk
#'   means fill value); `ic_get_many()` a list of those.
#' @name ic_store
#' @export
ic_get <- function(session, key, offset = NULL, length = NULL) {
  off <- if (is.null(offset)) -1 else as.double(offset)
  len <- if (is.null(length)) -1 else as.double(length)
  .ic(ic_get_(session$ptr, key, off, len))
}

#' @rdname ic_store
#' @export
ic_get_many <- function(session, keys) .ic(ic_get_many_(session$ptr, as.character(keys)))

#' @rdname ic_store
#' @export
ic_set <- function(session, key, value) {
  if (is.character(value)) value <- charToRaw(paste(value, collapse = "\n"))
  invisible(.ic(ic_set_(session$ptr, key, value)))
}

#' @rdname ic_store
#' @export
ic_exists <- function(session, key) .ic(ic_exists_(session$ptr, key))

#' @rdname ic_store
#' @export
ic_delete <- function(session, key) invisible(.ic(ic_delete_(session$ptr, key)))

#' @rdname ic_store
#' @export
ic_list <- function(session, prefix = "", recursive = TRUE) {
  sort(.ic(ic_list_(session$ptr, prefix, if (recursive) "prefix" else "dir")))
}

#' Nodes and chunk references
#'
#' `ic_chunk_refs()` returns one row per stored chunk of a snapshot (or of
#' one array): its Zarr key and where its bytes are. `kind` is `"native"`
#' (bytes in the repository, `location` is `chunks/<id>` relative to the
#' repository root), `"virtual"` (bytes in another object, `location` is its
#' URL) or `"inline"` (bytes in the manifest). Chunks never written are
#' absent (fill value). This is the table a reader needs to fetch chunks
#' itself, and to check a repository against the reference table it was
#' built from.
#'
#' @param session an `icechunk_session`.
#' @param path an array path (`"/temp"`), or `NULL` for all arrays.
#' @param inline include inline chunk bytes as a list column.
#' @return a data frame: path, coord (comma-separated 0-based chunk index),
#'   key, kind, location, offset, length, etag, last_modified (and inline).
#' @export
ic_chunk_refs <- function(session, path = NULL, inline = FALSE) {
  x <- .ic(ic_chunk_refs_(session$ptr, .chr(path), inline))
  if (!inline) x$inline <- NULL
  x <- .df(x)
  x$last_modified[is.nan(x$last_modified)] <- NA
  x$last_modified <- .POSIXct(x$last_modified, tz = "UTC")
  x
}

#' @rdname ic_chunk_refs
#' @export
ic_nodes <- function(session) {
  x <- .df(.ic(ic_nodes_(session$ptr)))
  names(x)[names(x) == "node_type"] <- "type"
  x[order(x$path), , drop = FALSE]
}

#' @rdname ic_chunk_refs
#' @export
ic_virtual_locations <- function(session) sort(.ic(ic_virtual_locations_(session$ptr)))

#' Write virtual chunk references
#'
#' @param session a writable `icechunk_session`.
#' @param key Zarr chunk key.
#' @param location URL of the object holding the bytes.
#' @param offset,length byte range in that object.
#' @param etag,last_modified optional checksum: reads fail if the object
#'   has changed.
#' @param validate check the location falls within a declared container.
#' @export
ic_set_virtual_ref <- function(session, key, location, offset, length,
                               etag = NULL, last_modified = NULL, validate = TRUE) {
  lm <- if (is.null(last_modified)) -1 else as.double(last_modified)
  invisible(.ic(ic_set_virtual_ref_(session$ptr, key, location, as.double(offset),
                                    as.double(length), .chr(etag), lm, validate)))
}

#' @rdname ic_set_virtual_ref
#' @param path array path.
#' @param refs data frame of 0-based chunk indices and byte ranges. Indices
#'   are either `dim_0, dim_1, ...` integer columns (the byterefs layout) or
#'   a `coord` column (list of integer vectors, or comma-separated strings).
#'   Byte ranges are `location` (or `path`), `offset` and `length` (or
#'   `size`). Optional `etag` and `last_modified` (POSIXct or seconds) set a
#'   per-chunk checksum; etag wins when both are present.
#' @return number of refs that failed validation (invisibly).
#' @export
ic_set_virtual_refs <- function(session, path, refs, validate = TRUE) {
  n <- nrow(refs)
  dcols <- grep("^dim_[0-9]+$", names(refs), value = TRUE)
  if (length(dcols)) {
    dcols <- dcols[order(as.integer(sub("dim_", "", dcols)))]
    m <- vapply(dcols, function(d) as.integer(refs[[d]]), integer(n))
    dim(m) <- c(n, length(dcols))
  } else {
    coord <- refs$coord
    if (is.character(coord)) coord <- lapply(strsplit(coord, ","), as.integer)
    m <- do.call(rbind, lapply(coord, as.integer))
    if (is.null(m)) m <- matrix(integer(), 0, 0)
  }
  pick <- function(a, b) if (!is.null(refs[[a]])) refs[[a]] else refs[[b]]
  loc <- as.character(pick("location", "path"))
  len <- as.double(pick("length", "size"))
  etag <- if (is.null(refs$etag)) rep(NA_character_, n) else as.character(refs$etag)
  lm <- if (is.null(refs$last_modified)) rep(NA_real_, n) else as.double(refs$last_modified)
  nfail <- .ic(ic_set_virtual_refs_(session$ptr, path, as.vector(m), ncol(m), loc,
                                    as.double(refs$offset), len, etag, lm, validate))
  invisible(nfail)
}

#' Commit, amend, flush, discard, rebase
#'
#' @param session a writable `icechunk_session`.
#' @param message commit message.
#' @param metadata a named list stored with the snapshot.
#' @param allow_empty commit even with no changes.
#' @param solver `"detect"` fails on any conflict; `"basic"` resolves chunk
#'   conflicts by `on_chunk`.
#' @param on_chunk for `solver = "basic"`: `"ours"`, `"theirs"` or `"fail"`.
#' @return new snapshot id (commit/amend/flush).
#' @details A commit that loses a race raises a condition of class
#'   `icechunk_conflict`; call `ic_rebase()` and commit again.
#' @export
ic_commit <- function(session, message, metadata = NULL, allow_empty = FALSE) {
  .ic(ic_commit_(session$ptr, message, .meta(metadata), "commit", allow_empty))
}

#' @rdname ic_commit
#' @export
ic_amend <- function(session, message, metadata = NULL) {
  .ic(ic_commit_(session$ptr, message, .meta(metadata), "amend", TRUE))
}

#' @rdname ic_commit
#' @export
ic_flush <- function(session, message, metadata = NULL) {
  .ic(ic_commit_(session$ptr, message, .meta(metadata), "flush", TRUE))
}

#' @rdname ic_commit
#' @export
ic_discard <- function(session) invisible(.ic(ic_discard_(session$ptr)))

#' @rdname ic_commit
#' @export
ic_rebase <- function(session, solver = c("detect", "basic"), on_chunk = c("ours", "theirs", "fail")) {
  invisible(.ic(ic_rebase_(session$ptr, match.arg(solver), match.arg(on_chunk))))
}

#' @rdname ic_commit
#' @export
ic_status <- function(session) .diff(.ic(ic_status_(session$ptr)))

.meta <- function(x) if (is.null(x)) "" else as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

#' Hand a session to other native code
#'
#' `ic_session_bytes()` serialises a session (Icechunk's own
#' `Session::as_bytes`) behind a small header: `"ICR1"`, a u16
#' little-endian length, the icechunk crate version, then the payload. Any
#' consumer built against the same icechunk version can rebuild the session
#' (e.g. pizzarr's zarrs backend) and read or write at native speed.
#'
#' For writes, hand out a [ic_session_fork()] of a writable session, take the
#' consumer's bytes back with `ic_session_merge()`, then [ic_commit()] the
#' base session. Commit authority stays here.
#'
#' **The bytes carry credentials.** They hold the repository storage
#' (bucket, prefix, endpoint, and its credentials) and the virtual chunk
#' authorisations the repository was opened with. Static keys given to
#' [ic_storage_s3()] or to [ic_cred_s3()]`("static", ...)` are in the bytes in
#' plain text (verified for icechunk 2.3.1: `S3ObjectStoreBackend` and
#' `S3Storage` serialise their `credentials` field, and so does the virtual
#' chunk resolver). `"from_env"` and `"anonymous"` credentials carry no
#' secret: the consumer resolves them from its own environment. Prefer those
#' when bytes leave the process, and never log or persist bytes made with
#' static keys.
#'
#' @param session an `icechunk_session`.
#' @param bytes a raw vector from `ic_session_bytes()`.
#' @return raw vector; a session; or nothing.
#' @export
ic_session_bytes <- function(session) {
  structure(.ic(ic_session_bytes_(session$ptr)), class = "icechunk_session_bytes")
}

#' @rdname ic_session_bytes
#' @export
ic_session_from_bytes <- function(bytes) .session(.ic(ic_session_from_bytes_(unclass(bytes))))

#' @rdname ic_session_bytes
#' @export
ic_session_fork <- function(session) .session(.ic(ic_session_fork_(session$ptr)))

#' @rdname ic_session_bytes
#' @export
ic_session_merge <- function(session, bytes) invisible(.ic(ic_session_merge_(session$ptr, unclass(bytes))))

#' @export
print.icechunk_session_bytes <- function(x, ...) {
  n <- readBin(x[5:6], "integer", size = 2, signed = FALSE, endian = "little")
  cat("<icechunk_session_bytes> ", length(x), " bytes, icechunk ",
      rawToChar(x[7:(6 + n)]), "\n", sep = "")
  invisible(x)
}

#' Build information
#' @return `ic_compiled_features()` the Cargo features in this build;
#'   `ic_icechunk_version()` the icechunk crate version.
#' @export
ic_compiled_features <- function() ic_compiled_features_()

#' @rdname ic_compiled_features
#' @export
ic_icechunk_version <- function() ic_icechunk_version_()

#' Concurrent manifest fetches
#'
#' Reading an array's chunk references ([ic_chunk_refs()],
#' [ic_to_byterefs()], [ic_refs_arrow()], the DBI `refs_<array>` tables)
#' needs every manifest that covers it. With split manifests on remote
#' storage, fetching them one after another dominates; icechunkr first
#' loads them into the session's cache this many at a time.
#'
#' @param n manifests fetched at once (1 = sequential).
#' @return the previous value (invisibly).
#' @export
ic_manifest_concurrency <- function(n = 32L) invisible(ic_set_manifest_concurrency_(as.integer(n)))
