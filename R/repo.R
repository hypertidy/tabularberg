#' Open or create an Icechunk repository
#'
#' @param storage an [ic_storage] object, or a local path.
#' @param containers virtual chunk containers ([ic_container()]) to layer
#'   on the persisted config (use [ic_repo_save_config()] to persist).
#' @param authorize virtual chunk prefixes this session may read: `NULL`
#'   (none), a character vector of prefixes (credentials inferred from the
#'   scheme: s3 from env, http(s) anonymous, file local), or a named list of
#'   prefix = [ic_cred_s3()] etc.
#' @param spec_version format version for new repositories (1 or 2;
#'   `NULL` for the library default).
#' @param manifest_split manifest splitting for new repositories: `NULL`
#'   (one manifest per array), a named integer vector of chunks per
#'   manifest along each dimension, by axis index or dimension name (e.g.
#'   `c(time = 1)` or `c("0" = 30)`; `"*"` for any dimension), applied to
#'   every array, or a list of rules `list(path = <regex or NULL>, dims = <as
#'   before>)` tried in order. Split manifests let a read of a slab (see
#'   [ic_to_byterefs()], the DBI `refs_<array>` tables) fetch only the
#'   manifests that cover it. Persist with [ic_repo_save_config()].
#' @return an `icechunk_repo`.
#' @export
ic_repo_open <- function(storage, containers = NULL, authorize = NULL) {
  storage <- .as_storage(storage)
  ptr <- .ic(ic_repo_open_(storage$ptr, .containers_json(containers), .authorize_json(authorize)))
  .repo(ptr, storage)
}

#' @rdname ic_repo_open
#' @export
ic_repo_create <- function(storage, containers = NULL, authorize = NULL, spec_version = NULL,
                           manifest_split = NULL) {
  storage <- .as_storage(storage)
  sv <- if (is.null(spec_version)) 0L else as.integer(spec_version)
  ptr <- .ic(ic_repo_create_(storage$ptr, .containers_json(containers), .authorize_json(authorize), sv,
                             .split_json(manifest_split)))
  .repo(ptr, storage)
}

#' @rdname ic_repo_open
#' @export
ic_repo_exists <- function(storage) .ic(ic_repo_exists_(.as_storage(storage)$ptr))

#' @rdname ic_repo_open
#' @param repo an `icechunk_repo`.
#' @export
ic_repo_save_config <- function(repo) invisible(.ic(ic_repo_save_config_(repo$ptr)))

#' @rdname ic_repo_open
#' @export
ic_repo_config <- function(repo) jsonlite::fromJSON(ic_repo_config_json_(repo$ptr), simplifyVector = FALSE)

.as_storage <- function(x) {
  if (inherits(x, "icechunk_storage")) return(x)
  if (is.character(x) && length(x) == 1L) {
    if (grepl("^https?://", x)) return(ic_storage_http(x))
    return(ic_storage_local(x))
  }
  stop("storage must be an icechunk_storage or a path", call. = FALSE)
}

.repo <- function(ptr, storage) structure(list(ptr = ptr, storage = storage), class = "icechunk_repo")

#' @export
print.icechunk_repo <- function(x, ...) {
  b <- ic_branches(x)
  t <- ic_tags(x)
  cat("<icechunk_repo> spec v", ic_repo_spec_version_(x$ptr), "\n", sep = "")
  cat("  storage: ", ic_storage_desc_(x$storage$ptr), "\n", sep = "")
  cat("  branches: ", paste(b$name, collapse = ", "), "\n", sep = "")
  if (nrow(t)) cat("  tags: ", paste(t$name, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Branches, tags, ancestry and diffs
#'
#' @param repo an `icechunk_repo`.
#' @param branch,tag,snapshot select one version (default branch "main").
#' @param name branch or tag name.
#' @param snapshot_id a snapshot id.
#' @return data frames for listings; snapshot ids as character.
#' @name ic_refs
#' @export
ic_branches <- function(repo) .df(.ic(ic_branches_(repo$ptr)))

#' @rdname ic_refs
#' @export
ic_tags <- function(repo) .df(.ic(ic_tags_(repo$ptr)))

#' @rdname ic_refs
#' @export
ic_ancestry <- function(repo, branch = NULL, tag = NULL, snapshot = NULL) {
  x <- .df(.ic(ic_ancestry_(repo$ptr, .chr(branch), .chr(tag), .chr(snapshot))))
  x$written_at <- as.POSIXct(x$written_at, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  x
}

#' @rdname ic_refs
#' @export
ic_branch_create <- function(repo, name, snapshot_id) invisible(.ic(ic_branch_create_(repo$ptr, name, snapshot_id)))

#' @rdname ic_refs
#' @param from optional expected current tip (safe reset).
#' @export
ic_branch_reset <- function(repo, name, snapshot_id, from = NULL) {
  invisible(.ic(ic_branch_reset_(repo$ptr, name, snapshot_id, .chr(from))))
}

#' @rdname ic_refs
#' @export
ic_branch_delete <- function(repo, name) invisible(.ic(ic_branch_delete_(repo$ptr, name)))

#' @rdname ic_refs
#' @export
ic_tag_create <- function(repo, name, snapshot_id) invisible(.ic(ic_tag_create_(repo$ptr, name, snapshot_id)))

#' @rdname ic_refs
#' @export
ic_tag_delete <- function(repo, name) invisible(.ic(ic_tag_delete_(repo$ptr, name)))

#' @rdname ic_refs
#' @export
ic_lookup <- function(repo, branch = "main", tag = NULL) .ic(ic_lookup_(repo$ptr, .chr(branch), .chr(tag)))

#' @rdname ic_refs
#' @param from,to lists like `list(branch = "main")`, `list(tag = "v1")`,
#'   `list(snapshot = id)`.
#' @export
ic_diff <- function(repo, from, to) {
  g <- function(x, k) .chr(x[[k]])
  .diff(.ic(ic_diff_(repo$ptr, g(from, "branch"), g(from, "tag"), g(from, "snapshot"),
                     g(to, "branch"), g(to, "tag"), g(to, "snapshot"))))
}

.diff <- function(x) {
  chunks <- .df(list(path = x$chunk_path, coord = x$chunk_coord))
  moved <- .df(list(from = x$moved_from, to = x$moved_to))
  structure(c(x[c("new_groups", "new_arrays", "deleted_groups", "deleted_arrays",
                  "updated_groups", "updated_arrays")],
              list(updated_chunks = chunks, moved = moved)), class = "icechunk_diff")
}

#' @export
print.icechunk_diff <- function(x, ...) {
  cat("<icechunk_diff>\n")
  for (k in c("new_groups", "new_arrays", "deleted_groups", "deleted_arrays",
              "updated_groups", "updated_arrays")) {
    if (length(x[[k]])) cat("  ", k, ": ", paste(x[[k]], collapse = ", "), "\n", sep = "")
  }
  if (nrow(x$updated_chunks)) cat("  updated chunks:", nrow(x$updated_chunks), "\n")
  if (nrow(x$moved)) cat("  moved nodes:", nrow(x$moved), "\n")
  invisible(x)
}

.split_json <- function(x) {
  if (is.null(x) || !length(x)) return("")
  rule <- function(path, dims) {
    if (is.null(names(dims)) || any(!nzchar(names(dims)))) stop("manifest_split dims must be named (axis index, dimension name or \"*\")", call. = FALSE)
    n <- as.integer(dims)
    if (anyNA(n) || any(n < 1L)) stop("manifest_split sizes must be positive integers", call. = FALSE)
    list(path = path, dims = unname(Map(function(k, v) list(k, v), names(dims), n)))
  }
  rules <- if (is.list(x) && !is.null(x[[1]]$dims)) lapply(x, function(r) rule(r$path, r$dims))
           else if (is.list(x) && !is.null(x$dims)) list(rule(x$path, x$dims))
           else list(rule(NULL, x))
  as.character(jsonlite::toJSON(rules, auto_unbox = TRUE, null = "null"))
}
