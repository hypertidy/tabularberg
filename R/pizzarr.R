# WP3: a pizzarr store backed by an icechunkr session, for pizzarr's pure-R
# code path (CRAN pizzarr, no Rust). All I/O goes through ic_get / ic_set /
# ic_list / ic_delete; pizzarr never sees Icechunk.

#' A pizzarr store over an Icechunk session
#'
#' Returns an R6 store that pizzarr's pure-R code path accepts (it inherits
#' `pizzarr::Store`), so pizzarr without its Rust backend, as released on
#' CRAN, can read and write an Icechunk repository. Every key read or
#' written goes through the session: reads see the session's snapshot plus
#' its uncommitted changes, and writes stay in the session until
#' [ic_commit()].
#'
#' The store reports no store identifier, so pizzarr always uses its R
#' code path with it (for pizzarr's zarrs backend, use pizzarr's own
#' `IcechunkStore`, which takes the session as bytes).
#'
#' Limits come from pizzarr's R decoder, not from Icechunk: Zarr v3 arrays
#' whose codecs it does not implement (`sharding_indexed`, `numcodecs.*` such
#' as `numcodecs.zlib` and `numcodecs.shuffle`) cannot be read this way;
#' [ic_get()] still returns their raw chunk bytes.
#'
#' @param session an `icechunk_session` (read-only, or writable to write).
#' @return an R6 object inheriting `pizzarr::Store`.
#' @examples
#' \dontrun{
#' s <- ic_session_writable(ic_repo_open("path/to/repo"), "main")
#' st <- ic_pizzarr_store(s)
#' z <- pizzarr::zarr_open(st, mode = "r", path = "sst")
#' z$get_item("...")
#' }
#' @export
ic_pizzarr_store <- function(session) {
  .need("pizzarr"); .need("R6")
  if (!inherits(session, "icechunk_session")) stop("session must be an icechunk_session", call. = FALSE)
  .pz_class()$new(session)
}

.pz_env <- new.env(parent = emptyenv())

# v2 metadata keys pizzarr probes for; Icechunk (Zarr v3 only) refuses them
.pz_v2_key <- function(key) grepl("(^|/)[.]z(array|group|attrs|metadata)$", key)

.pz_prefix <- function(path) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) return("")
  sub("/+$", "", sub("^/+", "", path))
}

.pz_class <- function() {
  if (!is.null(.pz_env$cls)) return(.pz_env$cls)
  .pz_env$cls <- R6::R6Class("IcechunkrStore",
    inherit = pizzarr::Store,
    private = list(sess = NULL, snapshot = NULL),
    public = list(
      initialize = function(session) {
        super$initialize()
        info <- ic_session_info(session)
        private$sess <- session
        private$snapshot <- info$snapshot_id
        private$store_version <- 3
        private$writeable <- !isTRUE(info$read_only)
        private$erasable <- private$writeable
        private$listable <- TRUE
      },
      get_item = function(key) {
        if (.pz_v2_key(key)) stop("KeyError: ", key)
        v <- ic_get(private$sess, key)
        if (is.null(v)) stop("KeyError: ", key)
        v
      },
      set_item = function(key, value) {
        if (!private$writeable) stop("Icechunk session is read-only")
        ic_set(private$sess, key, as.raw(value))
        invisible(NULL)
      },
      contains_item = function(key) {
        if (.pz_v2_key(key)) return(FALSE)
        isTRUE(tryCatch(ic_exists(private$sess, key), error = function(e) FALSE))
      },
      delete_item = function(key) {
        if (!private$writeable) stop("Icechunk session is read-only")
        if (isTRUE(ic_exists(private$sess, key))) ic_delete(private$sess, key)
        invisible(NULL)
      },
      listdir = function(path = NA) {
        out <- tryCatch(ic_list(private$sess, .pz_prefix(path), recursive = FALSE),
                        error = function(e) character())
        sort(as.character(out))
      },
      rmdir = function(path = NA) {
        if (!private$writeable) stop("Icechunk session is read-only")
        p <- .pz_prefix(path)
        keys <- tryCatch(ic_list(private$sess, p, recursive = TRUE), error = function(e) character())
        if (!length(keys)) return(invisible(NULL))
        # ic_list returns full keys; deleting a node's zarr.json removes the
        # node, so go deepest first
        full <- keys[order(-nchar(gsub("[^/]", "", keys)), keys)]
        for (k in full) tryCatch(if (isTRUE(ic_exists(private$sess, k))) ic_delete(private$sess, k),
                                 error = function(e) NULL)
        invisible(NULL)
      },
      get_store_identifier = function() NULL,
      session = function() private$sess,
      print = function(...) {
        cat("<IcechunkrStore> snapshot ", private$snapshot,
            if (private$writeable) " (writable)" else " (read-only)", "\n", sep = "")
        invisible(self)
      }
    )
  )
  .pz_env$cls
}

# R6 method bodies refer to these; R CMD check cannot see R6 binds them
utils::globalVariables(c("self", "private", "super"))
