# DBI interface: an Icechunk repository as a database of byterefs tables.
#
#   con <- dbConnect(icechunkr::Icechunk(), "path/to/repo", branch = "main")
#   dbListTables(con)                       # meta, arrays, sources, refs_<array>,
#                                           # branches, tags, ancestry, nodes
#   dbReadTable(con, "refs_sst")            # one row per chunk (byterefs layout)
#   dbGetQuery(con, "SELECT path, count(*) FROM refs_sst GROUP BY path")
#   dbBegin(con); dbAppendTable(con, "refs_sst", rows); dbCommit(con, "add day")
#
# A connection is pinned to a branch (readable and writable), a tag or a
# snapshot (read only): time travel is a connection argument. Writes run in
# a writable session; dbCommit() is an Icechunk commit, dbRollback() discards.
# Outside a transaction each write commits on its own (DBI autocommit).

#' @import DBI
#' @importFrom methods new setClass setMethod setGeneric show is
NULL

#' Icechunk DBI driver
#'
#' An Icechunk repository seen through DBI. Tables are the byterefs
#' projection of the snapshot the connection is pinned to (see
#' [ic_to_byterefs()]): `meta`, `arrays`, `sources` and one `refs_<array>`
#' per data array, plus Icechunk's own `branches`, `tags`, `ancestry` and
#' `nodes`. SQL ([DBI::dbGetQuery()]) runs on an in-memory SQLite copy of
#' the tables a statement names (requires RSQLite).
#'
#' Writes go through a writable session on the connection's branch:
#' [DBI::dbBegin()] opens it, [DBI::dbAppendTable()] on `refs_<array>` sets
#' virtual (or inline, from `data`) chunk refs, on `arrays` creates arrays,
#' [DBI::dbCommit()] commits (pass `message`), [DBI::dbRollback()]
#' discards. Without `dbBegin()` each append commits on its own.
#'
#' @return `Icechunk()`: an `IcechunkDriver`.
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(Icechunk(), "path/to/repo")
#' DBI::dbListTables(con)
#' DBI::dbGetQuery(con, "SELECT path, count(*) AS n, sum(size) AS bytes
#'                       FROM refs_sst GROUP BY path")
#' old <- DBI::dbConnect(Icechunk(), "path/to/repo", snapshot = "QJ0TJMNQT5Q5MY5395J0")
#' }
#' @export
Icechunk <- function() new("IcechunkDriver")

#' @rdname Icechunk
#' @export
setClass("IcechunkDriver", contains = "DBIDriver")

#' @rdname Icechunk
#' @export
setClass("IcechunkConnection", contains = "DBIConnection", slots = c(state = "environment"))

#' @rdname Icechunk
#' @export
setClass("IcechunkResult", contains = "DBIResult", slots = c(state = "environment"))

#' @rdname Icechunk
#' @param drv an `IcechunkDriver`.
#' @param storage an [ic_storage] object or a local path.
#' @param branch,tag,snapshot what the connection is pinned to (one of them;
#'   default branch `"main"`). Only a branch connection can write.
#' @param authorize,containers as for [ic_repo_open()].
#' @param create create the repository if it does not exist.
#' @param target remap target used to resolve `path` on writes (default:
#'   the one recorded by [ic_write_byterefs()], if any).
#' @param ... unused.
#' @export
setMethod("dbConnect", "IcechunkDriver", function(drv, storage, branch = NULL, tag = NULL, snapshot = NULL,
                                                  authorize = NULL, containers = NULL, create = FALSE,
                                                  target = NULL, ...) {
  if (sum(!is.null(branch), !is.null(tag), !is.null(snapshot)) > 1L) {
    stop("pin the connection to one of branch, tag or snapshot", call. = FALSE)
  }
  if (is.null(tag) && is.null(snapshot) && is.null(branch)) branch <- "main"
  st <- .as_storage(storage)
  repo <- if (create && !ic_repo_exists(st)) {
    ic_repo_create(st, containers = containers, authorize = authorize)
  } else ic_repo_open(st, containers = containers, authorize = authorize)
  state <- new.env(parent = emptyenv())
  state$repo <- repo; state$branch <- branch; state$tag <- tag; state$snapshot <- snapshot
  state$target <- target; state$in_tx <- FALSE; state$valid <- TRUE; state$cache <- list()
  state$session <- .dbi_readonly(state)
  new("IcechunkConnection", state = state)
})

.dbi_readonly <- function(state) {
  ic_session_readonly(state$repo, branch = state$branch, tag = state$tag, snapshot = state$snapshot)
}

.dbi_check <- function(conn) {
  if (!isTRUE(conn@state$valid)) stop("connection is closed", call. = FALSE)
  invisible(conn@state)
}

.dbi_invalidate <- function(state) state$cache <- list()

.dbi_root <- function(state) {
  state$cache$root %||% (state$cache$root <- .br_root(state$session))
}

.dbi_arrays <- function(state) {
  if (!is.null(state$cache$array_rows)) return(state$cache$array_rows)
  nodes <- ic_nodes(state$session)
  nodes <- nodes[nodes$type == "array", , drop = FALSE]
  root <- .dbi_root(state)
  res <- lapply(nodes$path, function(p) .br_array(state$session, p, root, native = TRUE))
  names(res) <- vapply(res, function(x) x$arow$array, "")
  state$cache$array_rows <- res
  res
}

.dbi_data_arrays <- function(state) {
  a <- .dbi_arrays(state)
  names(a)[vapply(a, function(x) identical(x$arow$kind, "data"), logical(1))]
}

.DBI_FIXED <- c("meta", "arrays", "sources", "branches", "tags", "ancestry", "nodes")

.dbi_tables <- function(state) {
  nodes <- ic_nodes(state$session)
  arrs <- sub("^/", "", nodes$path[nodes$type == "array"])
  c(.DBI_FIXED, paste0("refs_", arrs))
}

.dbi_read <- function(state, name) {
  name <- as.character(name)
  if (!is.null(state$cache$tables[[name]])) return(state$cache$tables[[name]])
  repo <- state$repo
  out <- switch(name,
    meta = {
      m <- .dbi_root(state)$meta
      data.frame(key = names(m), value = unlist(m, use.names = FALSE), stringsAsFactors = FALSE)
    },
    arrays = {
      rows <- lapply(.dbi_arrays(state), function(x) {
        v <- x$arow$values; x$arow$values <- NULL
        d <- as.data.frame(x$arow, stringsAsFactors = FALSE); d$values <- blob_list(v); d
      })
      if (length(rows)) do.call(rbind, rows) else data.frame()
    },
    sources = {
      r <- do.call(rbind, lapply(.dbi_arrays(state), function(x) {
        if (is.null(x$refs)) NULL else x$refs[!is.na(x$refs$path), c("path", "etag", "mtime")]
      }))
      if (is.null(r)) r <- data.frame(path = character(), etag = character(), mtime = numeric())
      u <- r[!duplicated(r$path), , drop = FALSE]
      u <- u[order(u$path), , drop = FALSE]
      data.frame(source_id = seq_len(nrow(u)), source = u$path, size = rep(NA_real_, nrow(u)),
                 mtime = u$mtime, etag = u$etag, stringsAsFactors = FALSE)
    },
    branches = ic_branches(repo),
    tags = ic_tags(repo),
    ancestry = ic_ancestry(repo, branch = state$branch, tag = state$tag, snapshot = state$snapshot),
    nodes = {
      n <- ic_nodes(state$session)
      for (f in names(n)) if (is.list(n[[f]])) n[[f]] <- vapply(n[[f]], function(v) paste(v, collapse = ","), "")
      n
    },
    {
      if (!startsWith(name, "refs_")) stop("no table '", name, "'", call. = FALSE)
      nm <- substring(name, 6L)
      a <- .dbi_arrays(state)[[nm]]
      if (is.null(a)) stop("no array '", nm, "'", call. = FALSE)
      r <- a$refs
      nd <- length(jsonlite::fromJSON(a$arow$shape))
      if (is.null(r)) {
        r <- as.data.frame(stats::setNames(rep(list(integer()), nd), paste0("dim_", seq_len(nd) - 1L)))
        r$present <- integer(); r$path <- character(); r$offset <- numeric(); r$size <- numeric()
        r$info <- character(); r$data <- list()
      }
      r <- r[, c(paste0("dim_", seq_len(nd) - 1L), "present", "path", "offset", "size", "info", "data")]
      r$data <- blob_list(r$data)
      r
    })
  rownames(out) <- NULL
  state$cache$tables[[name]] <- out
  out
}

#' @rdname Icechunk
#' @param conn an `IcechunkConnection`.
#' @param name table name.
#' @export
setMethod("dbListTables", "IcechunkConnection", function(conn, ...) .dbi_tables(.dbi_check(conn)))

#' @rdname Icechunk
#' @export
setMethod("dbExistsTable", c("IcechunkConnection", "character"), function(conn, name, ...) {
  name %in% .dbi_tables(.dbi_check(conn))
})

#' @rdname Icechunk
#' @export
setMethod("dbReadTable", c("IcechunkConnection", "character"), function(conn, name, ...) {
  .dbi_read(.dbi_check(conn), name)
})

#' @rdname Icechunk
#' @export
setMethod("dbListFields", c("IcechunkConnection", "character"), function(conn, name, ...) {
  names(.dbi_read(.dbi_check(conn), name))
})

# ---- SQL ------------------------------------------------------------------------

.dbi_sql_tables <- function(state, statement) {
  tabs <- .dbi_tables(state)
  hit <- vapply(tabs, function(t) {
    q <- paste0("\\Q", t, "\\E")
    grepl(paste0("(^|[^A-Za-z0-9_])[\"`\\[]?", q, "[\"`\\]]?([^A-Za-z0-9_/]|$)"), statement, perl = TRUE)
  }, logical(1))
  tabs[hit]
}

.dbi_sql <- function(state, statement) {
  .need("RSQLite")
  if (is.null(state$sql) || !DBI::dbIsValid(state$sql)) {
    state$sql <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    state$sql_loaded <- character()
  }
  key <- state$cache$key %||% (state$cache$key <- paste(ic_session_info(state$session)$snapshot_id, stats::runif(1)))
  if (!identical(state$sql_key, key)) {
    for (t in DBI::dbListTables(state$sql)) DBI::dbRemoveTable(state$sql, t)
    state$sql_loaded <- character(); state$sql_key <- key
  }
  for (t in setdiff(.dbi_sql_tables(state, statement), state$sql_loaded)) {
    d <- .dbi_read(state, t)
    for (f in names(d)) if (inherits(d[[f]], "POSIXct")) d[[f]] <- format(d[[f]], "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    DBI::dbWriteTable(state$sql, t, d)
    state$sql_loaded <- c(state$sql_loaded, t)
  }
  state$sql
}

#' @rdname Icechunk
#' @param statement SQL (read-only: SELECT over the tables it names).
#' @export
setMethod("dbGetQuery", c("IcechunkConnection", "character"), function(conn, statement, ...) {
  state <- .dbi_check(conn)
  DBI::dbGetQuery(.dbi_sql(state, statement), statement, ...)
})

#' @rdname Icechunk
#' @export
setMethod("dbSendQuery", c("IcechunkConnection", "character"), function(conn, statement, ...) {
  state <- .dbi_check(conn)
  rs <- new.env(parent = emptyenv())
  rs$res <- DBI::dbSendQuery(.dbi_sql(state, statement), statement, ...)
  rs$statement <- statement
  new("IcechunkResult", state = rs)
})

#' @rdname Icechunk
#' @param res an `IcechunkResult`.
#' @param n rows to fetch (-1 for all).
#' @export
setMethod("dbFetch", "IcechunkResult", function(res, n = -1, ...) DBI::dbFetch(res@state$res, n = n, ...))

#' @rdname Icechunk
#' @export
setMethod("dbClearResult", "IcechunkResult", function(res, ...) DBI::dbClearResult(res@state$res, ...))

#' @rdname Icechunk
#' @export
setMethod("dbHasCompleted", "IcechunkResult", function(res, ...) DBI::dbHasCompleted(res@state$res, ...))

#' @rdname Icechunk
#' @export
setMethod("dbGetStatement", "IcechunkResult", function(res, ...) res@state$statement)

#' @rdname Icechunk
#' @export
setMethod("dbIsValid", "IcechunkResult", function(dbObj, ...) DBI::dbIsValid(dbObj@state$res))

#' @rdname Icechunk
#' @export
setMethod("dbExecute", c("IcechunkConnection", "character"), function(conn, statement, ...) {
  stop("SQL statements are read-only on an Icechunk connection; write with dbAppendTable()", call. = FALSE)
})

# ---- transactions and writes ------------------------------------------------------

.dbi_writable <- function(state) {
  if (is.null(state$branch)) stop("connection is pinned to a tag or snapshot; connect to a branch to write", call. = FALSE)
  state$session <- ic_session_writable(state$repo, state$branch)
  state$in_tx <- TRUE
  .dbi_invalidate(state)
}

.dbi_end <- function(state) {
  state$in_tx <- FALSE
  state$session <- .dbi_readonly(state)
  .dbi_invalidate(state)
}

#' @rdname Icechunk
#' @export
setMethod("dbBegin", "IcechunkConnection", function(conn, ...) {
  state <- .dbi_check(conn)
  if (state$in_tx) stop("a transaction is already open", call. = FALSE)
  .dbi_writable(state)
  invisible(TRUE)
})

#' @rdname Icechunk
#' @param message commit message.
#' @param metadata commit metadata (named list).
#' @param rebase if another writer moved the branch since [DBI::dbBegin()],
#'   rebase onto the new head and retry, up to `tries` times. With
#'   `solver = "detect"` this succeeds only when the two change sets touch
#'   different chunks and metadata; `solver = "basic"` also resolves chunk
#'   conflicts, keeping `on_chunk` (`"ours"` or `"theirs"`). An unresolvable
#'   conflict is signalled as `icechunk_conflict` and leaves the transaction
#'   open (rebased as far as it got), for [DBI::dbRollback()] or another try.
#' @param solver,on_chunk as for [ic_rebase()].
#' @param tries maximum commit attempts with `rebase = TRUE`.
#' @export
setMethod("dbCommit", "IcechunkConnection", function(conn, message = "DBI commit", metadata = NULL,
                                                     rebase = FALSE, solver = c("detect", "basic"),
                                                     on_chunk = c("ours", "theirs", "fail"), tries = 5L, ...) {
  state <- .dbi_check(conn)
  if (!state$in_tx) stop("no open transaction (dbBegin())", call. = FALSE)
  solver <- match.arg(solver); on_chunk <- match.arg(on_chunk)
  n <- if (isTRUE(rebase)) max(1L, as.integer(tries)) else 1L
  snap <- NULL
  for (i in seq_len(n)) {
    snap <- tryCatch(ic_commit(state$session, message, metadata = metadata),
                     icechunk_conflict = function(e) if (i < n) e else stop(e))
    if (!inherits(snap, "icechunk_conflict")) break
    ic_rebase(state$session, solver = solver, on_chunk = on_chunk)
    state$rebases <- (state$rebases %||% 0L) + 1L
  }
  state$last_snapshot <- snap
  .dbi_end(state)
  invisible(snap)
})

#' @rdname Icechunk
#' @export
setMethod("dbRollback", "IcechunkConnection", function(conn, ...) {
  state <- .dbi_check(conn)
  if (!state$in_tx) stop("no open transaction (dbBegin())", call. = FALSE)
  ic_discard(state$session)
  .dbi_end(state)
  invisible(TRUE)
})

.dbi_autocommit <- function(state, what, fun) {
  auto <- !state$in_tx
  if (auto) .dbi_writable(state)
  ok <- FALSE
  on.exit(if (auto && !ok) { ic_discard(state$session); .dbi_end(state) })
  n <- fun()
  .dbi_invalidate(state)
  if (auto) {
    state$last_snapshot <- ic_commit(state$session, what)
    .dbi_end(state)
  }
  ok <- TRUE
  n
}

.dbi_rules <- function(state) {
  root <- .dbi_root(state)
  if (!is.null(state$target)) return(.br_rules(list(remap = root$remap), state$target))
  root$rules
}

#' @rdname Icechunk
#' @param value a data frame. For `refs_<array>`: `dim_0..`, and either
#'   `path` (+ `offset`, `size`) for a virtual ref or `data` (raw) for an
#'   inline chunk; optional `present` (0 rows are skipped), `etag`, `mtime`.
#'   For `arrays`: byterefs `arrays` rows (JSON text columns), creating
#'   each array.
#' @param checksums record `etag` / `mtime` checksums on virtual refs.
#'   Rows without them (columns absent or `NA`) take the checksum of the
#'   same `path` in the `sources` table, so refs to a known source are
#'   checked like the rest; a path new to the repository gets none unless
#'   supplied.
#' @export
setMethod("dbAppendTable", "IcechunkConnection", function(conn, name, value, ..., checksums = TRUE, row.names = NULL) {
  state <- .dbi_check(conn)
  name <- as.character(name)
  value <- as.data.frame(value)
  if (identical(name, "arrays")) {
    a <- .br_arrays_parse(.dbi_arrays_in(value))
    return(.dbi_autocommit(state, sprintf("DBI: create %d arrays", nrow(a)), function() {
      for (i in seq_len(nrow(a))) .br_write_array_meta(state$session, a[i, ])
      nrow(a)
    }))
  }
  if (!startsWith(name, "refs_")) stop("can append to 'arrays' and 'refs_<array>' only", call. = FALSE)
  nm <- substring(name, 6L)
  if (!paste0("/", nm) %in% ic_nodes(state$session)$path) stop("no array '", nm, "'; append to 'arrays' first", call. = FALSE)
  if (is.null(value$path)) value$path <- NA_character_
  if (!is.null(value$data)) value$data <- lapply(value$data, function(v) if (is.null(v)) NULL else as.raw(v))
  for (f in intersect(c("offset", "size", "mtime"), names(value))) value[[f]] <- .dbl(value[[f]])
  if (checksums) value <- .dbi_join_sources(state, value)
  rules <- .dbi_rules(state)
  .dbi_autocommit(state, sprintf("DBI: %d rows into %s", nrow(value), name), function() {
    s <- .br_write_refs(state$session, nm, value, rules, checksums = checksums)
    s$refs + s$native
  })
})

#' @rdname Icechunk
#' @param overwrite,append only appending is supported.
#' @export
setMethod("dbWriteTable", c("IcechunkConnection", "character", "data.frame"),
          function(conn, name, value, ..., overwrite = FALSE, append = FALSE) {
  if (overwrite) stop("overwrite is not supported; Icechunk chunk refs are replaced by appending the same indices", call. = FALSE)
  invisible(dbAppendTable(conn, name, value, ...) >= 0)
})

# Fill missing etag / mtime from the sources already in the repository, by
# path, so appended refs to a known source carry its checksum. Supplied
# values win; paths not yet in the repository are left without one.
.dbi_join_sources <- function(state, value) {
  if (!nrow(value) || all(is.na(value$path))) return(value)
  src <- .dbi_read(state, "sources")
  if (!nrow(src)) return(value)
  m <- match(value$path, src$source)
  if (is.null(value$etag)) value$etag <- NA_character_
  if (is.null(value$mtime)) value$mtime <- NA_real_
  value$etag <- as.character(value$etag)
  fill_e <- is.na(value$etag) & is.na(value$mtime) & !is.na(m)
  value$etag[fill_e] <- src$etag[m[fill_e]]
  fill_m <- is.na(value$mtime) & is.na(value$etag) & !is.na(m)
  value$mtime[fill_m] <- src$mtime[m[fill_m]]
  value
}

.dbi_arrays_in <- function(value) {
  need <- c("array", "kind", "storage", "dims", "shape", "chunks", "dtype")
  miss <- setdiff(need, names(value))
  if (length(miss)) stop("arrays rows need columns: ", paste(miss, collapse = ", "), call. = FALSE)
  for (f in c("fill", "codec", "attrs", "affine")) if (is.null(value[[f]])) value[[f]] <- NA_character_
  if (is.null(value$values)) value$values <- vector("list", nrow(value))
  value
}

# ---- connection housekeeping -------------------------------------------------------

#' @rdname Icechunk
#' @param dbObj an Icechunk DBI object.
#' @export
setMethod("dbDisconnect", "IcechunkConnection", function(conn, ...) {
  state <- conn@state
  if (isTRUE(state$in_tx)) warning("discarding an open transaction", call. = FALSE)
  if (!is.null(state$sql) && DBI::dbIsValid(state$sql)) DBI::dbDisconnect(state$sql)
  state$valid <- FALSE
  invisible(TRUE)
})

#' @rdname Icechunk
#' @export
setMethod("dbIsValid", "IcechunkConnection", function(dbObj, ...) isTRUE(dbObj@state$valid))

#' @rdname Icechunk
#' @export
setMethod("dbIsValid", "IcechunkDriver", function(dbObj, ...) TRUE)

#' @rdname Icechunk
#' @export
setMethod("dbGetInfo", "IcechunkConnection", function(dbObj, ...) {
  s <- dbObj@state
  i <- if (isTRUE(s$valid)) ic_session_info(s$session) else list()
  list(dbname = ic_storage_desc_(s$repo$storage$ptr), branch = s$branch, tag = s$tag,
       snapshot = i$snapshot_id, in_transaction = isTRUE(s$in_tx),
       last_commit = s$last_snapshot, icechunk = ic_icechunk_version())
})

#' @rdname Icechunk
#' @export
setMethod("dbGetInfo", "IcechunkDriver", function(dbObj, ...) {
  list(driver.version = utils::packageVersion("icechunkr"), client.version = ic_icechunk_version())
})

#' @rdname Icechunk
#' @param object an Icechunk DBI object.
#' @export
setMethod("show", "IcechunkConnection", function(object) {
  s <- object@state
  if (!isTRUE(s$valid)) { cat("<IcechunkConnection> (closed)\n"); return(invisible(object)) }
  i <- ic_session_info(s$session)
  pin <- if (!is.null(s$branch)) paste("branch", s$branch) else if (!is.null(s$tag)) paste("tag", s$tag) else paste("snapshot", s$snapshot)
  cat("<IcechunkConnection> ", pin, " @ ", i$snapshot_id, if (isTRUE(s$in_tx)) " (transaction open)" else "", "\n", sep = "")
  invisible(object)
})

#' @rdname Icechunk
#' @export
setMethod("show", "IcechunkDriver", function(object) cat("<IcechunkDriver>\n"))

#' @rdname Icechunk
#' @param obj an R object.
#' @export
setMethod("dbDataType", "IcechunkDriver", function(dbObj, obj, ...) {
  if (is.list(obj)) return("BLOB")
  if (is.integer(obj)) return("INTEGER")
  if (is.double(obj)) return("DOUBLE")
  "TEXT"
})
