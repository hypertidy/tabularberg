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
#' the tables a statement names: an in-memory SQLite copy (RSQLite), or with
#' `sql = "duckdb"` a DuckDB registration that does not copy.
#'
#' Writes go through a writable session on the connection's branch:
#' [DBI::dbBegin()] opens it, [DBI::dbAppendTable()] on `refs_<array>` sets
#' virtual (or inline, from `data`) chunk refs, on `arrays` creates arrays,
#' [DBI::dbCommit()] commits (pass `message`), [DBI::dbRollback()]
#' discards. Without `dbBegin()` each append commits on its own.
#'
#' @return `Icechunk()`: an `IcechunkDriver`.
#' @example inst/examples/Icechunk.R
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
#' @param sql SQL engine for [DBI::dbGetQuery()]: `"sqlite"` (default)
#'   copies each table a statement names into an in-memory SQLite;
#'   `"duckdb"` registers them with an in-memory DuckDB without copying,
#'   faster on large refs tables. In DuckDB `offset` and `array` are
#'   reserved words, so quote those columns (`"offset"`, `"array"`); SQL
#'   written that way runs on both engines.
#' @param arrow with `sql = "duckdb"`, hand `refs_<array>` tables to DuckDB
#'   as Arrow record batches built in Rust ([ic_refs_arrow()], native
#'   chunks' bytes included) rather than R vectors: less memory, faster
#'   first query, same results. `NA` (default): when nanoarrow and arrow
#'   are installed.
#' @param pushdown for a statement with a plain conjunctive `WHERE` (no
#'   `OR`, `NOT`, `CASE`, subquery or set operation) on `dim_k`
#'   (`=`, `<`, `<=`, `>`, `>=`, `BETWEEN`, `IN`), read only that slab of chunks of each
#'   `refs_<array>` table it names (and, with split manifests, only the
#'   manifests covering it) instead of the whole table. In joins, a
#'   predicate counts for a table when qualified by its name or alias, and
#'   ranges carry across `a.dim_i = b.dim_j` equalities, so a filter on one
#'   side narrows the other; self-joins and `NATURAL` / `USING` joins are
#'   read whole. The SQL still applies the full predicate, so results do
#'   not change.
#' @param ... unused.
#' @export
setMethod("dbConnect", "IcechunkDriver", function(drv, storage, branch = NULL, tag = NULL, snapshot = NULL,
                                                  authorize = NULL, containers = NULL, create = FALSE,
                                                  target = NULL, sql = c("sqlite", "duckdb"), pushdown = TRUE,
                                                  arrow = NA, ...) {
  sql <- match.arg(sql)
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
  state$engine <- sql; state$pushdown <- pushdown
  if (is.na(arrow)) arrow <- sql == "duckdb" && requireNamespace("nanoarrow", quietly = TRUE) &&
    requireNamespace("arrow", quietly = TRUE)
  state$arrow <- isTRUE(arrow) && sql == "duckdb"
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

.dbi_array1 <- function(state, nm) {
  if (!is.null(state$cache$arr[[nm]])) return(state$cache$arr[[nm]])
  if (!paste0("/", nm) %in% ic_nodes(state$session)$path) return(NULL)
  x <- .br_array(state$session, paste0("/", nm), .dbi_root(state), native = TRUE)
  state$cache$arr[[nm]] <- x
  x
}

.dbi_arrays <- function(state) {
  nodes <- ic_nodes(state$session)
  nms <- sub("^/", "", nodes$path[nodes$type == "array"])
  res <- lapply(nms, function(nm) .dbi_array1(state, nm))
  names(res) <- nms
  res
}

.dbi_data_arrays <- function(state) {
  a <- .dbi_arrays(state)
  names(a)[vapply(a, function(x) identical(x$arow$kind, "data"), logical(1))]
}

# refs_<array> as a data frame; `ranges` (list of c(lo, hi) chunk index
# ranges, hi exclusive) reads only that slab, fetching only the manifests
# that cover it
.dbi_refs_table <- function(state, nm, ranges = NULL) {
  a <- if (is.null(ranges)) .dbi_array1(state, nm) else {
    if (!paste0("/", nm) %in% ic_nodes(state$session)$path) NULL
    else .br_array(state$session, paste0("/", nm), .dbi_root(state), native = TRUE, ranges = ranges)
  }
  if (is.null(a)) stop("no array '", nm, "'", call. = FALSE)
  r <- a$refs
  nd <- length(jsonlite::fromJSON(a$arow$shape))
  if (is.null(r)) {
    r <- as.data.frame(stats::setNames(rep(list(integer()), nd), paste0("dim_", seq_len(nd) - 1L)))
    r$present <- integer(); r$path <- character(); r$offset <- numeric(); r$size <- numeric()
    r$info <- character(); r$data <- list()
  }
  r <- r[, c(paste0("dim_", seq_len(nd) - 1L), "present", "path", "offset", "size", "info", "data")]
  r$data <- blob_list(r$data, check = FALSE)
  rownames(r) <- NULL
  r
}

.dbi_ndim <- function(state, nm) {
  n <- ic_nodes(state$session)
  i <- match(paste0("/", nm), n$path)
  if (is.na(i) || is.na(n$shape[i])) return(NA_integer_)
  length(strsplit(n$shape[i], ",", fixed = TRUE)[[1]])
}

# chunks = list(dim_0 = c(lo, hi), ...) -> positional ranges
.dbi_chunks_ranges <- function(chunks, nd) {
  rng <- rep(list(c(0, Inf)), nd)
  for (k in names(chunks)) {
    d <- as.integer(sub("^dim_", "", k))
    if (is.na(d) || d < 0 || d >= nd) stop("chunks names are dim_0..dim_", nd - 1L, call. = FALSE)
    v <- as.numeric(chunks[[k]])
    rng[[d + 1L]] <- if (length(v) == 1L) c(v, v + 1) else v[1:2]
  }
  rng
}

# Conservative pushdown for one refs_<array> table of a statement: a plain
# conjunctive WHERE (no OR / NOT / CASE / subquery / set operation) with
# integer comparisons on dim_k. With several refs tables (joins), a
# predicate counts for a table only when qualified by its name or alias,
# and ranges carry across equalities `x.dim_i = y.dim_j` (in ON or WHERE):
# with a conjunctive WHERE every output row satisfies both, whatever the
# join type. Self-joins, NATURAL and USING joins are not pushed down.
# Returns chunk ranges (a superset of the rows the statement keeps; the
# SQL still applies the full predicate) or NULL.
.dbi_slab <- function(statement, table, nd) {
  st <- gsub("'[^']*'", "''", statement)                       # ignore string literals
  if (grepl("\\b(OR|NOT|CASE|UNION|EXCEPT|INTERSECT)\\b", st, ignore.case = TRUE, perl = TRUE)) return(NULL)
  if (grepl("\\(\\s*SELECT\\b", st, ignore.case = TRUE, perl = TRUE)) return(NULL)
  # every refs_<array> table named, with how often and under which aliases
  tok <- "(?<![\\w.])(?:\"(refs_[^\"]+)\"|`(refs_[^`]+)`|\\[(refs_[^\\]]+)\\]|(?<![\"`\\[])(refs_[\\w]+))(?![\\w]|\\s*\\.)"
  kw <- "(?i:JOIN|INNER|LEFT|RIGHT|FULL|OUTER|CROSS|NATURAL|ON|USING|WHERE|GROUP|ORDER|LIMIT|HAVING|WINDOW|QUALIFY|LATERAL|SEMI|ANTI|ASOF|POSITIONAL|AS)"
  m <- regmatches(st, gregexpr(paste0(tok, "(?:\\s+(?i:AS)\\s+|\\s+)(?!", kw, "\\b)([\"`]?)([A-Za-z_]\\w*)\\5(?![\\w.])|", tok), st, perl = TRUE))[[1]]
  if (!length(m)) return(NULL)
  name_of <- function(x) {
    g <- regmatches(x, regexec(tok, x, perl = TRUE))[[1]][-1]
    g[nzchar(g)][1]
  }
  alias_of <- function(x) {
    nm <- name_of(x)
    rest <- sub(tok, "", x, perl = TRUE)
    a <- gsub("[\"`]", "", trimws(sub("^\\s*(?i:AS)\\s+", " ", rest, perl = TRUE)))
    if (nzchar(a) && a != nm) a else character()
  }
  names_seen <- vapply(m, name_of, "")
  if (sum(names_seen == table) != 1L) return(NULL)               # absent, or a self-join
  tabs <- unique(names_seen)
  if (length(tabs) > 1L && grepl("\\b(NATURAL|USING)\\b", st, ignore.case = TRUE, perl = TRUE)) return(NULL)
  quals <- lapply(stats::setNames(tabs, tabs), function(t) {
    if (sum(names_seen == t) != 1L) return(character())          # ambiguous: never matched
    c(t, alias_of(m[names_seen == t]))
  })
  w <- regmatches(st, regexec("(?is)\\bWHERE\\b(.*?)(\\bGROUP\\s+BY\\b|\\bORDER\\s+BY\\b|\\bLIMIT\\b|\\bHAVING\\b|\\bWINDOW\\b|\\bQUALIFY\\b|$)", st, perl = TRUE))[[1]]
  w <- if (length(w) >= 2L) w[2] else ""
  # dbplyr asks for a table's columns with WHERE (0 = 1): an empty slab
  if (grepl("(^|[^\\w.])0\\s*=\\s*1([^\\w.]|$)", w, perl = TRUE)) return(rep(list(c(0, 0)), nd))
  qre <- function(q) paste0("(?:[\"`\\[]?(?:", paste0("\\Q", q, "\\E", collapse = "|"), ")[\"`\\]]?\\.)")
  # dim_k of table t: qualified by its name or alias; unqualified only when
  # it is the only refs table in the statement
  colre <- function(t, k) {
    q <- quals[[t]]
    if (!length(q)) return(NULL)
    d <- paste0("[\"`\\[]?dim_", k, "[\"`\\]]?(?![\\w])")
    if (length(tabs) == 1L) paste0("(?:", qre(q), "|(?<![\\w.\"`\\]]))", d) else paste0(qre(q), d)
  }
  rng <- function(t, k) .dbi_slab_range(w, colre(t, k))
  # equalities between dim columns of two tables (ON or WHERE)
  eq <- list()
  if (length(tabs) > 1L) {
    side <- "[\"`\\[]?([A-Za-z_][\\w/]*)[\"`\\]]?\\.[\"`\\[]?dim_(\\d+)[\"`\\]]?(?![\\w])"
    e <- regmatches(st, gregexpr(paste0(side, "\\s*=\\s*", side), st, perl = TRUE))[[1]]
    owner <- function(q) { o <- names(quals)[vapply(quals, function(x) q %in% x, NA)]; if (length(o) == 1L) o else NA }
    for (x in e) {
      g <- regmatches(x, regexec(paste0(side, "\\s*=\\s*", side), x, perl = TRUE))[[1]]
      ta <- owner(g[2]); tb <- owner(g[4])
      if (!is.na(ta) && !is.na(tb) && ta != tb) eq[[length(eq) + 1L]] <- list(ta, as.integer(g[3]), tb, as.integer(g[5]))
    }
  }
  key <- function(t, k) paste0(t, "\r", k)
  ks <- unique(c(lapply(seq_len(nd) - 1L, function(k) list(table, k)),
                 unlist(lapply(eq, function(x) list(list(x[[1]], x[[2]]), list(x[[3]], x[[4]]))), recursive = FALSE)))
  R <- list()
  for (x in ks) R[[key(x[[1]], x[[2]])]] <- rng(x[[1]], x[[2]])
  for (it in seq_len(length(eq) + 1L)) {                        # carry ranges across equalities
    changed <- FALSE
    for (x in eq) {
      a <- key(x[[1]], x[[2]]); b <- key(x[[3]], x[[4]])
      v <- c(max(R[[a]][1], R[[b]][1]), min(R[[a]][2], R[[b]][2]))
      if (!identical(v, R[[a]]) || !identical(v, R[[b]])) { R[[a]] <- R[[b]] <- v; changed <- TRUE }
    }
    if (!changed) break
  }
  out <- lapply(seq_len(nd) - 1L, function(k) { v <- R[[key(table, k)]]; c(v[1], max(v[1], v[2])) })
  if (any(vapply(out, function(v) v[1] > 0 || is.finite(v[2]), NA))) out else NULL
}

# [lo, hi) of one column from the conjunctive predicates in `w` (c(0, Inf)
# when none, or when `col` is NULL)
.dbi_slab_range <- function(w, col) {
  lo <- 0; hi <- Inf
  if (is.null(col) || !nzchar(w)) return(c(lo, hi))
  int <- "(-?\\d+)(?:\\.0+)?(?![\\d.eE])"
  m <- regmatches(w, gregexpr(paste0("(?i)", col, "\\s+BETWEEN\\s+", int, "\\s+AND\\s+", int), w, perl = TRUE))[[1]]
  for (x in m) { v <- as.numeric(regmatches(x, gregexpr("-?\\d+(?:\\.0+)?(?![\\w])", x, perl = TRUE))[[1]]); v <- v[length(v) - 1:0]; lo <- max(lo, v[1]); hi <- min(hi, v[2] + 1) }
  w2 <- gsub(paste0("(?i)", col, "\\s+BETWEEN\\s+", int, "\\s+AND\\s+", int), " ", w, perl = TRUE)
  m <- regmatches(w2, gregexpr(paste0("(?i)", col, "\\s+IN\\s*\\(([-\\d.,\\s]+)\\)"), w2, perl = TRUE))[[1]]
  for (x in m) { y <- sub("^.*\\bIN\\s*\\(", "", x, ignore.case = TRUE, perl = TRUE); v <- as.numeric(regmatches(y, gregexpr("-?\\d+(?:\\.0+)?", y, perl = TRUE))[[1]]); lo <- max(lo, min(v)); hi <- min(hi, max(v) + 1) }
  m <- regmatches(w2, gregexpr(paste0(col, "\\s*(<=|>=|<>|!=|=|<|>)\\s*", int), w2, perl = TRUE))[[1]]
  for (x in m) {
    op <- regmatches(x, regexpr("(<=|>=|<>|!=|=|<|>)(?=\\s*-?\\d+(?:\\.0+)?\\s*$)", x, perl = TRUE)); v <- as.numeric(regmatches(x, regexpr("-?\\d+(?:\\.0+)?\\s*$", x, perl = TRUE)))
    switch(op, "=" = { lo <- max(lo, v); hi <- min(hi, v + 1) }, ">=" = lo <- max(lo, v), ">" = lo <- max(lo, v + 1),
           "<=" = hi <- min(hi, v + 1), "<" = hi <- min(hi, v), NULL)
  }
  m <- regmatches(w2, gregexpr(paste0(int, "\\s*(<=|>=|=|<|>)\\s*", col), w2, perl = TRUE))[[1]]
  for (x in m) {
    op <- regmatches(x, regexpr("<=|>=|=|<|>", x)); v <- as.numeric(regmatches(x, regexpr("^\\s*-?\\d+", x)))
    switch(op, "=" = { lo <- max(lo, v); hi <- min(hi, v + 1) }, "<=" = lo <- max(lo, v), "<" = lo <- max(lo, v + 1),
           ">=" = hi <- min(hi, v + 1), ">" = hi <- min(hi, v), NULL)
  }
  c(lo, max(lo, hi))
}

.DBI_FIXED <- c("meta", "arrays", "sources", "branches", "tags", "ancestry", "nodes")

.dbi_tables <- function(state) {
  nodes <- ic_nodes(state$session)
  arrs <- sub("^/", "", nodes$path[nodes$type == "array"])
  c(.DBI_FIXED, if (length(arrs)) paste0("refs_", arrs))
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
        if (is.null(x$refs)) return(NULL)
        k <- which(!is.na(x$refs$path) & !duplicated(x$refs$path))
        data.frame(path = x$refs$path[k], etag = x$refs$etag[k], mtime = x$refs$mtime[k], stringsAsFactors = FALSE)
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
      .dbi_refs_table(state, substring(name, 6L))
    })
  rownames(out) <- NULL
  state$cache$tables[[name]] <- out
  out
}

#' @rdname Icechunk
#' @param conn an `IcechunkConnection`.
#' @param name table name.
#' @param chunks for `dbReadTable()` on `refs_<array>`: a slab of chunk
#'   indices, `list(dim_0 = c(lo, hi), ...)` (hi exclusive; a single number
#'   is one index). Only the manifests covering it are read when the
#'   repository splits manifests (see `manifest_split` in [ic_repo_create()]).
#' @export
setMethod("dbListTables", "IcechunkConnection", function(conn, ...) .dbi_tables(.dbi_check(conn)))

#' @rdname Icechunk
#' @export
setMethod("dbExistsTable", c("IcechunkConnection", "character"), function(conn, name, ...) {
  name %in% .dbi_tables(.dbi_check(conn))
})

#' @rdname Icechunk
#' @export
setMethod("dbReadTable", c("IcechunkConnection", "character"), function(conn, name, ..., chunks = NULL) {
  state <- .dbi_check(conn)
  if (is.null(chunks)) return(.dbi_read(state, name))
  if (!startsWith(name, "refs_")) stop("chunks = applies to refs_<array> tables", call. = FALSE)
  nm <- substring(name, 6L)
  nd <- .dbi_ndim(state, nm)
  if (is.na(nd)) stop("no array '", nm, "'", call. = FALSE)
  .dbi_refs_table(state, nm, .dbi_chunks_ranges(chunks, nd))
})

#' @rdname Icechunk
#' @export
setMethod("dbListFields", c("IcechunkConnection", "character"), function(conn, name, ...) {
  state <- .dbi_check(conn)
  if (startsWith(name, "refs_") && is.null(state$cache$tables[[name]])) {
    nd <- .dbi_ndim(state, substring(name, 6L))
    if (is.na(nd)) stop("no table '", name, "'", call. = FALSE)
    return(names(.dbi_refs_table(state, substring(name, 6L), rep(list(c(0, 0)), nd))))
  }
  names(.dbi_read(state, name))
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

.dbi_sql <- function(state, statement, pushdown = isTRUE(state$pushdown)) {
  duck <- identical(state$engine, "duckdb")
  if (is.null(state$sql) || !DBI::dbIsValid(state$sql)) {
    if (duck) {
      .need("duckdb")
      # in-memory only: keep DuckDB from creating ~/.duckdb state
      drv <- if ("shared_home" %in% names(formals(duckdb::duckdb))) duckdb::duckdb(shared_home = FALSE) else duckdb::duckdb()
      state$sql <- DBI::dbConnect(drv)
    } else { .need("RSQLite"); state$sql <- DBI::dbConnect(RSQLite::SQLite(), ":memory:") }
    state$sql_loaded <- character()
  }
  key <- state$cache$key %||% (state$cache$key <- paste(ic_session_info(state$session)$snapshot_id, stats::runif(1)))
  if (!identical(state$sql_key, key)) {
    for (t in state$sql_loaded) .dbi_drop(state, t)
    state$sql_loaded <- character(); state$sql_key <- key
  }
  transient <- character()
  need <- setdiff(.dbi_sql_tables(state, statement), state$sql_loaded)
  for (t in need) {
    if (t %in% c(state$sql_arrow, if (!identical(state$engine, "duckdb")) DBI::dbListTables(state$sql))) .dbi_drop(state, t)
    slab <- NULL
    if (pushdown && startsWith(t, "refs_") && is.null(state$cache$tables[[t]])) {
      nd <- .dbi_ndim(state, substring(t, 6L))
      if (!is.na(nd)) slab <- .dbi_slab(statement, t, nd)
    }
    if (isTRUE(state$arrow) && startsWith(t, "refs_") && is.null(state$cache$tables[[t]]) &&
        .dbi_put_arrow(state, t, slab)) {
      if (is.null(slab)) state$sql_loaded <- c(state$sql_loaded, t) else transient <- c(transient, t)
      next
    }
    d <- if (is.null(slab)) .dbi_read(state, t) else .dbi_refs_table(state, substring(t, 6L), slab)
    .dbi_put(state, t, d)
    if (is.null(slab)) state$sql_loaded <- c(state$sql_loaded, t) else transient <- c(transient, t)
  }
  state$last_pushdown <- transient
  list(con = state$sql, transient = transient)
}

.dbi_put <- function(state, t, d) {
  if (identical(state$engine, "duckdb")) {
    duckdb::duckdb_register(state$sql, t, d)
  } else {
    for (f in names(d)) if (inherits(d[[f]], "POSIXct")) d[[f]] <- format(d[[f]], "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    DBI::dbWriteTable(state$sql, t, d)
  }
}

.dbi_drop <- function(state, t) {
  if (t %in% state$sql_arrow) {
    duckdb::duckdb_unregister_arrow(state$sql, t)
    state$sql_arrow <- setdiff(state$sql_arrow, t)
  } else if (identical(state$engine, "duckdb")) duckdb::duckdb_unregister(state$sql, t) else DBI::dbRemoveTable(state$sql, t)
}

# register refs_<array> with DuckDB from Rust-built Arrow batches (native
# chunks' bytes fetched in Rust too). Collected into an Arrow table, as
# DuckDB may scan a registered table more than once.
.dbi_put_arrow <- function(state, t, slab) {
  nm <- substring(t, 6L)
  st <- .refs_arrow(state$session, paste0("/", nm), slab, .dbi_root(state)$rules)
  duckdb::duckdb_register_arrow(state$sql, t, arrow::as_arrow_table(st))
  state$sql_arrow <- c(state$sql_arrow, t)
  TRUE
}

#' @rdname Icechunk
#' @param statement SQL (read-only: SELECT over the tables it names).
#' @export
setMethod("dbGetQuery", c("IcechunkConnection", "character"), function(conn, statement, ...) {
  state <- .dbi_check(conn)
  e <- .dbi_sql(state, statement)
  on.exit(for (t in e$transient) .dbi_drop(state, t))
  DBI::dbGetQuery(e$con, statement, ...)
})

#' @rdname Icechunk
#' @export
setMethod("dbSendQuery", c("IcechunkConnection", "character"), function(conn, statement, ...) {
  state <- .dbi_check(conn)
  rs <- new.env(parent = emptyenv())
  e <- .dbi_sql(state, statement)
  rs$res <- DBI::dbSendQuery(e$con, statement, ...)
  rs$statement <- statement
  rs$conn_state <- state
  rs$transient <- e$transient
  new("IcechunkResult", state = rs)
})

#' @rdname Icechunk
#' @param res an `IcechunkResult`.
#' @param n rows to fetch (-1 for all).
#' @export
setMethod("dbFetch", "IcechunkResult", function(res, n = -1, ...) DBI::dbFetch(res@state$res, n = n, ...))

#' @rdname Icechunk
#' @export
setMethod("dbClearResult", "IcechunkResult", function(res, ...) {
  ok <- DBI::dbClearResult(res@state$res, ...)
  for (t in res@state$transient) .dbi_drop(res@state$conn_state, t)
  res@state$transient <- character()
  invisible(ok)
})

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
#' @param row.names ignored (required by the DBI generic).
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
      # a repository made by dbConnect(create = TRUE) has no root group yet
      if (!isTRUE(ic_exists(state$session, "zarr.json")))
        ic_set(state$session, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{}}'))
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
  if (!is.null(state$sql) && DBI::dbIsValid(state$sql)) {
    if (identical(state$engine, "duckdb")) DBI::dbDisconnect(state$sql, shutdown = TRUE) else DBI::dbDisconnect(state$sql)
  }
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
       snapshot = i$snapshot_id, in_transaction = isTRUE(s$in_tx), sql = s$engine,
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
