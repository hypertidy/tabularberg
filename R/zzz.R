# dbplyr integration (Suggests): tbl(con, "refs_<array>") works on an
# Icechunk connection, translated in the dialect of its SQL engine.
.onLoad <- function(libname, pkgname) {
  s3_register("dbplyr::dbplyr_edition", "IcechunkConnection", function(con) 2L)
  s3_register("dbplyr::sql_dialect", "IcechunkConnection", function(con) {
    state <- .dbi_check(con)
    dbplyr::sql_dialect(.dbi_sql(state, "")$con)
  })
  invisible()
}

# Register an S3 method for a generic in a suggested package, now if it is
# loaded and again whenever it loads (after vctrs::s3_register).
s3_register <- function(generic, class, method) {
  parts <- strsplit(generic, "::", fixed = TRUE)[[1]]
  pkg <- parts[1]; gen <- parts[2]
  reg <- function(...) {
    ns <- asNamespace(pkg)
    if (exists(gen, envir = ns)) registerS3method(gen, class, method, envir = ns)
  }
  setHook(packageEvent(pkg, "onLoad"), reg)
  if (isNamespaceLoaded(pkg)) reg()
  invisible()
}
