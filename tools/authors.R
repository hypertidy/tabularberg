# Regenerate inst/AUTHORS from Cargo.lock (run from the package root):
#   Rscript tools/authors.R
# Every crate the "full" tier can compile: normal and build dependencies,
# all target platforms (a superset of the "minimal" and "cloud" tiers).
m <- jsonlite::fromJSON(system2("cargo", c("metadata", "--format-version", "1", "--features", "full", "--locked",
                                           "--manifest-path", "src/rust/Cargo.toml"), stdout = TRUE),
                        simplifyVector = FALSE)
pk <- setNames(m$packages, vapply(m$packages, `[[`, "", "id"))
nodes <- setNames(m$resolve$nodes, vapply(m$resolve$nodes, `[[`, "", "id"))
root <- m$resolve$root
seen <- character(); stack <- root
while (length(stack)) {
  i <- stack[1]; stack <- stack[-1]
  if (i %in% seen) next
  seen <- c(seen, i)
  for (d in nodes[[i]]$deps) {
    kinds <- vapply(d$dep_kinds, function(k) if (is.null(k$kind)) "normal" else k$kind, "")
    if (any(kinds %in% c("normal", "build"))) stack <- c(stack, d$pkg)
  }
}
ps <- pk[setdiff(seen, root)]
ps <- ps[order(tolower(vapply(ps, `[[`, "", "name")), vapply(ps, `[[`, "", "version"))]
line <- function(p) {
  a <- vapply(p$authors, function(x) sub(" <.*", "", x), "")
  if (!length(a)) a <- paste("the contributors to", if (is.null(p$repository)) "the crate" else p$repository)
  sprintf("%s %s (%s): %s", p$name, p$version, if (is.null(p$license)) "see crate" else p$license,
          paste(a, collapse = "; "))
}
writeLines(c(
  "Authors and licences of the Rust crates compiled into icechunkr",
  "",
  "icechunkr links Rust crates (from crates.io) into its shared library. Below",
  "is every crate the 'full' feature tier can compile (normal and build",
  "dependencies, all target platforms; the 'minimal' and 'cloud' tiers use a",
  "subset). Generated from Cargo.lock by tools/authors.R; rerun it when",
  "dependencies change. Copyright in each crate belongs to its authors as",
  "named in the crate's metadata or, where none are named, to the",
  "contributors of the repository given.",
  "",
  sprintf("%d crates.", length(ps)), "",
  vapply(ps, line, "")), "inst/AUTHORS")
