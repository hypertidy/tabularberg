test_that("every fixture opens and refs match manifest", {
  skip_if_no_fixtures()
  m <- manifest()
  for (nm in names(m$fixtures)) {
    fx <- m$fixtures[[nm]]
    auth <- if (!is.null(fx$virtual_prefix)) fx$virtual_prefix
    repo <- ic_repo_open(fx_path(nm), authorize = auth)
    b <- ic_branches(repo)
    expect_setequal(b$name, names(fx$branches))
    expect_equal(b$snapshot_id[order(b$name)], unlist(fx$branches[sort(names(fx$branches))]), ignore_attr = TRUE)
    t <- ic_tags(repo)
    expect_setequal(t$name, names(fx$tags))
    expect_equal(ic_ancestry(repo)$snapshot_id, unlist(fx$ancestry_main))
  }
})

test_that("every key's bytes match zarr-python through icechunk, at every branch and tag", {
  skip_if_no_fixtures()
  m <- manifest()
  for (nm in names(m$fixtures)) {
    fx <- m$fixtures[[nm]]
    auth <- if (!is.null(fx$virtual_prefix)) fx$virtual_prefix
    repo <- ic_repo_open(fx_path(nm), authorize = auth)
    keys <- fx_keys(nm)
    for (v in names(keys)) {
      kv <- strsplit(v, ":", fixed = TRUE)[[1]]
      s <- if (kv[1] == "branch") ic_session_readonly(repo, branch = kv[2]) else ic_session_readonly(repo, tag = kv[2])
      want <- keys[[v]]
      expect_setequal(ic_list(s), names(want))
      got <- ic_get_many(s, names(want))
      for (i in seq_along(want)) {
        expect_identical(sha(got[[i]]), want[[i]][[1]], label = paste(nm, v, names(want)[i]))
      }
    }
  }
})

test_that("bench_f32 decodes to the documented formula", {
  skip_if_no_fixtures()
  s <- ic_session_readonly(ic_repo_open(fx_path("native_v2")))
  ch <- ic_get(s, "bench_f32/c/2/5")
  v <- readBin(ch, "numeric", n = 512 * 512, size = 4, endian = "little")
  m <- matrix(v, 512, 512, byrow = TRUE)
  i <- (2 * 512 + 0:511); j <- (5 * 512 + 0:511)
  want <- outer(i, j, function(i, j) (i * 4096 + j) %% 1000)
  expect_equal(m, want)
  expect_null(ic_get(s, "int16_fill/c/1/1"))  # never written: fill value
})

test_that("byte ranges", {
  skip_if_no_fixtures()
  s <- ic_session_readonly(ic_repo_open(fx_path("native_v2")))
  all <- ic_get(s, "bench_f32/c/0/0")
  expect_identical(ic_get(s, "bench_f32/c/0/0", offset = 100, length = 50), all[101:150])
  expect_identical(ic_get(s, "bench_f32/c/0/0", offset = 1000), all[1001:length(all)])
})

test_that("chunk refs table", {
  skip_if_no_fixtures()
  s <- ic_session_readonly(ic_repo_open(fx_path("native_v2")))
  r <- ic_chunk_refs(s, "/bench_f32")
  expect_equal(nrow(r), 64L)
  expect_true(all(r$kind == "native"))
  expect_true(all(file.exists(file.path(fx_path("native_v2"), r$location))))
  expect_true(all(r$length == 512 * 512 * 4))
  # native location + offset/length reproduce the store bytes
  i <- 7
  con <- file(file.path(fx_path("native_v2"), r$location[i]), "rb")
  seek(con, r$offset[i]); raw <- readBin(con, "raw", r$length[i]); close(con)
  expect_identical(raw, ic_get(s, r$key[i]))
  # sparse array has only the written chunk
  expect_equal(ic_chunk_refs(s, "/int16_fill")$coord, "0,0")

  si <- ic_session_readonly(ic_repo_open(fx_path("inline_v2")))
  ri <- ic_chunk_refs(si, inline = TRUE)
  expect_true(all(ri$kind == "inline"))
  expect_identical(ri$inline[[1]], ic_get(si, ri$key[1]))

  fx <- manifest()$fixtures$virtual_v2
  sv <- ic_session_readonly(ic_repo_open(fx_path("virtual_v2"), authorize = fx$virtual_prefix))
  rv <- ic_chunk_refs(sv)
  expect_true(all(rv$kind == "virtual"))
  expect_true(all(startsWith(rv$location, fx$virtual_prefix)))
  expect_setequal(ic_virtual_locations(sv), unique(rv$location))
})

test_that("virtual chunks need authorisation", {
  skip_if_no_fixtures()
  fx <- manifest()$fixtures$virtual_v2
  s <- ic_session_readonly(ic_repo_open(fx_path("virtual_v2")))
  expect_error(ic_get(s, "from_bin/c/0/0"), class = "icechunk_error")
  s2 <- ic_session_readonly(ic_repo_open(fx_path("virtual_v2"), authorize = list2(fx$virtual_prefix)))
  v <- readBin(ic_get(s2, "from_bin/c/1/0"), "numeric", 16 * 48, size = 4)
  expect_equal(v, (16 * 48 + 0:(16 * 48 - 1)) / 10, tolerance = 1e-6)
})


test_that("history: branch, tag, snapshot selection; amend; diff", {
  skip_if_no_fixtures()
  fx <- manifest()$fixtures$history_v2
  repo <- ic_repo_open(fx_path("history_v2"))
  k <- fx_keys("history_v2")
  dig <- function(s) sha(ic_get(s, "x/c/0"))
  expect_identical(dig(ic_session_readonly(repo, tag = "v1")), k[["tag:v1"]][["x/c/0"]][[1]])
  expect_identical(dig(ic_session_readonly(repo, branch = "dev")), k[["branch:dev"]][["x/c/0"]][[1]])
  s1 <- ic_session_readonly(repo, snapshot = fx$snapshots$c2)
  expect_identical(dig(s1), k[["tag:v1"]][["x/c/0"]][[1]])
  a <- ic_ancestry(repo)
  expect_false(fx$snapshots$c3_before_amend %in% a$snapshot_id)
  expect_true(fx$snapshots$c3_amended %in% a$snapshot_id)
  d <- ic_diff(repo, list(tag = "v1"), list(branch = "dev"))
  expect_true("/x" %in% d$updated_chunks$path)
  expect_true("/" %in% d$updated_groups)
})
