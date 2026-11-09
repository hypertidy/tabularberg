test_that("authorize expansion against declared containers", {
  d <- c("https://a.org/", "https://b.org/data/", "s3://bucket/")
  x <- .authorize_expand("https://", d)
  expect_setequal(names(x), c("https://a.org/", "https://b.org/data/"))
  expect_true(all(vapply(x, function(c) identical(c$type, "http"), logical(1))))
  expect_identical(names(.authorize_expand("s3://bucket/", d)), "s3://bucket/")
  # a more specific entry wins over a broad one for the same container
  y <- .authorize_expand(list("s3://" = ic_cred_s3("anonymous"), "s3://bucket/" = ic_cred_s3("from_env")), d)
  expect_identical(y[["s3://bucket/"]]$kind, "from_env")
  expect_error(.authorize_expand("https://b.org/data/sub/", d), "lies inside container 'https://b.org/data/'")
  expect_warning(z <- .authorize_expand("gs://", d), "covers no virtual chunk container.*declared: 'https://a.org/'")
  expect_length(z, 0)
  expect_warning(.authorize_expand("https://x/", character()), "declared: none")
  expect_error(.authorize_expand(list(ic_cred_local()), d), "named")
})

test_that("broad authorize reads virtual chunks; containers listed", {
  skip_if_no_fixtures()
  fx <- manifest()$fixtures$virtual_v2
  r0 <- ic_repo_open(fx_path("virtual_v2"))
  cn <- ic_repo_containers(r0)
  expect_true(fx$virtual_prefix %in% cn$prefix)
  expect_false(any(cn$authorized))
  r <- ic_repo_open(fx_path("virtual_v2"), authorize = "file://")
  expect_true(all(ic_repo_containers(r)$authorized))
  s <- ic_session_readonly(r)
  expect_type(ic_get(s, "from_bin/c/0/0"), "raw")
  expect_error(ic_repo_open(fx_path("virtual_v2"), authorize = paste0(fx$virtual_prefix, "deeper/")), "lies inside")
})
