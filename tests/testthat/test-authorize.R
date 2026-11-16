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
  fx <- local_vfx(); fx$virtual_prefix <- fx$auth
  r0 <- ic_repo_open(fx$path)
  cn <- ic_repo_containers(r0)
  expect_true(fx$virtual_prefix %in% cn$prefix)
  expect_false(any(cn$authorized))
  r <- ic_repo_open(fx$path, authorize = "file://")
  expect_true(all(ic_repo_containers(r)$authorized))
  s <- ic_session_readonly(r)
  expect_type(ic_get(s, "from_bin/c/0/0"), "raw")
  expect_error(ic_repo_open(fx$path, authorize = paste0(fx$virtual_prefix, "deeper/")), "lies inside")
})

test_that("an anonymous S3 credential marks its container anonymous", {
  # icechunk 2.3.1's object_store S3 fetcher lets the container's own
  # anonymous flag overwrite an anonymous credential; see anonymous_containers()
  p <- "s3://example-bucket/data/"
  rp <- withr::local_tempdir()
  repo <- ic_repo_create(rp, containers = ic_container(p, "s3", region = "us-east-1"),
                         authorize = stats::setNames(list(ic_cred_s3("anonymous")), p))
  store <- ic_repo_config(repo)$virtual_chunk_containers[[p]]$store$s3
  expect_true(store$anonymous)
  expect_identical(store$region, "us-east-1")
  repo2 <- ic_repo_open(rp, containers = ic_container(p, "s3", region = "us-east-1"),
                        authorize = stats::setNames(list(ic_cred_s3("from_env")), p))
  expect_false(ic_repo_config(repo2)$virtual_chunk_containers[[p]]$store$s3$anonymous)
})
