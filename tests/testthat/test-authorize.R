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

test_that("file:// prefixes and locations with doubled slashes still match", {
  # macOS: TMPDIR ends in "/", so tempdir() paths contain "//"; Icechunk
  # matches neither prefixes nor locations spelled that way
  d <- withr::local_tempdir()
  dd <- paste0(dirname(d), "//", basename(d))
  writeBin(as.raw(1:8), file.path(d, "grid.bin"))
  pre <- paste0("file://", dd, "/")
  rp <- file.path(withr::local_tempdir(), "r")
  repo <- ic_repo_create(rp, containers = ic_container(pre, "local", path = d), authorize = pre)
  expect_false(grepl("//", sub("^file://", "", ic_repo_containers(repo)$prefix)))
  s <- ic_session_writable(repo, "main")
  ic_set(s, "zarr.json", charToRaw('{"zarr_format":3,"node_type":"group","attributes":{}}'))
  ic_set(s, "a/zarr.json", charToRaw(paste0(
    '{"zarr_format":3,"node_type":"array","shape":[1],"data_type":"uint8",',
    '"chunk_grid":{"name":"regular","configuration":{"chunk_shape":[1]}},',
    '"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},',
    '"fill_value":0,"codecs":[{"name":"bytes"}],"attributes":{}}')))
  refs <- data.frame(dim_0 = 0L, location = paste0(pre, "grid.bin"), offset = 0, length = 8)
  expect_identical(ic_set_virtual_refs(s, "/a", refs), 0L)
  ic_commit(s, "doubled slashes")
  r <- ic_session_readonly(ic_repo_open(rp, authorize = pre), branch = "main")
  expect_identical(ic_get(r, "a/c/0"), as.raw(1:8))
})

test_that("Windows paths become file:///D:/ URLs, and back", {
  # Icechunk stores file://D:/x as file:///D:/x (and "\" as "/") but matches
  # containers by string prefix, so both sides must be spelled that way
  expect_identical(.file_url(c("file://D:/a/src/", "file:///D:/a/b.bin", "file://D:\\a\\b\\x.bin", "s3://b/k", NA)),
                   c("file:///D:/a/src/", "file:///D:/a/b.bin", "file:///D:/a/b/x.bin", "s3://b/k", NA))
  expect_identical(.path_file_url(c("D:/a/b/", "C:\\Users\\me\\T\\", "/home/me/")),
                   c("file:///D:/a/b/", "file:///C:/Users/me/T/", "file:///home/me/"))
  none <- data.frame(prefix = character(), replacement = character())
  expect_identical(.br_resolve(c("D:/a/x.bin", "C:\\u\\y.bin", "/home/me/z.bin", "https://h/p"), none),
                   c("file:///D:/a/x.bin", "file:///C:/u/y.bin", "file:///home/me/z.bin", "https://h/p"))
  expect_identical(.br_unresolve(c("file:///D:/a/x.bin", "file:///home/me/my%20data/z.bin", "s3://b/k"), none),
                   c("D:/a/x.bin", "/home/me/my data/z.bin", "s3://b/k"))
  expect_identical(.br_container_prefix("file:///D:/a/x.bin"), "file:///D:/a/")
})

test_that("a % in an Icechunk error message arrives intact", {
  # extendr passes the message to Rf_error() as a format string; "%s" there
  # would read a missing argument
  s <- ic_session_writable(ic_repo_create(file.path(withr::local_tempdir(), "r")), "main")
  loc <- "file:///no/container/a%s%d%20b/x.bin"
  e <- tryCatch(ic_set_virtual_ref(s, "a/c/0", loc, 0, 1), error = identity)
  expect_s3_class(e, "icechunk_error")
  expect_match(conditionMessage(e), "a%s%d%20b", fixed = TRUE)
})
