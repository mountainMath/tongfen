# ── cached_download ───────────────────────────────────────────────────────────

# local file standing in for the remote file, with its md5 checksum as ETag like S3
local_remote <- function(content) {
  remote <- tempfile()
  writeLines(content, remote)
  url <- paste0("file://", normalizePath(remote, winslash = "/"))
  rm(list = intersect(url, ls(tongfen:::tongfen_session)), envir = tongfen:::tongfen_session)
  list(path = remote, url = url)
}

test_that("cached_download: downloads the file and remembers the ETag", {
  remote <- local_remote("a")
  path <- file.path(tempfile(), "file.txt")
  local_mocked_bindings(remote_etag = function(url) unname(tools::md5sum(remote$path)))
  expect_equal(tongfen:::cached_download(remote$url, path), path)
  expect_equal(readLines(path), "a")
  expect_equal(readLines(paste0(path, ".etag")), unname(tools::md5sum(remote$path)))
})

test_that("cached_download: checks the remote file only once per session", {
  remote <- local_remote("a")
  path <- file.path(tempfile(), "file.txt")
  checks <- 0
  local_mocked_bindings(remote_etag = function(url) {
    checks <<- checks + 1
    unname(tools::md5sum(remote$path))
  })
  tongfen:::cached_download(remote$url, path)
  tongfen:::cached_download(remote$url, path)
  expect_equal(checks, 1)
  tongfen:::cached_download(remote$url, path, refresh = TRUE)
  expect_equal(checks, 2)
})

test_that("cached_download: downloads again only if the remote file changed", {
  remote <- local_remote("a")
  path <- file.path(tempfile(), "file.txt")
  local_mocked_bindings(remote_etag = function(url) unname(tools::md5sum(remote$path)))
  tongfen:::cached_download(remote$url, path)

  # new session, unchanged remote file, the cached file is left alone
  rm(list = remote$url, envir = tongfen:::tongfen_session)
  writeLines("local", path)
  tongfen:::cached_download(remote$url, path)
  expect_equal(readLines(path), "local")

  # new session, changed remote file
  rm(list = remote$url, envir = tongfen:::tongfen_session)
  writeLines("b", remote$path)
  tongfen:::cached_download(remote$url, path)
  expect_equal(readLines(path), "b")
  expect_equal(readLines(paste0(path, ".etag")), unname(tools::md5sum(remote$path)))
})

test_that("cached_download: falls back to the cached file if the remote can't be checked", {
  remote <- local_remote("a")
  path <- file.path(tempfile(), "file.txt")
  local_mocked_bindings(remote_etag = function(url) unname(tools::md5sum(remote$path)))
  tongfen:::cached_download(remote$url, path)

  rm(list = remote$url, envir = tongfen:::tongfen_session)
  writeLines("b", remote$path)
  local_mocked_bindings(remote_etag = function(url) NULL)
  expect_message(tongfen:::cached_download(remote$url, path), "using cached version")
  expect_equal(readLines(path), "a")
})

test_that("cached_download: rejects downloads not matching the md5 ETag", {
  remote <- local_remote("a")
  path <- file.path(tempfile(), "file.txt")
  local_mocked_bindings(remote_etag = function(url) strrep("0", 32))
  expect_error(tongfen:::cached_download(remote$url, path), "corrupted")
  expect_false(file.exists(path))
})

test_that("cached_download: check_remote = FALSE uses the cached file without contacting the remote", {
  remote <- local_remote("a")
  path <- file.path(tempfile(), "file.txt")
  local_mocked_bindings(remote_etag = function(url) stop("remote should not be checked"))
  expect_equal(tongfen:::cached_download(remote$url, path, check_remote = FALSE), path)
  expect_equal(readLines(path), "a")
  expect_false(file.exists(paste0(path, ".etag")))
  # only the downloaded file ends up in the cache directory
  expect_equal(list.files(dirname(path)), "file.txt")

  # the cached file is used as is, also in a new session and if the remote changed
  writeLines("b", remote$path)
  tongfen:::cached_download(remote$url, path, check_remote = FALSE)
  expect_equal(readLines(path), "a")

  tongfen:::cached_download(remote$url, path, refresh = TRUE, check_remote = FALSE)
  expect_equal(readLines(path), "b")
})

test_that("cached_download: failed downloads don't leave files in the cache", {
  missing <- paste0("file://", normalizePath(tempdir(), winslash = "/"), "/does-not-exist.txt")
  path <- file.path(tempfile(), "file.txt")
  expect_error(suppressWarnings(tongfen:::cached_download(missing, path, check_remote = FALSE)))
  expect_false(file.exists(path))
  expect_equal(list.files(dirname(path)), character(0))
})

test_that("us_cache_dir: uses the given path and falls back to the tongfen cache directory", {
  local_mocked_bindings(tongfen_cache_dir = function() "tongfen/cache")
  expect_equal(tongfen:::us_cache_dir("some/path"), file.path("some/path", "us_data"))
  expect_equal(tongfen:::us_cache_dir(NULL), file.path("tongfen/cache", "us_data"))
  expect_equal(tongfen:::us_cache_dir(""), file.path("tongfen/cache", "us_data"))
})
