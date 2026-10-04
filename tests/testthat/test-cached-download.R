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
