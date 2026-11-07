# ── StatCan correspondence files ──────────────────────────────────────────────

# Small correspondence files in the layout of the hosted files, in a fresh mirror and cache directory.
# Joint regions across 1991, 1996 and 2001 are {A, e1, e2, f1} and {B, C, e3, f2}.
local_statcan_mirror <- function(env = parent.frame()) {
  mirror <- tempfile("mirror")
  cache <- tempfile("cache")
  dir.create(mirror)
  dir.create(cache)
  files <- list(
    "2006" = tibble::tibble(DBUID2006 = c("A01", "A02", "B01", "D01"), DBUID2001 = c("a1", "a2", "b1", "c1"),
                            DBRELFLAG = 1L, DAUID2006 = c("A", "A", "B", "D"), DAUID2001 = c("A", "A", "B", "C"),
                            DARELFLAG = c(1L, 1L, 1L, 1L)),
    "2001" = tibble::tibble(DBUID2001 = c("a1", "a2", "b1", "c1", "c1"), DAUID2001 = c("A", "A", "B", "C", "C"),
                            EAUID1996 = c("e1", "e2", "e3", "e3", "e4"), EAPARTFLAG = c(0L, 0L, 1L, 1L, 0L),
                            EAPOP1996 = 1L, EAPOP2001 = 1L),
    "1996" = tibble::tibble(EAUID1996 = c("e1", "e2", "e3", "e4"), EAUID1991 = c("f1", "f1", "f2", "f2"),
                            EAPARTFLAG = c(1L, 1L, 1L, 1L))
  )
  for (year in names(files)) {
    nanoparquet::write_parquet(files[[year]], file.path(mirror, paste0("correspondence_", year, ".parquet")))
  }
  withr::local_options(tongfen.statcan_correspondence_url = paste0("file://", normalizePath(mirror, winslash = "/")),
                       tongfen.cache_path = cache, .local_envir = env)
  local_mocked_bindings(remote_etag = function(url) NULL, .env = env)
  rm(list = ls(tongfen:::tongfen_session), envir = tongfen:::tongfen_session)
  invisible(mirror)
}

test_that("statcan_unit_level: enumeration areas up to 1996", {
  expect_equal(tongfen:::statcan_unit_level(c(1981, 1996, 2001, 2021), "DA"), c("EA", "EA", "DA", "DA"))
  expect_equal(tongfen:::statcan_unit_level(1996, "DB"), "EA")
  expect_equal(tongfen:::normalize_ca_level("EA"), "DA")
  expect_equal(tongfen:::normalize_ca_level("CT"), "CT")
})

test_that("normalize_datasets: translates census years back to 1981", {
  expect_equal(tongfen:::normalize_datasets(c("1981", "1996", "2001", "CA21")),
               c("CA1981", "CA1996", "CA01", "CA21"))
  expect_equal(tongfen:::years_from_datasets(c("CA1981", "CA1996", "CA01", "CA21")),
               c(1981L, 1996L, 2001L, 2021L))
})

test_that("get_single_correspondence_ca_census_for: selects the links for the level", {
  local_statcan_mirror()
  quietly <- function(x) suppressMessages(x)

  da <- quietly(tongfen:::get_single_correspondence_ca_census_for(2006, "DA"))
  expect_named(da, c("DAUID2006", "DAUID2001", "flag"))
  # DA links are repeated for each DB in the file
  expect_equal(nrow(da), 3)

  db <- quietly(tongfen:::get_single_correspondence_ca_census_for(2006, "DB"))
  expect_named(db, c("DBUID2006", "DBUID2001", "flag"))
  expect_equal(nrow(db), 4)

  da01 <- quietly(tongfen:::get_single_correspondence_ca_census_for(2001, "DA"))
  expect_named(da01, c("DAUID2001", "EAUID1996", "flag"))
  # A/e1, A/e2, B/e3, C/e3 and C/e4, the population columns are dropped
  expect_equal(nrow(da01), 5)

  db01 <- quietly(tongfen:::get_single_correspondence_ca_census_for(2001, "DB"))
  expect_named(db01, c("DBUID2001", "EAUID1996", "flag"))

  ea <- quietly(tongfen:::get_single_correspondence_ca_census_for(1996, "DA"))
  expect_named(ea, c("EAUID1996", "EAUID1991", "flag"))
  expect_equal(quietly(tongfen:::get_single_correspondence_ca_census_for(1996, "DB")), ea)
})

test_that("get_single_correspondence_ca_census_for: rejects unsupported years and files", {
  mirror <- local_statcan_mirror()
  expect_error(tongfen:::get_single_correspondence_ca_census_for(1976, "DA"), "Year needs to be one of")
  nanoparquet::write_parquet(tibble::tibble(EAUID1991 = "f1", EAUID1986 = "g1"),
                             file.path(mirror, "correspondence_1991.parquet"))
  expect_error(suppressMessages(tongfen:::get_single_correspondence_ca_census_for(1991, "DA")),
               "does not have the expected columns EAPARTFLAG")
})

# ── statcan method ────────────────────────────────────────────────────────────

local_mock_cancensus <- function(env = parent.frame()) {
  # the regions of each census, at DA (or EA) level with the CT they are in
  census_regions <- list(
    CA1991 = tibble::tibble(GeoUID = c("f1", "f2"), CT_UID = c("T1", "T2")),
    CA1996 = tibble::tibble(GeoUID = c("e1", "e2", "e3", "e4"), CT_UID = c("T1", "T1", "T2", "T2")),
    CA01 = tibble::tibble(GeoUID = c("A", "B", "C"), CT_UID = c("S1", "S2", "S2")),
    CA06 = tibble::tibble(GeoUID = c("A", "B", "D"), CT_UID = c("S1", "S2", "S2"))
  )
  local_mocked_bindings(
    get_census = function(dataset, regions, level, ...) {
      d <- census_regions[[dataset]]
      if (level == "CT") d <- d %>% dplyr::distinct(GeoUID = .data$CT_UID)
      d
    },
    list_census_datasets = function(...) {
      tibble::tibble(dataset = names(census_regions),
                     description = paste0(c(1991, 1996, 2001, 2006), " Canada Census"),
                     geo_dataset = names(census_regions))
    },
    .package = "cancensus", .env = env
  )
}

joint_regions <- function(correspondence, column) {
  correspondence %>%
    dplyr::group_by(.data$TongfenID) %>%
    dplyr::summarize(ids = paste0(sort(unique(.data[[column]])), collapse = ",")) %>%
    dplyr::pull(.data$ids) %>%
    sort()
}

test_that("statcan method: chains DA level links through enumeration areas", {
  skip_if_not_installed("cancensus")
  local_statcan_mirror()
  local_mock_cancensus()

  correspondence <- suppressMessages(
    get_tongfen_correspondence_ca_census(c("CA1991", "CA01"), regions = list(PR = "59"), level = "DA",
                                         quiet = TRUE))
  expect_true(all(c("GeoUIDCA1991", "GeoUIDCA01", "TongfenID", "TongfenUID") %in% names(correspondence)))
  expect_equal(joint_regions(correspondence, "GeoUIDCA01"), c("A", "B,C"))
  expect_equal(joint_regions(correspondence, "GeoUIDCA1991"), c("f1", "f2"))

  # EA is an alias for DA
  ea <- suppressMessages(
    get_tongfen_correspondence_ca_census(c("CA1991", "CA01"), regions = list(PR = "59"), level = "EA",
                                         quiet = TRUE))
  expect_equal(ea, correspondence)
})

test_that("statcan method: CT level links through enumeration areas", {
  skip_if_not_installed("cancensus")
  local_statcan_mirror()
  local_mock_cancensus()

  correspondence <- suppressMessages(
    get_tongfen_correspondence_ca_census(c("CA1991", "CA06"), regions = list(PR = "59"), level = "CT",
                                         quiet = TRUE))
  expect_equal(joint_regions(correspondence, "GeoUIDCA06"), c("S1", "S2"))
  expect_equal(joint_regions(correspondence, "GeoUIDCA1991"), c("T1", "T2"))
})

test_that("statcan method: checks datasets and levels", {
  skip_if_not_installed("cancensus")
  expect_error(get_tongfen_correspondence_ca_census(c("CA1996", "CA01"), regions = list(PR = "59"), level = "DB"),
               "Level DB only works for census years 2001 through 2021")
  expect_error(get_tongfen_correspondence_ca_census(c("CA1986", "CA21"), regions = list(PR = "59"),
                                                    method = "estimate"),
               "geographies of CA1986 are incomplete")
  expect_error(get_tongfen_correspondence_ca_census(c("CA1976", "CA21"), regions = list(PR = "59")),
               "only works for census years 1981 through 2021")
  meta <- tibble::tibble(variable = c("v_CA1986_1", "v_CA21_1"), geo_dataset = c("CA1986", "CA21"))
  expect_error(get_tongfen_ca_census(regions = list(PR = "59"), meta = meta, base_geo = "CA1986"),
               "geography of CA1986 is incomplete")
})
