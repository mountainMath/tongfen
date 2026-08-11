test_that("us tract correspondence links chain across censuses", {
  l9000 <- tibble::tibble(GEOID90=c("a","b","c"),
                          GEOID00=c("A","A","C"))
  l0010 <- tibble::tibble(GEOID00=c("A","C","D"),
                          GEOID10=c("1","2","3"))
  l1020 <- tibble::tibble(GEOID10=c("1","2","3"),
                          GEOID20=c("X","X","Y"))

  all <- tongfen:::join_us_correspondence(list(l9000,l0010,l1020),
                                             c("dec1990","dec2000","dec2010","dec2020"))
  expect_equal(names(all),c("GEOID90","GEOID00","GEOID10","GEOID20"))
  # tract D has no 1990 predecessor but is kept
  expect_equal(nrow(all),4)
  expect_true(all(c("a","b","c") %in% all$GEOID90))
  expect_true(is.na(all$GEOID90[all$GEOID00=="D"]))

  # censuses that only serve as stepping stones get dropped
  ends <- tongfen:::join_us_correspondence(list(l9000,l0010,l1020),
                                              c("dec1990","dec2020"))
  expect_equal(names(ends),c("GEOID90","GEOID20"))
  expect_equal(sort(ends$GEOID20[!is.na(ends$GEOID90)]),c("X","X","X"))

  short <- tongfen:::join_us_correspondence(list(l9000),c("dec1990","dec2000"))
  expect_equal(nrow(short),3)
})

test_that("us correspondence spans intermediate censuses", {
  years <- names(tongfen:::us_geoid_columns)

  expect_equal(tongfen:::us_correspondence_span(c("dec1990","dec2020"),years),years)
  expect_equal(tongfen:::us_correspondence_span(c("dec2000","dec2010"),years),
               c("dec2000","dec2010"))
  # order of the requested censuses does not matter
  expect_equal(tongfen:::us_correspondence_span(c("dec2020","dec2000"),years),
               c("dec2000","dec2010","dec2020"))

  expect_error(tongfen:::us_correspondence_span(c("dec1980","dec1990"),years),"dec1980")
  expect_error(tongfen:::us_correspondence_span("dec1990",years),"at least two")
  # county subdivisions have no 1990 relationship file
  expect_error(tongfen:::us_correspondence_span(c("dec1990","dec2000"),years[-1]),"dec1990")
})

test_that("us correspondence validates the requested censuses", {
  expect_error(tongfen:::get_us_ct_correspondence("RI",c("dec1980","dec1990")),"dec1980")
  expect_error(tongfen:::get_us_ct_correspondence("RI","dec1990"),"at least two")
  expect_error(tongfen:::get_us_county_subdivision_correspondence_for("RI",c("dec1990","dec2000")),
               "dec1990")
})

test_that("us tract relationship file paths are built for all census pairs", {
  skip_if_not_installed("tidycensus")

  expect_equal(tongfen:::get_us_ct_correspondence_path("RI","2000"),
               "https://www2.census.gov/geo/relfiles/tract/ri/ri44pop.txt")
  expect_equal(tongfen:::get_us_ct_correspondence_path("RI","2010"),
               "https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/ri44trf.txt")
  expect_equal(tongfen:::get_us_ct_correspondence_path("RI","2020"),
               paste0("https://www2.census.gov/geo/docs/maps-data/data/rel2020/t10t20/",
                      "TAB2010_TAB2020_ST44.zip"))
  expect_error(tongfen:::get_us_ct_correspondence_path("RI","1990"),"1990")
})
