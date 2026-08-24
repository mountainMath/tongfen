square <- function(xmin, xmax, ymin = 0, ymax = 1) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
    c(xmin, ymax), c(xmin, ymin)
  )))
}

test_that("tongfen_estimate area-weights several variables", {
  source <- sf::st_sf(
    total = c(10, 20),
    second = c(100, 200),
    geometry = sf::st_sfc(square(0, 1), square(1, 2), crs = 3347)
  )
  target <- sf::st_sf(
    region = c("left", "middle"),
    geometry = sf::st_sfc(square(0, 0.5), square(0.5, 1.5), crs = 3347)
  )
  meta <- meta_for_additive_variables(
    "synthetic",
    c(total = "total", second = "second")
  )

  result <- tongfen_estimate(target, source, meta)

  expect_s3_class(result, "sf")
  expect_equal(result$region, c("left", "middle"))
  expect_equal(result$total, c(5, 15))
  expect_equal(result$second, c(50, 150))
  expect_equal(sf::st_geometry(result), sf::st_geometry(target))
})

test_that("tongfen_estimate preserves missing-value aggregation semantics", {
  source <- sf::st_sf(
    total = c(10, NA_real_),
    geometry = sf::st_sfc(square(0, 1), square(1, 2), crs = 3347)
  )
  target <- sf::st_sf(
    geometry = sf::st_sfc(square(0.5, 1.5), crs = 3347)
  )
  meta <- meta_for_additive_variables("synthetic", c(total = "total"))

  keep_na <- tongfen_estimate(target, source, meta, na.rm = FALSE)
  drop_na <- tongfen_estimate(target, source, meta, na.rm = TRUE)

  expect_true(is.na(keep_na$total))
  expect_equal(drop_na$total, 5)
})
