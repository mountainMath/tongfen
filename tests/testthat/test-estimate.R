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

test_that("tongfen_estimate ignores source regions that only touch the target", {
  source <- sf::st_sf(
    total = c(10, NA_real_),
    geometry = sf::st_sfc(square(0, 1), square(1, 2), crs = 3347)
  )
  target <- sf::st_sf(
    geometry = sf::st_sfc(square(0, 1), crs = 3347)
  )
  meta <- meta_for_additive_variables("synthetic", c(total = "total"))

  # the second source region shares a boundary with the target but does not overlap it
  expect_equal(tongfen_estimate(target, source, meta, na.rm = FALSE)$total, 10)
  expect_equal(tongfen_estimate(target, source, meta, na.rm = TRUE)$total, 10)
})

test_that("tongfen_estimate handles intersections that are geometry collections", {
  poly <- function(...) {
    coords <- matrix(c(...), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(rbind(coords, coords[1, ])))
  }
  # comb shaped source, overlaps the target in two separate squares of area 0.5
  # and touches it along a line in between
  comb <- poly(0,1, 0,0.5, 1,0.5, 1,1, 2,1, 2,0.5, 3,0.5, 3,1, 3,2, 0,2)
  source <- sf::st_sf(total = 100, geometry = sf::st_sfc(comb, crs = 3347))
  target <- sf::st_sf(geometry = sf::st_sfc(poly(0,0, 3,0, 3,1, 0,1), crs = 3347))
  meta <- meta_for_additive_variables("synthetic", c(total = "total"))

  intersection <- sf::st_intersection(sf::st_geometry(source), sf::st_geometry(target))
  expect_equal(as.character(sf::st_geometry_type(intersection)), "GEOMETRYCOLLECTION")

  result <- tongfen_estimate(target, source, meta)

  expect_equal(nrow(result), 1L)
  expect_equal(result$total, 100 * 1 / as.numeric(sf::st_area(source)))
})

test_that("tongfen_estimate returns NA for target regions without overlap", {
  source <- sf::st_sf(
    total = 10,
    geometry = sf::st_sfc(square(0, 1), crs = 3347)
  )
  target <- sf::st_sf(
    region = c("inside", "outside"),
    geometry = sf::st_sfc(square(0, 0.5), square(5, 6), crs = 3347)
  )
  meta <- meta_for_additive_variables("synthetic", c(total = "total"))

  result <- tongfen_estimate(target, source, meta)
  expect_equal(result$region, c("inside", "outside"))
  expect_equal(result$total, c(5, NA_real_))

  # no overlap at all
  result <- tongfen_estimate(target[2, ], source, meta)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$total))
})

test_that("tongfen_estimate estimates parent weighted averages", {
  source <- sf::st_sf(
    hh = c(10, 30),
    avg = c(2, 4),
    geometry = sf::st_sfc(square(0, 1), square(1, 2), crs = 3347)
  )
  target <- sf::st_sf(
    geometry = sf::st_sfc(square(0, 2), crs = 3347)
  )
  meta <- tibble::tibble(
    variable = c("hh", "avg"), dataset = "synthetic", label = c("hh", "avg"),
    type = "Manual", aggregation = c("Additive", "Average of hh"),
    rule = c("Additive", "Average"), geo_dataset = "synthetic",
    parent = c(NA, "hh")
  )

  result <- tongfen_estimate(target, source, meta)
  expect_equal(result$hh, 40)
  expect_equal(result$avg, 3.5)
})

test_that("tongfen_estimate complains about target columns that clash with the estimates", {
  source <- sf::st_sf(
    total = 10,
    geometry = sf::st_sfc(square(0, 1), crs = 3347)
  )
  target <- sf::st_sf(
    total = 1,
    geometry = sf::st_sfc(square(0, 0.5), crs = 3347)
  )
  meta <- meta_for_additive_variables("synthetic", c(total = "total"))

  expect_error(tongfen_estimate(target, source, meta), "already has columns named total")
})

test_that("tongfen_estimate takes averages over the regions that have a value with na.rm = TRUE", {
  source <- sf::st_sf(
    hh = c(10, 30, 60),
    avg = c(2, NA, 4),
    geometry = sf::st_sfc(square(0, 1), square(1, 2), square(2, 3), crs = 3347)
  )
  target <- sf::st_sf(
    geometry = sf::st_sfc(square(0, 3), square(0.5, 2), crs = 3347)
  )
  meta <- tibble::tibble(
    variable = c("hh", "avg"), dataset = "synthetic", label = c("hh", "avg"),
    type = "Manual", aggregation = c("Additive", "Average of hh"),
    rule = c("Additive", "Average"), geo_dataset = "synthetic",
    parent = c(NA, "hh")
  )

  result <- tongfen_estimate(target, source, meta, na.rm = TRUE)
  expect_equal(names(result), c("hh", "avg", "geometry"))
  expect_equal(result$hh, c(100, 35))
  # the second target only gets a value from half of the first source region
  expect_equal(result$avg, c((2 * 10 + 4 * 60) / 70, 2))

  expect_true(all(is.na(tongfen_estimate(target, source, meta, na.rm = FALSE)$avg)))
})
