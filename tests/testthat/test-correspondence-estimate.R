library(dplyr)
library(sf)

# unit squares in a projected crs so that the buffer tolerance is in metres
sq <- function(x0, y0, w, h) {
  st_polygon(list(cbind(c(x0, x0 + w, x0 + w, x0, x0),
                        c(y0, y0, y0 + h, y0 + h, y0))))
}

grid_4 <- function(id_column) {
  d <- st_sf(id = c("a1", "a2", "a3", "a4"),
             geometry = st_sfc(sq(0, 0, 1, 1), sq(1, 0, 1, 1),
                               sq(0, 1, 1, 1), sq(1, 1, 1, 1),
                               crs = 3347))
  names(d)[1] <- id_column
  d
}

test_that("estimate_tongfen_correspondence: pairs of regions merge into one component", {
  geo_a <- grid_4("idA")
  geo_b <- st_sf(idB = c("b1", "b2"),
                 geometry = st_sfc(sq(0, 0, 2, 1), sq(0, 1, 2, 1), crs = 3347))

  correspondence <- estimate_tongfen_correspondence(list(geo_a, geo_b),
                                                    c("idA", "idB"),
                                                    tolerance = 0.01)
  expect_equal(nrow(correspondence), 4L)
  expect_equal(length(unique(correspondence$TongfenID)), 2L)
  expect_equal(correspondence$TongfenID[correspondence$idB == "b1"],
               rep(correspondence$TongfenID[correspondence$idA == "a1"], 2))
})

test_that("estimate_tongfen_correspondence: identifier method works when every id matches", {
  # no region is left over for the geometric estimate, the intersection is empty
  correspondence <- estimate_tongfen_correspondence(list(grid_4("idA"), grid_4("idB")),
                                                    c("idA", "idB"),
                                                    method = "identifier",
                                                    tolerance = 0.01)
  expect_equal(nrow(correspondence), 4L)
  expect_equal(correspondence$idA, correspondence$idB)
  expect_true(all(correspondence$TongfenMethod == "identifier"))
})

test_that("estimate_tongfen_correspondence: identifier method estimates the unmatched rest", {
  geo_a <- grid_4("idA")
  geo_c <- st_sf(idB = c("a1", "a2", "x"),
                 geometry = st_sfc(sq(0, 0, 1, 1), sq(1, 0, 1, 1), sq(0, 1, 2, 1),
                                   crs = 3347))

  correspondence <- estimate_tongfen_correspondence(list(geo_a, geo_c),
                                                    c("idA", "idB"),
                                                    method = "identifier",
                                                    tolerance = 0.01)
  matched <- correspondence %>% filter(.data$idA %in% c("a1", "a2"))
  expect_true(all(matched$TongfenMethod == "identifier"))
  estimated <- correspondence %>% filter(.data$idB == "x")
  expect_setequal(estimated$idA, c("a3", "a4"))
  expect_equal(length(unique(estimated$TongfenID)), 1L)
})

test_that("estimate_tongfen_correspondence: needs at least two geographies", {
  expect_error(estimate_tongfen_correspondence(list(grid_4("idA")), "idA"),
               "at least two")
})

test_that("tongfen_tag_largest_overlap: tags each source region by its containing target", {
  source <- grid_4("idA")
  target <- st_sf(t = c("t1", "t2"),
                  geometry = st_sfc(sq(0, 0, 2, 1), sq(0, 1, 2, 1), crs = 3347))

  tagged <- tongfen_tag_largest_overlap(source, target, "t") %>% st_drop_geometry()
  expect_equal(tagged$t, c("t1", "t1", "t2", "t2"))
  expect_equal(as.numeric(tagged[["...overlap_fraction"]]), rep(1, 4), tolerance = 1e-9)
})
