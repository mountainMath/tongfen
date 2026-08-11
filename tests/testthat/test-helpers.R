library(dplyr)

# Helpers for building correspondence input tables
make_correspondence <- function(...) {
  tibble(...)
}

# ── get_tongfen_correspondence ────────────────────────────────────────────────

test_that("get_tongfen_correspondence: each row gets a TongfenID and TongfenUID", {
  dd <- make_correspondence(
    geo_a = c("A1", "A2"),
    geo_b = c("B1", "B2")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_true(all(c("TongfenID", "TongfenUID") %in% names(result)))
  expect_equal(nrow(result), 2L)
})

test_that("get_tongfen_correspondence: disjoint pairs remain separate components", {
  dd <- make_correspondence(
    geo_a = c("A1", "A2"),
    geo_b = c("B1", "B2")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 2L)
})

test_that("get_tongfen_correspondence: shared geo ID merges two rows into one component", {
  # A1-B1, A1-B2 → A1, B1, B2 in same component
  dd <- make_correspondence(
    geo_a = c("A1", "A1"),
    geo_b = c("B1", "B2")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 1L)
})

test_that("get_tongfen_correspondence: transitive connections are resolved", {
  # A1-B1, A2-B1 → A1, A2, B1 in same component
  # A3-B2 → separate
  dd <- make_correspondence(
    geo_a = c("A1", "A2", "A3"),
    geo_b = c("B1", "B1", "B2")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 2L)
  # A1 and A2 must share a TongfenID
  id_A1 <- result %>% filter(.data$geo_a == "A1") %>% pull(.data$TongfenID) %>% unname()
  id_A2 <- result %>% filter(.data$geo_a == "A2") %>% pull(.data$TongfenID) %>% unname()
  expect_equal(id_A1, id_A2)
})

test_that("get_tongfen_correspondence: multi-hop transitive chain is resolved", {
  # A1-B2, A2-B2, A2-B3, A3-B3 → all four in one component
  dd <- make_correspondence(
    geo_a = c("A1", "A2", "A2", "A3"),
    geo_b = c("B2", "B2", "B3", "B3")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 1L)
})

test_that("get_tongfen_correspondence: TongfenUID encodes all geo IDs in component", {
  dd <- make_correspondence(
    geo_a = c("A1", "A2", "A3"),
    geo_b = c("B1", "B1", "B2")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  # Component with A1+A2 should list both in UID
  uid_A1 <- result %>% filter(.data$geo_a == "A1") %>% pull(.data$TongfenUID)
  expect_true(grepl("A1", uid_A1))
  expect_true(grepl("A2", uid_A1))
  # A3 component should not mention A1 or A2
  uid_A3 <- result %>% filter(.data$geo_a == "A3") %>% pull(.data$TongfenUID)
  expect_false(grepl("A1", uid_A3))
})

test_that("get_tongfen_correspondence: all input rows are present in output", {
  dd <- make_correspondence(
    geo_a = c("A1", "A1", "A2", "A3"),
    geo_b = c("B1", "B2", "B2", "B3")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(nrow(result), nrow(dd))
})

test_that("get_tongfen_correspondence: single row input returns one component", {
  dd <- make_correspondence(geo_a = "A1", geo_b = "B1")
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(nrow(result), 1L)
  expect_equal(n_distinct(result$TongfenID), 1L)
})

test_that("get_tongfen_correspondence: three-column input supported", {
  dd <- make_correspondence(
    geo_a = c("A1", "A2"),
    geo_b = c("B1", "B2"),
    geo_c = c("C1", "C2")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 2L)
})

test_that("get_tongfen_correspondence: NA identifiers do not link rows", {
  # rows that only share a missing identifier belong to separate components
  dd <- make_correspondence(
    geo_a = c("A1", "A2", "A3"),
    geo_b = c(NA, NA, "B3")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 3L)
  # a missing identifier is not listed in the UID either
  expect_false(any(grepl("NA", result$TongfenUID)))
})

test_that("get_tongfen_correspondence: rows are labelled from later columns when first is NA", {
  dd <- make_correspondence(
    geo_a = c(NA, "A2"),
    geo_b = c("B1", "B2")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 2L)
  expect_equal(result$TongfenID[[1]], "2_B1")
})

test_that("get_tongfen_correspondence: TongfenID is unnamed", {
  dd <- make_correspondence(geo_a = c("A1", "A2"), geo_b = c("B1", "B2"))
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_null(names(result$TongfenID))
})

test_that("get_tongfen_correspondence: TongfenMethod column is not treated as an identifier", {
  dd <- make_correspondence(
    geo_a = c("A1", "A2"),
    geo_b = c("B1", "B2"),
    TongfenMethod = c("statcan", "statcan")
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 2L)
  expect_false(any(grepl("TongfenMethod", result$TongfenUID)))
})

test_that("get_tongfen_correspondence: resolves a long chain of linked rows", {
  # zig-zag chain: every row links to the next through alternating columns, the
  # worst case for label propagation without pointer jumping
  n <- 20000
  i <- seq_len(n)
  dd <- make_correspondence(
    geo_a = paste0("x", ceiling(i / 2)),
    geo_b = paste0("y", ceiling((i + 1) / 2))
  )
  result <- tongfen:::get_tongfen_correspondence(dd)
  expect_equal(n_distinct(result$TongfenID), 1L)
  expect_equal(nrow(result), n)
})

# ── collapse_unique_by_row ────────────────────────────────────────────────────

test_that("collapse_unique_by_row: collapses distinct values per row", {
  d <- tibble(a = c("x", "x", "y"), b = c("x", "y", "y"))
  expect_equal(tongfen:::collapse_unique_by_row(d, c("a", "b")),
               c("x", "x, y", "y"))
})

test_that("collapse_unique_by_row: works for a single column", {
  d <- tibble(a = c("x", "y"))
  expect_equal(tongfen:::collapse_unique_by_row(d, "a"), c("x", "y"))
})

# ── aggregate_correspondences ─────────────────────────────────────────────────

test_that("aggregate_correspondences: joins on shared identifier and merges methods", {
  cl <- list(
    tibble(A = c("a1", "a2"), B = c("b1", "b2"), TongfenMethod = "statcan"),
    tibble(B = c("b1", "b2"), C = c("c1", "c2"), TongfenMethod = "estimate")
  )
  result <- tongfen:::aggregate_correspondences(cl)
  expect_equal(sort(names(result)), c("A", "B", "C", "TongfenMethod"))
  expect_equal(nrow(result), 2L)
  expect_equal(unique(result$TongfenMethod), "statcan, estimate")
})

test_that("aggregate_correspondences: uses every input correspondence exactly once", {
  cl <- list(
    tibble(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"), TongfenMethod = "statcan"),
    tibble(B = c("b1", "b2"), C = c("c1", "c2"), TongfenMethod = "statcan"),
    tibble(C = c("c1", "c2", "c3", "c4"), D = c("d1", "d2", "d3", "d4"), TongfenMethod = "statcan")
  )
  result <- tongfen:::aggregate_correspondences(cl)
  expect_equal(sort(names(result)), c("A", "B", "C", "D", "TongfenMethod"))
  expect_equal(nrow(result), 2L)
})

# ── summarize_geometry_by_group ───────────────────────────────────────────────

test_that("summarize_geometry_by_group: matches grouped st_union", {
  skip_if_not_installed("sf")
  square <- function(x, y) {
    sf::st_polygon(list(cbind(c(x, x + 1, x + 1, x, x),
                              c(y, y, y + 1, y + 1, y))))
  }
  # groups: "a" two adjacent squares, "b" a single square, "c" two disjoint squares
  d <- sf::st_sf(
    grp = c("a", "a", "b", "c", "c"),
    geometry = sf::st_sfc(square(0, 0), square(1, 0), square(5, 5),
                          square(10, 10), square(20, 20), crs = 3347)
  )

  expected <- d %>%
    group_by(.data$grp) %>%
    summarize(geometry = suppressMessages(sf::st_union(.data$geometry)) %>%
                sf::st_cast("MULTIPOLYGON"), .groups = "drop")
  result <- tongfen:::summarize_geometry_by_group(d %>% group_by(.data$grp), "grp")

  expect_equal(result$grp, expected$grp)
  expect_equal(as.numeric(sf::st_area(result)), as.numeric(sf::st_area(expected)))
  expect_true(all(vapply(seq_len(nrow(result)),
                         function(i) sf::st_equals(result$geometry[i], expected$geometry[i],
                                                   sparse = FALSE)[1, 1],
                         logical(1))))
  expect_equal(sf::st_crs(result), sf::st_crs(d))
  expect_false(dplyr::is_grouped_df(result))
})

test_that("summarize_geometry_by_group: supports multiple grouping columns", {
  skip_if_not_installed("sf")
  square <- function(x, y) {
    sf::st_polygon(list(cbind(c(x, x + 1, x + 1, x, x),
                              c(y, y, y + 1, y + 1, y))))
  }
  d <- sf::st_sf(
    TongfenID = c("a", "a", "b"),
    TongfenUID = c("u1", "u1", "u2"),
    geometry = sf::st_sfc(square(0, 0), square(1, 0), square(5, 5), crs = 3347)
  )
  result <- tongfen:::summarize_geometry_by_group(d, c("TongfenID", "TongfenUID"))
  expect_equal(nrow(result), 2L)
  expect_equal(result$TongfenID, c("a", "b"))
  expect_equal(result$TongfenUID, c("u1", "u2"))
})
