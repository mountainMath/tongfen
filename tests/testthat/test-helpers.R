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
