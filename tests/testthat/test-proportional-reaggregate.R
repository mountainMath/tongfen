library(dplyr)
library(sf)

# ── Test fixtures ─────────────────────────────────────────────────────────────

# Minimal sf wrapper: adds a point geometry column so st_drop_geometry works
as_point_sf <- function(df, crs = 4326) {
  n <- nrow(df)
  df %>%
    mutate(.lon = seq_len(n), .lat = 0) %>%
    sf::st_as_sf(coords = c(".lon", ".lat"), crs = crs)
}

# Two parent regions P1 (pop=1000) and P2 (pop=2000)
# Four children: C1,C2,C3 under P1; C4 under P2
make_numeric_fixture <- function() {
  parent <- tibble(
    parent_id   = c("P1", "P2"),
    pop         = c(1000, 2000),
    numeric_var = c(500.0, 600.0)
  ) %>% as_point_sf()

  child <- tibble(
    child_id    = c("C1", "C2", "C3", "C4"),
    parent_id   = c("P1", "P1", "P1", "P2"),
    pop         = c(200,  300,  500, 2000),
    numeric_var = c(100.0, 150.0, 200.0, 600.0)
  ) %>% as_point_sf()

  list(parent = parent, child = child)
}

# ── Basic numeric reaggregation ───────────────────────────────────────────────

test_that("proportional_reaggregate: result has same number of rows as child data", {
  fix <- make_numeric_fixture()
  result <- proportional_reaggregate(
    fix$child, fix$parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "numeric_var",
    base       = "pop"
  )
  expect_equal(nrow(result), nrow(fix$child))
})

test_that("proportional_reaggregate: children sum to parent total after reaggregation", {
  fix <- make_numeric_fixture()
  result <- proportional_reaggregate(
    fix$child, fix$parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "numeric_var",
    base       = "pop"
  )
  result_tbl <- result %>% sf::st_drop_geometry()
  # P1 children should sum to 500
  p1_sum <- result_tbl %>% filter(.data$parent_id == "P1") %>% pull(.data$numeric_var) %>% sum()
  expect_equal(p1_sum, 500, tolerance = 1e-9)
  # P2 child should equal 600
  p2_val <- result_tbl %>% filter(.data$parent_id == "P2") %>% pull(.data$numeric_var)
  expect_equal(p2_val, 600, tolerance = 1e-9)
})

test_that("proportional_reaggregate: weights are proportional to base variable", {
  fix <- make_numeric_fixture()
  result <- proportional_reaggregate(
    fix$child, fix$parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "numeric_var",
    base       = "pop"
  )
  result_tbl <- result %>% sf::st_drop_geometry() %>% arrange(.data$child_id)
  # C1: 100 + (200/1000)*(500-450) = 100 + 10 = 110
  # C2: 150 + (300/1000)*(500-450) = 150 + 15 = 165
  # C3: 200 + (500/1000)*(500-450) = 200 + 25 = 225
  expect_equal(result_tbl$numeric_var, c(110, 165, 225, 600), tolerance = 1e-9)
})

test_that("proportional_reaggregate: result retains all original child columns", {
  fix <- make_numeric_fixture()
  result <- proportional_reaggregate(
    fix$child, fix$parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "numeric_var",
    base       = "pop"
  )
  expect_true("child_id" %in% names(result))
  expect_true("pop"      %in% names(result))
})

test_that("proportional_reaggregate: missing category column is filled from parent", {
  # Child data has no numeric_var column at all
  child_no_var <- tibble(
    child_id  = c("C1", "C2"),
    parent_id = c("P1", "P2"),
    pop       = c(500, 2000)
  ) %>% as_point_sf()

  parent <- tibble(
    parent_id   = c("P1", "P2"),
    pop         = c(1000, 2000),
    numeric_var = c(500.0, 600.0)
  ) %>% as_point_sf()

  result <- proportional_reaggregate(
    child_no_var, parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "numeric_var",
    base       = "pop"
  )
  # All child values come from parent (child had NA, so p_value fills in)
  result_tbl <- result %>% sf::st_drop_geometry() %>% arrange(.data$child_id)
  expect_equal(result_tbl$numeric_var, c(500.0, 600.0))
})

# ── Factor column reaggregation ───────────────────────────────────────────────

test_that("proportional_reaggregate: plain factor column is preserved in result", {
  parent <- tibble(
    parent_id = c("P1", "P2"),
    pop       = c(1000, 2000),
    label     = factor(c("Low", "High"))
  ) %>% as_point_sf()

  child <- tibble(
    child_id  = c("C1", "C2", "C3"),
    parent_id = c("P1", "P1", "P2"),
    pop       = c(400, 600, 2000),
    label     = factor(c("Low", "Low", "High"))
  ) %>% as_point_sf()

  result <- proportional_reaggregate(
    child, parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "label",
    base       = "pop"
  )
  expect_true("label" %in% names(result))
})

# ── Bug #2: ordered factor in missing category column ────────────────────────
#
# BUG: When a category is absent from `data` but present in `parent_data` as
# an ordered factor, `class()` returns c("ordered","factor"). The code then
# calls `get(paste0("as.", c("ordered","factor")))(NA)`, which passes a
# length-2 vector to `get()` and throws "first argument has length > 1".
#
# This test expresses the CORRECT expected behaviour. It currently FAILS.

test_that("BUG #2: ordered factor missing from child data is filled with NA (not an error)", {
  parent <- tibble(
    parent_id = c("P1", "P2"),
    pop       = c(1000, 2000),
    rank      = factor(c("Low", "High"), levels = c("Low", "High"), ordered = TRUE)
  ) %>% as_point_sf()

  # Child has no `rank` column → triggers the get(paste0("as.",...)) path
  child <- tibble(
    child_id  = c("C1", "C2"),
    parent_id = c("P1", "P2"),
    pop       = c(1000, 2000)
  ) %>% as_point_sf()

  result <- proportional_reaggregate(
    child, parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "rank",
    base       = "pop"
  )
  expect_true("rank" %in% names(result))
})

# ── Bug #3: ordered factor column present in both, silently dropped ───────────
#
# BUG: `unique(var_types) %>% unlist()` flattens c("ordered","factor") into
# two separate strings. The subsequent `names(var_types)[var_types == vt]`
# list equality returns character(0) for both, so the column never enters
# any type bucket and is absent from the joined result.
#
# This test expresses the CORRECT expected behaviour. It currently FAILS.

test_that("BUG #3: ordered factor column present in both data and parent is included in result", {
  lvls <- c("Low", "Medium", "High")

  parent <- tibble(
    parent_id = c("P1", "P2"),
    pop       = c(1000, 2000),
    rank      = factor(c("Low", "High"), levels = lvls, ordered = TRUE)
  ) %>% as_point_sf()

  child <- tibble(
    child_id  = c("C1", "C2", "C3"),
    parent_id = c("P1", "P1", "P2"),
    pop       = c(400, 600, 2000),
    rank      = factor(c("Low", "Low", "High"), levels = lvls, ordered = TRUE)
  ) %>% as_point_sf()

  result <- proportional_reaggregate(
    child, parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = "rank",
    base       = "pop"
  )

  expect_true("rank" %in% names(result))
  # All child ranks are present and non-NA
  result_tbl <- result %>% sf::st_drop_geometry()
  expect_false(any(is.na(result_tbl$rank)))
})

# ── Bug #4: data already has a column named "...id" ──────────────────────────
#
# BUG: The collision-avoidance while-loop bumps `id` to "......id" when
# "...id" already exists, but the `Reduce` at the end hard-codes
# by="...id". When categories contain more than one distinct R type
# (here: numeric + character), `Reduce` is called with a 2-element list
# and tries to join on the non-existent column "...id" → error.
#
# This test expresses the CORRECT expected behaviour. It currently FAILS.

test_that("BUG #4: proportional_reaggregate works when child data has a column named '...id'", {
  parent <- tibble(
    parent_id   = c("P1", "P2"),
    pop         = c(1000, 2000),
    numeric_var = c(500.0, 600.0),
    char_var    = c("a", "b")        # second type forces Reduce to join
  ) %>% as_point_sf()

  # Column "...id" collides with the internal sentinel → id bumps to "......id"
  child <- tibble(
    child_id    = c("C1", "C2", "C3"),
    parent_id   = c("P1", "P1", "P2"),
    pop         = c(200, 800, 2000),
    numeric_var = c(100.0, 380.0, 600.0),
    char_var    = c("a", "a", "b"),
    `...id`     = c("x", "y", "z")
  ) %>% as_point_sf()

  result <- proportional_reaggregate(
    child, parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = c("numeric_var", "char_var"),
    base       = c(numeric_var = "pop", char_var = NA)
  )

  expect_equal(nrow(result), nrow(child))
  result_tbl <- result %>% sf::st_drop_geometry()
  # numeric_var in P1 children should sum to parent value 500
  p1_sum <- result_tbl %>% filter(.data$parent_id == "P1") %>% pull(.data$numeric_var) %>% sum()
  expect_equal(p1_sum, 500, tolerance = 1e-9)
})

# ── Mixed numeric + factor categories ────────────────────────────────────────

test_that("proportional_reaggregate: handles numeric and factor categories together", {
  parent <- tibble(
    parent_id   = c("P1", "P2"),
    pop         = c(1000, 2000),
    numeric_var = c(500.0, 600.0),
    label       = factor(c("Low", "High"))
  ) %>% as_point_sf()

  child <- tibble(
    child_id    = c("C1", "C2", "C3"),
    parent_id   = c("P1", "P1", "P2"),
    pop         = c(200, 800, 2000),
    numeric_var = c(100.0, 380.0, 600.0),
    label       = factor(c("Low", "Low", "High"))
  ) %>% as_point_sf()

  result <- proportional_reaggregate(
    child, parent,
    geo_match  = c("parent_id" = "parent_id"),
    categories = c("numeric_var", "label"),
    base       = c(numeric_var = "pop", label = NA)
  )

  expect_equal(nrow(result), nrow(child))
  expect_true(all(c("numeric_var", "label") %in% names(result)))
  result_tbl <- result %>% sf::st_drop_geometry()
  p1_sum <- result_tbl %>% filter(.data$parent_id == "P1") %>% pull(.data$numeric_var) %>% sum()
  expect_equal(p1_sum, 500, tolerance = 1e-9)
})
