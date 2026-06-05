library(dplyr)

# Build a minimal meta tibble for testing
make_meta <- function(variable, rule, parent = NA_character_) {
  tibble(
    variable    = variable,
    rule        = rule,
    parent      = parent,
    type        = rule,
    dataset     = "test",
    label       = variable,
    aggregation = rule,
    geo_dataset = "test"
  )
}

# ── aggregate_data_with_meta: additive variables ──────────────────────────────

test_that("aggregate_data_with_meta: additive variables sum correctly", {
  data <- tibble(
    group = c("A", "A", "B", "B"),
    pop   = c(100L, 200L, 150L, 50L)
  ) %>% group_by(.data$group)

  meta <- make_meta("pop", "Additive")

  result <- aggregate_data_with_meta(data, meta, quiet = TRUE)

  expect_equal(nrow(result), 2L)
  expect_equal(sort(result$pop), c(200L, 300L))
})

test_that("aggregate_data_with_meta: NAs are handled with na.rm=TRUE", {
  data <- tibble(
    group = c("A", "A", "B"),
    pop   = c(100L, NA_integer_, 50L)
  ) %>% group_by(.data$group)

  meta <- make_meta("pop", "Additive")
  result <- aggregate_data_with_meta(data, meta, na.rm = TRUE, quiet = TRUE)

  expect_equal(result %>% filter(.data$group == "A") %>% pull(.data$pop), 100L)
})

test_that("aggregate_data_with_meta: NAs propagate with na.rm=FALSE", {
  data <- tibble(
    group = c("A", "A", "B"),
    pop   = c(100L, NA_integer_, 50L)
  ) %>% group_by(.data$group)

  meta <- make_meta("pop", "Additive")
  result <- aggregate_data_with_meta(data, meta, na.rm = FALSE, quiet = TRUE)

  expect_true(is.na(result %>% filter(.data$group == "A") %>% pull(.data$pop)))
})

test_that("aggregate_data_with_meta: meta variables not in data are silently ignored", {
  data <- tibble(group = c("A", "B"), pop = c(100L, 200L)) %>% group_by(.data$group)
  meta <- bind_rows(
    make_meta("pop", "Additive"),
    make_meta("missing_var", "Additive")
  )
  expect_no_error(aggregate_data_with_meta(data, meta, quiet = TRUE))
})

# ── Bug #1: Average/Median variables on grouped data ─────────────────────────
#
# BUG: Inside mutate(across(..., ~ .x * data[[parent_lookup[cur_column()]]])),
# `data` is the ungrouped function parameter captured by closure. When dplyr
# evaluates the lambda per-group, `.x` is a group-sized slice but
# `data[[col]]` returns the full-length column → length mismatch → error.
#
# These tests express the CORRECT expected behaviour. They currently FAIL
# because of the bug and should PASS after the fix.

test_that("BUG #1: aggregate_data_with_meta handles Average variable on grouped data", {
  # Two groups, two rows each
  data <- tibble(
    group      = c("A", "A", "B", "B"),
    pop        = c(100, 200, 150,  50),
    avg_income = c(50000, 60000, 40000, 70000)
  ) %>% group_by(.data$group)

  meta <- bind_rows(
    make_meta("pop",        "Additive"),
    make_meta("avg_income", "Average",  parent = "pop")
  )

  # Group A: pop_total=300, weighted avg = (100*50000 + 200*60000)/300 = 56666.67
  # Group B: pop_total=200, weighted avg = (150*40000 +  50*70000)/200 = 47500
  result <- aggregate_data_with_meta(data, meta, quiet = TRUE)

  expect_equal(nrow(result), 2L)
  result_A <- result %>% filter(.data$group == "A")
  expect_equal(result_A$avg_income, 56666 + 2/3, tolerance = 0.1)
  result_B <- result %>% filter(.data$group == "B")
  expect_equal(result_B$avg_income, 47500, tolerance = 0.1)
})

test_that("BUG #1: aggregate_data_with_meta handles Median variable on grouped data", {
  data <- tibble(
    group      = c("A", "A", "B"),
    pop        = c(100, 200, 300),
    med_income = c(40000, 60000, 50000)
  ) %>% group_by(.data$group)

  meta <- bind_rows(
    make_meta("pop",        "Additive"),
    make_meta("med_income", "Median",   parent = "pop")
  )

  expect_no_error(aggregate_data_with_meta(data, meta, quiet = TRUE))
})

# ── pre_scale / post_scale ────────────────────────────────────────────────────

test_that("pre_scale multiplies Average variable by its parent", {
  data <- tibble(pop = c(100, 200), avg_income = c(50000, 60000))
  meta <- tibble(
    variable    = c("pop", "avg_income"),
    data_var    = c("pop", "avg_income"),
    rule        = c("Additive", "Average"),
    parent      = c(NA_character_, "pop"),
    parent_name = c(NA_character_, "pop")
  )
  result <- tongfen:::pre_scale(data, meta, quiet = TRUE)
  # avg_income should now be avg_income * pop
  expect_equal(result$avg_income, c(50000 * 100, 60000 * 200))
})

test_that("post_scale divides Average variable by its parent", {
  data <- tibble(pop = c(100, 200), avg_income = c(5e6, 12e6))
  meta <- tibble(
    variable    = c("pop", "avg_income"),
    data_var    = c("pop", "avg_income"),
    rule        = c("Additive", "Average"),
    parent      = c(NA_character_, "pop"),
    parent_name = c(NA_character_, "pop")
  )
  result <- tongfen:::post_scale(data, meta)
  expect_equal(result$avg_income, c(5e6 / 100, 12e6 / 200))
})

test_that("pre_scale is a no-op when there are no Average/Median variables", {
  data <- tibble(pop = c(100, 200))
  meta <- tibble(
    variable    = "pop",
    data_var    = "pop",
    rule        = "Additive",
    parent      = NA_character_,
    parent_name = NA_character_
  )
  result <- tongfen:::pre_scale(data, meta, quiet = TRUE)
  expect_equal(result, data)
})
