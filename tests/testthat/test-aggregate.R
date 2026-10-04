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

# ── tongfen_aggregate ─────────────────────────────────────────────────────────

make_tongfen_data <- function(pop_a = c(100L, 200L, 50L)) {
  correspondence <- tibble(
    GeoUIDa    = c("a1", "a2", "a3"),
    GeoUIDb    = c("b1", "b1", "b2"),
    TongfenID  = c("a1", "a1", "a3"),
    TongfenUID = c("u1", "u1", "u2")
  )
  data <- list(
    A = tibble(GeoUIDa = c("a1", "a2", "a3"), pop_a = pop_a),
    B = tibble(GeoUIDb = c("b1", "b2"), pop_b = c(300L, 50L))
  )
  list(data = data, correspondence = correspondence)
}

test_that("tongfen_aggregate: aggregates both datasets onto the common geography", {
  d <- make_tongfen_data()
  meta <- bind_rows(
    make_meta("pop_a", "Additive") %>% mutate(dataset = "A", geo_dataset = "A"),
    make_meta("pop_b", "Additive") %>% mutate(dataset = "B", geo_dataset = "B")
  )
  result <- tongfen_aggregate(d$data, d$correspondence, meta)
  expect_equal(nrow(result), 2L)
  expect_equal(result %>% filter(.data$TongfenID == "a1") %>% pull(.data$pop_a), 300L)
  expect_equal(result %>% filter(.data$TongfenID == "a1") %>% pull(.data$pop_b), 300L)
})

test_that("tongfen_aggregate: na.rm is passed through to the aggregation", {
  d <- make_tongfen_data(pop_a = c(100L, NA_integer_, 50L))
  meta <- bind_rows(
    make_meta("pop_a", "Additive") %>% mutate(dataset = "A", geo_dataset = "A"),
    make_meta("pop_b", "Additive") %>% mutate(dataset = "B", geo_dataset = "B")
  )
  kept <- tongfen_aggregate(d$data, d$correspondence, meta, na.rm = FALSE)
  expect_true(is.na(kept %>% filter(.data$TongfenID == "a1") %>% pull(.data$pop_a)))

  dropped <- tongfen_aggregate(d$data, d$correspondence, meta, na.rm = TRUE)
  expect_equal(dropped %>% filter(.data$TongfenID == "a1") %>% pull(.data$pop_a), 100L)
})

test_that("tongfen_aggregate: base_geo determines which geometry is returned", {
  skip_if_not_installed("sf")
  square <- function(x, y) {
    sf::st_polygon(list(cbind(c(x, x + 1, x + 1, x, x),
                              c(y, y, y + 1, y + 1, y))))
  }
  correspondence <- tibble(
    GeoUIDa    = c("a1", "a2"),
    GeoUIDb    = c("b1", "b1"),
    TongfenID  = c("a1", "a1"),
    TongfenUID = c("u1", "u1")
  )
  data <- list(
    A = sf::st_sf(GeoUIDa = c("a1", "a2"), pop_a = c(100L, 200L),
                  geometry = sf::st_sfc(square(0, 0), square(1, 0), crs = 3347)),
    B = sf::st_sf(GeoUIDb = "b1", pop_b = 300L,
                  geometry = sf::st_sfc(square(0, 0), crs = 3347))
  )
  meta <- bind_rows(
    make_meta("pop_a", "Additive") %>% mutate(dataset = "A", geo_dataset = "A"),
    make_meta("pop_b", "Additive") %>% mutate(dataset = "B", geo_dataset = "B")
  )
  # base geography A is the union of the two squares, base geography B is one square
  result_a <- tongfen_aggregate(data, correspondence, meta, base_geo = "A")
  result_b <- tongfen_aggregate(data, correspondence, meta, base_geo = "B")
  expect_s3_class(result_a, "sf")
  expect_equal(as.numeric(sf::st_area(result_a)), 2)
  expect_equal(as.numeric(sf::st_area(result_b)), 1)
})

test_that("tongfen_aggregate: returns a plain tibble when no dataset has geometry", {
  d <- make_tongfen_data()
  meta <- bind_rows(
    make_meta("pop_a", "Additive") %>% mutate(dataset = "A", geo_dataset = "A"),
    make_meta("pop_b", "Additive") %>% mutate(dataset = "B", geo_dataset = "B")
  )
  result <- tongfen_aggregate(d$data, d$correspondence, meta)
  expect_false("sf" %in% class(result))
})

# ── same variable name in several datasets ───────────────────────────────────

make_average_meta <- function(ds) {
  bind_rows(
    make_meta("hh",  "Additive"),
    make_meta("avg", "Average", parent = "hh")
  ) %>%
    mutate(dataset = ds, geo_dataset = ds,
           label = paste0(.data$variable, "_", ds))
}

test_that("aggregate_data_with_meta: variables listed for several datasets are only scaled once", {
  data <- tibble(
    group = c("A", "A"),
    hh    = c(10, 30),
    avg   = c(2, 4)
  ) %>% group_by(.data$group)

  meta <- bind_rows(make_average_meta("X"), make_average_meta("Y"))

  result <- aggregate_data_with_meta(data, meta, quiet = TRUE)
  expect_equal(result$hh, 40)
  expect_equal(result$avg, (2 * 10 + 4 * 30) / 40)
})

test_that("aggregate_data_with_meta: conflicting rules for the same variable are an error", {
  data <- tibble(
    group = c("A", "A"),
    hh    = c(10, 30),
    avg   = c(2, 4)
  ) %>% group_by(.data$group)

  meta <- bind_rows(
    make_average_meta("X"),
    make_meta("avg", "Additive") %>% mutate(geo_dataset = "Y")
  )

  expect_error(aggregate_data_with_meta(data, meta, quiet = TRUE), "Conflicting aggregation rules")
})

test_that("tongfen_aggregate: averages use the rules of their own dataset when variable names repeat", {
  correspondence <- tibble(
    GeoUIDX    = c("x1", "x2"),
    GeoUIDY    = c("y1", "y1"),
    TongfenID  = c("x1", "x1"),
    TongfenUID = c("u1", "u1")
  )
  data <- list(
    X = tibble(GeoUIDX = c("x1", "x2"), hh = c(10, 30), avg = c(2, 4)),
    Y = tibble(GeoUIDY = "y1", hh = 50, avg = 5)
  )
  meta <- bind_rows(make_average_meta("X"), make_average_meta("Y"))

  result <- tongfen_aggregate(data, correspondence, meta)

  expect_equal(nrow(result), 1L)
  expect_equal(result$hh_X, 40)
  expect_equal(result$avg_X, 3.5)
  expect_equal(result$hh_Y, 50)
  expect_equal(result$avg_Y, 5)

  # same variable name but a different rule in the second dataset
  meta_mixed <- bind_rows(
    make_average_meta("X"),
    make_average_meta("Y") %>% mutate(rule = "Additive", parent = NA_character_)
  )
  result <- tongfen_aggregate(data, correspondence, meta_mixed)
  expect_equal(result$avg_X, 3.5)
  expect_equal(result$avg_Y, 5)
})

# ── averages with missing values ─────────────────────────────────────────────

test_that("aggregate_data_with_meta: missing averages don't count toward the weights with na.rm=TRUE", {
  data <- tibble(
    group = c("A", "A", "A", "B", "B"),
    hh    = c(10, 30, 60, 5, 15),
    avg   = c(2, NA, 4, NA, NA),
    avg2  = c(1, 2, 3, 4, NA)
  ) %>% group_by(.data$group)

  meta <- bind_rows(
    make_meta("hh",   "Additive"),
    make_meta("avg",  "Average", parent = "hh"),
    make_meta("avg2", "Average", parent = "hh")
  )

  result <- aggregate_data_with_meta(data, meta, na.rm = TRUE, quiet = TRUE)

  expect_equal(names(result), c("group", "hh", "avg", "avg2"))
  # the parent keeps counting all regions
  expect_equal(result$hh, c(100, 20))
  # the average is taken over the regions that have a value, each average on its own
  expect_equal(result$avg[1], (2 * 10 + 4 * 60) / 70)
  expect_true(is.na(result$avg[2]))
  expect_equal(result$avg2, c((1 * 10 + 2 * 30 + 3 * 60) / 100, 4))

  kept <- aggregate_data_with_meta(data, meta, na.rm = FALSE, quiet = TRUE)
  expect_true(all(is.na(kept$avg)))
  expect_equal(kept$avg2[1], (1 * 10 + 2 * 30 + 3 * 60) / 100)
  expect_true(is.na(kept$avg2[2]))
})

# ── "Average to" variables ───────────────────────────────────────────────────

make_average_to_meta <- function(variable, units = "Percentage (0-100)") {
  make_meta(variable, "AverageTo", parent = "pop") %>% mutate(units = units)
}

test_that("aggregate_data_with_meta: percentage change is averaged relative to its base", {
  # population grew from 100 to 110 and from 100 to 220
  data <- tibble(
    group = c("A", "A"),
    pop   = c(110, 220),
    chg   = c(10, 120)
  ) %>% group_by(.data$group)

  meta <- bind_rows(
    make_meta("pop", "Additive") %>% mutate(units = "Number"),
    make_average_to_meta("chg")
  )

  result <- aggregate_data_with_meta(data, meta, quiet = TRUE)
  expect_equal(result$pop, 330)
  expect_equal(result$chg, 65)
  expect_equal(result$base_pop, 200)
})

test_that("aggregate_data_with_meta: 'Average to' variables sharing a parent don't interfere", {
  data <- tibble(
    group = c("A", "A"),
    pop   = c(110, 220),
    chg1  = c(10, 10),
    chg2  = c(100, 100),
    ratio = c(0.1, 1.2)
  ) %>% group_by(.data$group)

  meta <- bind_rows(
    make_meta("pop", "Additive") %>% mutate(units = "Number"),
    make_average_to_meta("chg1"),
    make_average_to_meta("chg2"),
    make_average_to_meta("ratio", units = "Percentage ratio (0.0-1.0)")
  )

  result <- aggregate_data_with_meta(data, meta, quiet = TRUE)
  expect_equal(result$chg1, 10)
  expect_equal(result$chg2, 100)
  expect_equal(result$ratio, 0.65)
  # each variable comes with its own base
  expect_equal(result$base_chg1, 300)
  expect_equal(result$base_chg2, 165)
  expect_equal(result$base_ratio, 200)

  # same results as when aggregating the variables one at a time
  for (v in c("chg1", "chg2", "ratio")) {
    single <- aggregate_data_with_meta(data, meta %>% filter(.data$variable %in% c("pop", v)),
                                       quiet = TRUE)
    expect_equal(single[[v]], result[[v]])
    expect_equal(single$base_pop, result[[paste0("base_", v)]])
  }
})
