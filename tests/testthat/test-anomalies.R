library(dplyr)
library(sf)

sq <- function(x0, y0, w, h) {
  st_polygon(list(cbind(c(x0, x0 + w, x0 + w, x0, x0),
                        c(y0, y0, y0 + h, y0 + h, y0))))
}

# regions lined up in a row, each one only neighbours the one before and after
row_regions <- function(ids, values) {
  geometry <- st_sfc(lapply(seq_along(ids), \(i) sq(i - 1, 0, 1, 1)), crs = 3347)
  st_sf(bind_cols(tibble(TongfenID = ids), as_tibble(values)), geometry = geometry)
}

years <- c("2001", "2006", "2011", "2016")

# population moves from A to B between 2001 and 2006 while C stays flat, like dwellings
# getting geocoded to the neighbouring region
misallocation <- function() {
  row_regions(c("A", "B", "C"),
              rbind(c(1000, 400, 410, 420),
                    c(500, 1100, 1110, 1120),
                    c(800, 805, 810, 815)) %>%
                `colnames<-`(years))
}

# population in A drops and no neighbour picks it up, like a redevelopment site getting cleared
real_drop <- function() {
  row_regions(c("A", "B", "C"),
              rbind(c(700, 710, 720, 730),
                    c(1000, 300, 310, 320),
                    c(800, 805, 810, 815)) %>%
                `colnames<-`(years))
}

# A loses to B between 2001 and 2006, B loses to C between 2006 and 2011. A only matches
# up with its neighbour once B and C are joined.
chain <- function() {
  row_regions(c("A", "B", "C", "D"),
              rbind(c(1000, 600, 600, 600),
                    c(500, 900, 400, 400),
                    c(500, 500, 1000, 1000),
                    c(800, 800, 800, 800)) %>%
                `colnames<-`(years))
}

# ── surprise ──────────────────────────────────────────────────────────────────

test_that("anomaly_surprise: only decreases that are large in relative and absolute terms surprise", {
  surprise <- \(change, base) tongfen:::anomaly_surprise(change, base, rel_scale = 0.25, abs_scale = 200)

  # half way to full surprise for both the relative and the absolute decrease
  expect_equal(surprise(-200, 800), 0.25)
  expect_equal(surprise(-400, 800), 0.75 * 0.75)
  # large relative but small absolute decrease, and the other way around
  expect_lt(surprise(-10, 20), 0.04)
  expect_lt(surprise(-200, 100000), 0.01)
  expect_equal(surprise(c(100, 0, NA, -50, 0), c(800, 800, 800, NA, 0)), rep(0, 5))
  # decrease from nothing
  expect_equal(surprise(-50, 0), 0)

  m <- surprise(matrix(c(-200, 100, -400, NA), 2), matrix(800, 2, 2))
  expect_equal(m, matrix(c(0.25, 0, 0.75 * 0.75, 0), 2))
})

# ── detection ─────────────────────────────────────────────────────────────────

test_that("tongfen_detect_anomalies: complementary change in a neighbour qualifies for joining", {
  anomalies <- tongfen_detect_anomalies(misallocation(), years, total_surprise_cutoff = 0.4)

  expect_equal(names(anomalies),
               c("TongfenID", "surprise_count", "surprise_total", "period", "neighbour",
                 "surprise_total_joined", "join"))
  expect_equal(nrow(anomalies), 1L)
  expect_equal(anomalies$TongfenID, "A")
  expect_equal(anomalies$surprise_count, 1L)
  expect_equal(anomalies$surprise_total, (1 - 0.5^(0.6 / 0.25)) * (1 - 0.5^3))
  expect_equal(anomalies$period, "2001-2006")
  expect_equal(anomalies$neighbour, "B")
  expect_equal(anomalies$surprise_total_joined, 0)
  expect_true(anomalies$join)
})

test_that("tongfen_detect_anomalies: drop without complementary change is listed but not joined", {
  anomalies <- tongfen_detect_anomalies(real_drop(), years)

  expect_equal(anomalies$TongfenID, "B")
  expect_false(anomalies$join)
  expect_equal(anomalies$surprise_total_joined, anomalies$surprise_total, tolerance = 0.01)
  expect_equal(nrow(tongfen_anomaly_joins(real_drop(), years)), 0L)
})

test_that("tongfen_detect_anomalies: no candidates gives an empty table", {
  data <- misallocation()
  anomalies <- tongfen_detect_anomalies(data, years[2:4])
  expect_equal(nrow(anomalies), 0L)
  expect_equal(names(anomalies),
               names(tongfen_detect_anomalies(data, years, total_surprise_cutoff = 0.4)))
  expect_equal(nrow(tongfen_anomaly_joins(data, years[2:4])), 0L)
})

test_that("tongfen_detect_anomalies: candidates are ordered by surprise", {
  anomalies <- tongfen_detect_anomalies(chain(), years, total_surprise_cutoff = 0.4)
  expect_equal(anomalies$TongfenID, c("B", "A"))
  expect_equal(anomalies$period, c("2006-2011", "2001-2006"))
  expect_equal(anomalies$neighbour, c("C", "B"))
  expect_equal(anomalies$join, c(TRUE, FALSE))
})

test_that("tongfen_detect_anomalies: missing values are not surprising", {
  data <- misallocation()
  data$`2006`[1] <- NA
  expect_equal(nrow(tongfen_detect_anomalies(data, years, total_surprise_cutoff = 0.4)), 0L)
  expect_equal(nrow(tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4)), 0L)

  # joined regions are missing the values their parts are missing
  data <- chain()
  data$`2016`[3] <- NA
  joins <- tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4)
  expect_equal(joins$TongfenID, c("A", "B", "C"))
  # a missing value in a neighbour does not make up for a surprising change
  data$`2006`[3] <- NA
  anomalies <- tongfen_detect_anomalies(data, years, total_surprise_cutoff = 0.4)
  expect_equal(anomalies$TongfenID, c("B", "A"))
  expect_equal(anomalies$surprise_total_joined[1], anomalies$surprise_total[1])
  expect_equal(anomalies$join, c(FALSE, FALSE))
  expect_equal(nrow(tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4)), 0L)
})

test_that("tongfen_detect_anomalies: candidate without neighbours", {
  data <- misallocation() %>% st_drop_geometry()
  anomalies <- tongfen_detect_anomalies(data, years, total_surprise_cutoff = 0.4,
                                        neighbours = tibble(a = "B", b = "C"))
  expect_equal(anomalies$TongfenID, "A")
  expect_true(is.na(anomalies$neighbour))
  expect_true(is.na(anomalies$surprise_total_joined))
  expect_false(anomalies$join)
})

# ── joins ─────────────────────────────────────────────────────────────────────

test_that("tongfen_anomaly_joins: joins regions with complementary changes", {
  joins <- tongfen_anomaly_joins(misallocation(), years, total_surprise_cutoff = 0.4)
  expect_equal(joins, tibble(TongfenID = c("A", "B"), TongfenID_joined = "A", round = 1L))

  # too high a bar for regions to become candidates
  expect_equal(nrow(tongfen_anomaly_joins(misallocation(), years, total_surprise_cutoff = 0.9)), 0L)
})

test_that("tongfen_anomaly_joins: joined regions keep getting joined", {
  joins <- tongfen_anomaly_joins(chain(), years, total_surprise_cutoff = 0.4)
  expect_equal(joins, tibble(TongfenID = c("A", "B", "C"), TongfenID_joined = "A",
                             round = c(2L, 1L, 1L)))
})

test_that("tongfen_anomaly_joins: joined regions are named after their smallest identifier", {
  data <- chain() %>% mutate(TongfenID = c("59150010", "5915002", "59150003", "59150004"))
  joins <- tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4)
  # sorted like the identifiers of a correspondence, character by character
  expect_equal(joins$TongfenID, c("59150003", "59150010", "5915002"))
  expect_true(all(joins$TongfenID_joined == "59150003"))
})

test_that("tongfen_anomaly_joins: result does not depend on the order of the regions", {
  # A drops and both neighbours make up for it in full
  data <- row_regions(c("B", "A", "C"),
                      rbind(c(500, 1100, 1110, 1120),
                            c(1000, 400, 410, 420),
                            c(500, 1100, 1110, 1120)) %>%
                        `colnames<-`(years))
  expected <- tibble(TongfenID = c("A", "B"), TongfenID_joined = "A", round = 1L)
  expect_equal(tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4), expected)
  expect_equal(tongfen_anomaly_joins(data[3:1, ], years, total_surprise_cutoff = 0.4), expected)
  expect_equal(tongfen_detect_anomalies(data[3:1, ], years, total_surprise_cutoff = 0.4),
               tongfen_detect_anomalies(data, years, total_surprise_cutoff = 0.4))
})

test_that("tongfen_anomaly_joins: identifier column can go by any name and type", {
  data <- chain() %>% st_drop_geometry() %>% mutate(TongfenID = c(40L, 30L, 20L, 10L)) %>%
    rename(GeoUID = "TongfenID")
  joins <- tongfen_anomaly_joins(data, years, id = "GeoUID", total_surprise_cutoff = 0.4,
                                 neighbours = tibble(a = c(40L, 30L, 20L), b = c(30L, 20L, 10L)))
  expect_equal(joins, tibble(GeoUID = c(20L, 30L, 40L), GeoUID_joined = 20L,
                             round = c(1L, 1L, 2L)))
})

test_that("tongfen_anomaly_joins: neighbours can be specified", {
  data <- chain()
  expected <- tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4)
  data <- data %>% st_drop_geometry()

  neighbour_table <- tibble(id1 = c("A", "B", "C", "A", "X"), id2 = c("B", "C", "D", "A", "A"))
  expect_equal(tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4,
                                     neighbours = neighbour_table),
               expected)

  # neighbours list in the format of the spdep package
  nb <- list(2L, c(1L, 3L), c(2L, 4L), 3L)
  attr(nb, "region.id") <- data$TongfenID
  class(nb) <- "nb"
  expect_equal(tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4, neighbours = nb),
               expected)

  # in a different order than the data, and with a region without neighbours
  nb <- list(0L, 3L, c(2L, 4L), c(3L, 5L), 4L)
  attr(nb, "region.id") <- c("Z", rev(data$TongfenID))
  expect_equal(tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4, neighbours = nb),
               expected)

  # without the link between B and C nothing can get joined
  expect_equal(nrow(tongfen_anomaly_joins(data, years, total_surprise_cutoff = 0.4,
                                          neighbours = neighbour_table[-2, ])), 0L)
})

test_that("tongfen_anomaly_joins: checks inputs", {
  data <- misallocation()
  expect_error(tongfen_anomaly_joins(st_drop_geometry(data), years), "neighbours")
  expect_error(tongfen_anomaly_joins(data, years, neighbours = "B"), "neighbours")
  expect_error(tongfen_anomaly_joins(data, "2001"), "at least two")
  expect_error(tongfen_anomaly_joins(data, c("2001", "2026")), "2026")
  expect_error(tongfen_anomaly_joins(data, years, id = "GeoUID"), "GeoUID")
  expect_error(tongfen_anomaly_joins(data %>% mutate(`2006` = as.character(.data$`2006`)), years),
               "numeric")
  expect_error(tongfen_anomaly_joins(data %>% mutate(TongfenID = c("A", "A", "B")), years),
               "uniquely")
  expect_error(tongfen_anomaly_joins(data, years, p = c(2, 4)), "single number")
  expect_error(tongfen_detect_anomalies(data, years, abs_scale = 0), "positive")
})

# ── comparison with the original implementation ───────────────────────────────

# The method was first implemented in
# https://doodles.mountainmath.ca/posts/2024-07-26-geocoding-errors-in-aggregate-data/
# This is the code from that post, with the neighbours derived via `st_intersects` instead
# of `spdep::poly2nb`. It works on timelines in columns named by year and names joined
# regions by stringing together their identifiers.
blog_rel_surprise <- function(x) {
  r <- as.integer(x <= 0 & !is.na(x) & is.finite(x))
  r[r == 1] <- 1 - dexp(x[r == 1] * log(0.5) / 0.25, rate = 1)
  r
}

blog_abs_surprise <- function(x) {
  r <- as.integer(x <= 0 & !is.na(x) & is.finite(x))
  r[r == 1] <- 1 - dexp(x[r == 1] * log(0.5) / 200, rate = 1)
  r
}

blog_add_surprise <- function(data, base_field = "value") {
  data |>
    arrange(Year) |>
    mutate(`Absolute change` = value - lag(value, order_by = Year),
           `Relative change` = `Absolute change` / lag(!!as.name(base_field), order_by = Year),
           .by = TongfenID) |>
    mutate(surprise = (blog_rel_surprise(`Relative change`)) * (blog_abs_surprise(`Absolute change`)))
}

blog_get_match_list <- function(geo_data2,
                                cutoff_fact = 0.6, total_surprise_cutoff = 0.75,
                                surprise_reduction_const = 0.15, sum_fact = 0.7, p = 4) {
  intersecting <- st_intersects(geo_data2)
  data_nb <- lapply(seq_along(intersecting), \(i) geo_data2$TongfenID[setdiff(intersecting[[i]], i)]) |>
    setNames(geo_data2$TongfenID)

  summarize_surprise <- function(data, fields = "surprise") {
    data |>
      summarize(across(all_of(fields), list(count = ~sum(.x > 0.15),
                                            total = ~(sum(.x^p))^(1 / p))),
                .by = TongfenID) |>
      arrange(!!as.name(paste0(fields[1], "_count")), !!as.name(paste0(fields[1], "_total")))
  }

  long_data2 <- geo_data2 |>
    st_drop_geometry() |>
    select(TongfenID, matches("^\\d{4}$")) |>
    tidyr::pivot_longer(matches("^\\d{4}$"), names_to = "Year") |>
    blog_add_surprise()

  candidate_list <- long_data2 |>
    summarize_surprise() |>
    purrr::map_df(rev) |>
    filter(surprise_count > 0, surprise_total > total_surprise_cutoff)

  match_list <- tibble(TongfenID = NA_character_, TongfenID_original = NA_character_) |>
    slice(-1)

  if (nrow(candidate_list) == 0) return(match_list)

  for (i in 1:nrow(candidate_list)) {
    id0 <- candidate_list$TongfenID[i]

    if (id0 %in% match_list$TongfenID_original) next

    neighbour_ids <- data_nb[[id0]]

    if (length(neighbour_ids) == 0) next

    od <- long_data2 |>
      filter(TongfenID %in% c(id0))

    original <- od |>
      summarize_surprise()

    reductions <- long_data2 |>
      filter(TongfenID %in% c(neighbour_ids)) |>
      left_join(od |> select(Year, ov = value), by = "Year") |>
      mutate(value = value + ov) |>
      blog_add_surprise(base_field = "ov") |>
      rename(surprise_ov = surprise) |>
      blog_add_surprise() |>
      summarize_surprise(c("surprise_ov", "surprise")) |>
      slice(1) |>
      rename(TongfenID2 = TongfenID) |>
      mutate(TongfenID1 = original$TongfenID, .before = TongfenID2) |>
      mutate(TongfenID = paste0(TongfenID1, "_", TongfenID2))

    if (reductions$TongfenID2 %in% c(match_list$TongfenID_original)) next

    pre_sum <- long_data2 |>
      filter(TongfenID %in% c(reductions$TongfenID1, reductions$TongfenID2)) |>
      summarize_surprise()

    if (reductions$surprise_ov_total < cutoff_fact * original$surprise_total |
        original$surprise_total - reductions$surprise_ov_total > surprise_reduction_const |
        reductions$surprise_ov_total < cutoff_fact * sum_fact * sum(pre_sum$surprise_total)) {
      match_list <- bind_rows(match_list,
                              reductions |>
                                tidyr::pivot_longer(c(TongfenID1, TongfenID2),
                                                    values_to = "TongfenID_original"))
    }
  }
  match_list
}

blog_iterate_geo_match_joins <- function(geo_data2, ...) {
  stop_looking <- FALSE

  while (!stop_looking) {
    match_list <- blog_get_match_list(geo_data2, ...)

    if (nrow(match_list) == 0) {
      stop_looking <- TRUE
      next
    }

    g1 <- geo_data2 |>
      rename(TongfenID_original = TongfenID) |>
      inner_join(match_list |> select(TongfenID, TongfenID_original),
                 by = "TongfenID_original",
                 relationship = "many-to-one") |>
      mutate(TongfenID = coalesce(TongfenID, TongfenID_original)) |>
      select(-TongfenID_original) |>
      group_by(TongfenID) |>
      summarize(across(matches("\\d{4}"), sum), .groups = "drop") |>
      st_make_valid()

    g2 <- geo_data2 |>
      filter(!(TongfenID %in% match_list$TongfenID_original))

    geo_data2 <- bind_rows(g1, g2)
  }
  geo_data2
}

# Regions on a grid with slowly declining counts, and counts that get moved between
# neighbouring regions for a couple of years. The counts are not rounded and all
# regions change in all periods to keep clear of ties, the two implementations
# order regions differently after the first round of joins.
random_timelines <- function(n, n_moves, n_drops, seed) {
  set.seed(seed)
  cells <- expand.grid(x = seq_len(n), y = seq_len(n))
  n_years <- 6
  values <- matrix(runif(n * n, 300, 1500), n * n, n_years) -
    t(apply(matrix(runif(n * n * n_years, 1, 20), n * n, n_years), 1, cumsum))
  cell_index <- \(x, y) (y - 1) * n + x

  for (move in seq_len(n_moves)) {
    from <- sample(n * n, 1)
    step <- list(c(1, 0), c(-1, 0), c(0, 1), c(0, -1))[[sample(4, 1)]]
    x <- cells$x[from] + step[1]
    y <- cells$y[from] + step[2]
    if (x < 1 || x > n || y < 1 || y > n) next
    to <- cell_index(x, y)
    start <- sample(2:n_years, 1)
    moved_years <- start:sample(start:n_years, 1)
    amount <- runif(1, 0.2, 0.8) * min(values[from, ])
    values[from, moved_years] <- values[from, moved_years] - amount
    values[to, moved_years] <- values[to, moved_years] + amount
  }
  for (drop in seq_len(n_drops)) {
    region <- sample(n * n, 1)
    dropped_years <- sample(2:n_years, 1):n_years
    values[region, dropped_years] <- values[region, dropped_years] * runif(1, 0.3, 0.7)
  }
  colnames(values) <- seq(1996, by = 5, length.out = n_years)

  geometry <- st_sfc(lapply(seq_len(n * n), \(i) sq(cells$x[i], cells$y[i], 1, 1)), crs = 3347)
  st_sf(bind_cols(tibble(TongfenID = sprintf("r%03d", sample(n * n))), as_tibble(values)),
        geometry = geometry)
}

test_that("tongfen_anomaly_joins: agrees with the original implementation", {
  rounds <- c()
  for (seed in 1:4) {
    data <- random_timelines(n = 7, n_moves = 30, n_drops = 5, seed = seed)
    timeline <- names(data)[grepl("^\\d{4}$", names(data))]

    joins <- tongfen_anomaly_joins(data, timeline, total_surprise_cutoff = 0.4)
    blog_ids <- blog_iterate_geo_match_joins(data, total_surprise_cutoff = 0.4)$TongfenID
    blog_groups <- strsplit(blog_ids, "_", fixed = TRUE)
    blog_groups <- blog_groups[lengths(blog_groups) > 1] %>%
      vapply(\(g) paste0(sort(g), collapse = "_"), character(1))
    groups <- split(joins$TongfenID, joins$TongfenID_joined) %>%
      vapply(\(g) paste0(sort(g), collapse = "_"), character(1))

    expect_gt(length(groups), 3)
    expect_setequal(unname(groups), blog_groups)
    rounds <- c(rounds, max(joins$round))

    # stricter parameters
    joins <- tongfen_anomaly_joins(data, timeline, cutoff_fact = 0.3, surprise_reduction_const = 0.4,
                                   sum_fact = 0.5, p = 2)
    blog_ids <- blog_iterate_geo_match_joins(data, cutoff_fact = 0.3, surprise_reduction_const = 0.4,
                                             sum_fact = 0.5, p = 2)$TongfenID
    expect_equal(nrow(data) - n_distinct(joins$TongfenID) + n_distinct(joins$TongfenID_joined),
                 length(blog_ids))
    expect_setequal(joins$TongfenID, unlist(strsplit(blog_ids[grepl("_", blog_ids)], "_", fixed = TRUE)))
  }
  # some of the timelines need several rounds of joins
  expect_gt(max(rounds), 1)
})

test_that("tongfen_anomaly_joins: random timelines don't depend on the order of the regions", {
  data <- random_timelines(n = 6, n_moves = 25, n_drops = 4, seed = 11)
  timeline <- names(data)[grepl("^\\d{4}$", names(data))]
  joins <- tongfen_anomaly_joins(data, timeline, total_surprise_cutoff = 0.4)
  expect_gt(nrow(joins), 6)
  expect_equal(tongfen_anomaly_joins(data[sample(nrow(data)), ], timeline, total_surprise_cutoff = 0.4),
               joins)
})

# ── joining regions ───────────────────────────────────────────────────────────

joins_ab <- tibble(TongfenID = c("A", "B"), TongfenID_joined = "A", round = 1L)

test_that("tongfen_join_regions: aggregates joined regions and leaves the rest alone", {
  data <- misallocation() %>%
    mutate(TongfenUID = paste0("GeoUID16:", c("1", "2,3", "4"), " GeoUID21:", c("11,12", "13", "14")),
           .after = "TongfenID")

  expect_message(result <- tongfen_join_regions(data, joins_ab), "additive")
  expect_s3_class(result, "sf")
  expect_equal(names(result), names(data))
  expect_equal(result$TongfenID, c("A", "C"))
  expect_equal(result$TongfenUID, c("GeoUID16:1,2,3 GeoUID21:11,12,13", "GeoUID16:4 GeoUID21:14"))
  expect_equal(result$`2001`, c(1500, 800))
  expect_equal(result$`2016`, c(1540, 815))
  expect_equal(as.numeric(st_area(result)), c(2, 1))
  expect_true(st_equals(st_geometry(result)[1], sq(0, 0, 2, 1), sparse = FALSE)[1, 1])
  expect_true(st_equals(st_geometry(result)[2], st_geometry(data)[3], sparse = FALSE)[1, 1])
  expect_equal(st_crs(result), st_crs(data))
  expect_equal(result %>% st_drop_geometry() %>% filter(.data$TongfenID == "C"),
               data %>% st_drop_geometry() %>% filter(.data$TongfenID == "C"))

  # the timeline of the joined region does not have anything surprising left
  expect_equal(nrow(tongfen_detect_anomalies(result, years, total_surprise_cutoff = 0.4)), 0L)
})

test_that("tongfen_join_regions: works on data without geometry and keeps the order of regions", {
  data <- chain() %>% st_drop_geometry() %>% slice(c(4, 2, 3, 1))
  joins <- tibble(TongfenID = c("C", "B"), TongfenID_joined = "B")

  result <- suppressMessages(tongfen_join_regions(data, joins))
  expect_false(inherits(result, "sf"))
  expect_equal(result$TongfenID, c("D", "B", "A"))
  expect_equal(result$`2011`, c(800, 1400, 600))

  # nothing to join
  expect_equal(tongfen_join_regions(data, joins[0, ]), data)
  expect_equal(tongfen_join_regions(data, tibble(TongfenID = "X", TongfenID_joined = "Y")), data)
  # regions other regions get joined to don't have to be listed
  result <- suppressMessages(tongfen_join_regions(data, joins[1, ]))
  expect_equal(result$TongfenID, c("D", "B", "A"))
  expect_equal(result$`2011`, c(800, 1400, 600))
})

test_that("tongfen_join_regions: aggregates according to metadata", {
  data <- tibble(TongfenID = c("A", "B", "C"),
                 Population_CA16 = c(100, 300, 50),
                 Income_CA16 = c(10, 20, 30),
                 Population_CA21 = c(300, 100, 60),
                 Income_CA21 = c(40, 20, 35),
                 name = c("a", "b", "c"))
  meta <- tibble(variable = c("v_pop", "v_income", "v_pop", "v_income"),
                 label = c("Population_CA16", "Income_CA16", "Population_CA21", "Income_CA21"),
                 rule = c("Additive", "Average", "Additive", "Average"),
                 parent = c(NA, "v_pop", NA, "v_pop"),
                 type = "Original",
                 geo_dataset = c("CA16", "CA16", "CA21", "CA21"))

  expect_message(result <- tongfen_join_regions(data, joins_ab, meta), "name")
  expect_equal(result$TongfenID, c("A", "C"))
  expect_equal(result$Population_CA16, c(400, 50))
  expect_equal(result$Income_CA16, c((100 * 10 + 300 * 20) / 400, 30))
  expect_equal(result$Income_CA21, c((300 * 40 + 100 * 20) / 400, 35))
  expect_equal(result$name, c(NA, "c"))

  # count variables that are not part of the metadata, like the ones census calls add
  expect_message(result <- tongfen_join_regions(data %>% mutate(Dwellings_CA16 = c(40, 110, 20)),
                                                joins_ab, meta),
                 "not part of the metadata as additive: Dwellings_CA16")
  expect_equal(result$Dwellings_CA16, c(150, 20))
  expect_equal(result$Income_CA16, c((100 * 10 + 300 * 20) / 400, 30))

  # averages can't be aggregated without their parent
  expect_error(tongfen_join_regions(data %>% select(-"Population_CA21"), joins_ab, meta),
               "Income_CA21.*tongfen_join_correspondence")
  # treating averages as additive is on the user
  result <- suppressMessages(tongfen_join_regions(data, joins_ab))
  expect_equal(result$Income_CA16, c(30, 30))
})

test_that("tongfen_join_regions: checks inputs", {
  data <- misallocation()
  expect_error(tongfen_join_regions(data, joins_ab %>% select(-"TongfenID_joined")), "TongfenID_joined")
  expect_error(tongfen_join_regions(data, joins_ab, id = "GeoUID"), "GeoUID")
  expect_error(tongfen_join_regions(bind_rows(data, data), joins_ab), "uniquely")
})

test_that("merge_tongfen_uids: combines the identifiers by geography", {
  merge_uids <- tongfen:::merge_tongfen_uids
  expect_equal(merge_uids(c("a:2,3 b:10", "a:1 b:9,10")), "a:1,2,3 b:10,9")
  expect_equal(merge_uids(c("a:1 b:9,10", "a:2,3 b:10")), "a:1,2,3 b:10,9")
  expect_equal(merge_uids("a:1 b:9"), "a:1 b:9")
  # regions that are not part of all geographies
  expect_equal(merge_uids(c("a:1 ", "a:2 b:3")), "a:1,2 b:3")
  # not in the format of a TongfenUID
  expect_equal(merge_uids(c("x", "y", "x")), "x y")
})

# ── joining correspondences ───────────────────────────────────────────────────

# two geographies on a grid of four regions, the second one has the top row in one piece
two_geographies <- function() {
  ids <- c("a1", "a2", "a3", "a4")
  geo_a <- st_sf(idA = ids, Population = c(1000, 400, 300, 500),
                 geometry = st_sfc(sq(0, 0, 1, 1), sq(1, 0, 1, 1), sq(0, 1, 1, 1), sq(1, 1, 1, 1),
                                   crs = 3347))
  geo_b <- st_sf(idB = c("b1", "b2", "b3"), Population = c(400, 1050, 820),
                 geometry = st_sfc(sq(0, 0, 1, 1), sq(1, 0, 1, 1), sq(0, 1, 2, 1), crs = 3347))
  correspondence <- tibble(idA = ids, idB = c("b1", "b2", "b3", "b3")) %>%
    tongfen:::get_tongfen_correspondence()
  list(data = list(A = geo_a, B = geo_b), correspondence = correspondence,
       meta = meta_for_additive_variables(c("A", "B"), "Population"))
}

test_that("tongfen_join_correspondence: joins the regions in the correspondence", {
  correspondence <- two_geographies()$correspondence
  joins <- tibble(TongfenID = c("a1", "a2"), TongfenID_joined = "a1")

  result <- tongfen_join_correspondence(correspondence, joins)
  expect_equal(names(result), names(correspondence))
  expect_equal(result$TongfenID, c("a1", "a1", "a3", "a3"))
  expect_equal(result$TongfenUID, c(rep("idA:a1,a2 idB:b1,b2", 2), rep("idA:a3,a4 idB:b3", 2)))
  expect_equal(result[3:4, ], correspondence[3:4, ])

  # joining a region made up of several regions
  result <- tongfen_join_correspondence(correspondence, tibble(TongfenID = c("a3", "a2"),
                                                                 TongfenID_joined = "a2"))
  expect_equal(result$TongfenID, c("a1", "a2", "a2", "a2"))
  expect_equal(result$TongfenUID, c("idA:a1 idB:b1", rep("idA:a2,a3,a4 idB:b2,b3", 3)))

  expect_equal(tongfen_join_correspondence(correspondence, joins[0, ]), correspondence)
  expect_warning(result <- tongfen_join_correspondence(correspondence,
                                                       bind_rows(joins, tibble(TongfenID = "x",
                                                                               TongfenID_joined = "a1"))),
                 "Did not find 1")
  expect_equal(result$TongfenID, c("a1", "a1", "a3", "a3"))
  expect_error(tongfen_join_correspondence(correspondence, tibble(TongfenID = "a1")),
               "TongfenID_joined")
})

test_that("tongfen_join_correspondence: tags the method of joined regions", {
  correspondence <- two_geographies()$correspondence %>%
    mutate(TongfenMethod = c("identifier", "identifier", "estimate", "estimate"))
  joins <- tibble(TongfenID = c("a1", "a2"), TongfenID_joined = "a1")

  result <- tongfen_join_correspondence(correspondence, joins)
  expect_equal(result$TongfenMethod,
               c("identifier, anomaly", "identifier, anomaly", "estimate", "estimate"))
  # joining again does not tag again
  joins <- tibble(TongfenID = c("a1", "a3"), TongfenID_joined = "a1")
  result <- tongfen_join_correspondence(result, joins)
  expect_equal(result$TongfenID, rep("a1", 4))
  expect_equal(result$TongfenMethod,
               c("identifier, anomaly", "identifier, anomaly", "estimate, anomaly", "estimate, anomaly"))
  # the summary of the correspondence still works
  expect_equal(nrow(check_tongfen_areas(two_geographies()$data, result)), 1L)
})

test_that("joining the aggregated data and aggregating with the joined correspondence agree", {
  fixture <- two_geographies()
  aggregated <- tongfen_aggregate(fixture$data, fixture$correspondence, fixture$meta, base_geo = "A")
  expect_equal(aggregated$TongfenID, c("a1", "a2", "a3"))

  # a1 loses 600 that show up in a2
  joins <- tongfen_anomaly_joins(aggregated, c("Population_A", "Population_B"),
                                 total_surprise_cutoff = 0.4)
  expect_equal(joins$TongfenID, c("a1", "a2"))

  joined <- tongfen_join_regions(aggregated, joins, fixture$meta)
  reaggregated <- tongfen_aggregate(fixture$data,
                                    tongfen_join_correspondence(fixture$correspondence, joins),
                                    fixture$meta, base_geo = "A")

  expect_equal(joined$TongfenID, c("a1", "a3"))
  expect_equal(joined$Population_B, c(1450, 820))
  expect_equal(st_drop_geometry(joined)[names(st_drop_geometry(reaggregated))],
               st_drop_geometry(reaggregated))
  expect_true(all(diag(st_equals(joined, reaggregated, sparse = FALSE))))
  expect_equal(as.character(st_geometry_type(joined)), as.character(st_geometry_type(reaggregated)))
})
