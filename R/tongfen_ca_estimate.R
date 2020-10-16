#' Tongfen estimate data for given geometry
#'
#' @description
#' \lifecycle{experimental}
#'
#' Estimates values for the given census vectors for the given geometry using
#' data from the specified level range
#'
#' @param geometry geometry
#' @param meta metadata for the census veraiables to aggregate, for example as returned
#' @param level level to use for tongfen
#' @param intersection_level level to use for geometry intersection, if different from tongfen level
#' by \code{meta_for_ca_census_vectors}.
#' @param na.rm how to deal with NA values, default is \code{FALSE}.
#'
tongfen_estimate_ca_census <- function(geometry, meta, level,
                                       intersection_level = level,
                                       na.rm=FALSE) {
  datasets <- meta$geo_dataset %>% unique()
  regions <- datasets %>%
    lapply(function(ds){
      cancensus::get_intersecting_geometries(dataset=ds, level=intersection_level, geometry=geometry)
    }) %>%
    lapply(as_tibble) %>%
    bind_rows() %>%
    unique %>%
    as.list()

  # This has issues, this is missing tongen regions. Possibly a better approach would be to only
  # call get_intersecting_geometries on one of the datasets and then use the statcan correspondence
  # files to determine the full extent of the geometries needed for all levels based on that.

  # So maybe a function like get_tongfen_correspondence_from_seed
  census_data <- get_tongfen_ca_census(regions = regions, meta = meta,
                                       level = level, na.rm = na.rm)

  result <- tongfen_estimate(target = geometry, source = census_data, meta = meta)
}


