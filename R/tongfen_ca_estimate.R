#' Tongfen estimate data for given geometry
#'
#' @description
#' \lifecycle{maturing}
#'
#' Estimates values for the given census vectors for the given geometry using
#' data from the specified level range
#'
#' @param geometry geometry
#' @param meta metadata for the census variables to aggregate, for example as returned by `meta_for_ca_census_vectors`.
#' At this point this function only accepts variables from the same census geography year. We will expand this to also
#' allow estimates across multiple census geography years, but this requires further attention to detail. It is
#' recommended to apply due caution when running this function separately across several census geography years with
#' the purpose of comparing data across time as a naive application can lead to systematic biases.
#' @param level level to use for tongfen
#' @param intersection_level level to use for geometry intersection, if different from tongfen level
#' by \code{meta_for_ca_census_vectors}. This can be set at a higher aggregation level to conserve API points
#' for the `get_intersecting_geometries` call.
#' @param downsample_level default `NULL`, can be a geographic level lower than `level`, in which case the data is downsamples
#' to that geography level proportionally using the value of the `downsample` column (must be supplied) in the `meta`
#' argument before intersecting the geometries. This can lead to more accurate results. At this point the only allowed
#' variables for the `downsample` column in `meta` are "Population", "Households" or "Dwellings", and it can only be
#' one of these for all variables.
#' @param na.rm how to deal with NA values, default is \code{FALSE}.
#' @param quiet suppress progress messages
#' @export
#'
#' @examples
#' # Estimate a common geography for 2006 and 2016 dissemination areas in the City of Vancouver
#' # based on the geographic data and check estimation errors
#' \dontrun{
#' toronto_city_hall <- sf::st_point(c(-79.3839,43.6534)) %>%
#'   sf::st_sfc(crs=4326) %>%
#'   sf::st_transform(3348) %>%
#'   sf::st_buffer(1000) %>%
#'   sf::st_sf()
#'
#' meta <- meta_for_additive_variables("CA16","Population")
#'
#' data <- tongfen_estimate_ca_census(toronto_city_hall,meta,level="DA",intersection_level="CT")
#'
#' print(paste0("Approximately ",scales::comma(data$Population,accuracy=100),
#'              " people live within a 1 km radius of Toronto City."))
#'
#'}
tongfen_estimate_ca_census <- function(geometry, meta, level,
                                       intersection_level = level,
                                       downsample_level = NULL,
                                       na.rm=FALSE,
                                       quiet=FALSE) {
  datasets <- meta$geo_dataset %>% unique()
  if (length(datasets)!=1) stop("At this point tongfen_estimate_ca_census can only handle data for a single census geography year")
  regions <- datasets %>%
    lapply(function(ds){
      cancensus::get_intersecting_geometries(dataset=ds, level=intersection_level, geometry=geometry, quiet - quiet)
    }) %>%
    lapply(as_tibble) %>%
    bind_rows() %>%
    unique %>%
    as.list()

  # This has issues, this is missing tongfen regions. Possibly a better approach would be to only
  # call get_intersecting_geometries on one of the datasets and then use the statcan correspondence
  # files to determine the full extent of the geometries needed for all levels based on that.

  # So maybe a function like get_tongfen_correspondence_from_seed
  census_data <- get_tongfen_ca_census(regions = regions, meta = meta,
                                       level = level, na.rm = na.rm, quiet = quiet) %>%
    sf::st_transform(st_crs(geometry))

  if (!is.null(downsample_level)){
    if (!("downsample" %in% names(meta))) stop("The downsample column in meta needs to be set")
    base_vars <- meta %>% pull(.data$downsample) %>% stats::na.omit() %>% unique()
    if (length(base_vars)==0) stop("The downsample column in meta needs to be set")
    if (length(base_vars)!=1) stop(paste0("The downsample column has to have a unique variable, you provided ",paste0(base_vars, collapse = ", ")))
    dg <- cancensus::get_census(dataset=datasets,regions = regions,
                                level = downsample_level, quiet = quiet, geo_format = "sf") %>%
      sf::st_transform(st_crs(geometry))

    g <- proportional_reaggregate(dg, census_data,
                                  geo_match = setNames("GeoUID",paste0(level,"_UID")),
                                  categories=meta$label, base=base_vars)
    census_data <- g
  }

  result <- tongfen_estimate(target = geometry, source = census_data, meta = meta)
}


