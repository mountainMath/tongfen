#' Canadian census CT level tongfen
#'
#' @description
#' \lifecycle{deprecated}
#'
#' Grab variables from several censuses on a common geography. Requires sf package to be available
#' Will return CT level data
#'
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format geographic format for returned data, 'sf' for sf format and `NA``
#' @param na.rm remove NA values when aggregating up values, default is `TRUE`
#' @param quiet suppress download progress output, default is `FALSE`
#' @param refresh optional character, refresh data cache for this call
#' @return dataframe with census variables on common geography
#' @export
get_tongfen_census_ct <- function(regions,
                                  vectors,
                                  geo_format = NA,
                                  na.rm = TRUE,
                                  quiet = TRUE,
                                  refresh = FALSE){
  lifecycle::deprecate_warn("0.2.0", "get_tongfen_census_ct()", "get_tongfen_ca_census()")
  #warning("This method is deprecated, use `get_tongfen_ca_census(regions,vectors,level='CT', method = 'identifier', tolerance = 500, ...)` instead")
  meta <- meta_for_ca_census_vectors(vectors)
  base_geo <- ifelse(is.na(geo_format),NULL,meta$geo_dataset %>% unique %>% sort %>% first )

  get_tongfen_ca_census(regions = regions,
                        vectors = vectors,
                        level = 'CT',
                        method = 'identifier',
                        tolerance = 500,
                        base_geo = base_geo,
                        na.rm = na.rm,
                        quiet = quiet,
                        refresh = refresh)
}

#' Canadian Census DA level tongfen
#'
#' @description
#' \lifecycle{deprecated}
#'
#' Grab variables from several censuses on a common geography. Requires sf package to be available
#' Will return CT level data
#'
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format `NA` to only get the variables or 'sf' to also get geographic data
#' @param na.rm logical, determines how NA values should be treated when aggregating variables
#' @param use_cache logical, passed to `cancensus::get_census` to regulate caching
#' @param quiet suppress download progress output, default is `TRUE`
#' @return dataframe with variables on common geography
#' @export
get_tongfen_census_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,quiet=TRUE) {
  lifecycle::deprecate_warn("0.2.0", "get_tongfen_census_da()", "get_tongfen_ca_census()")
  meta <- meta_for_ca_census_vectors(vectors)
  base_geo <- ifelse(is.na(geo_format),NULL,meta$geo_dataset %>% unique %>% sort %>% first )

  get_tongfen_ca_census(regions = regions,
                        vectors = vectors,
                        level = 'DA',
                        method = 'statcan',
                        base_geo = base_geo,
                        na.rm = na.rm,
                        quiet = quiet,
                        refresh = !use_cache)
}


#' Canadian census CT level tongfen via DA correspondence
#'
#' @description
#' \lifecycle{deprecated}
#'
#' Grab variables from several censuses on a common geography. Requires sf package to be available
#' Will return CT level data
#'
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format `NA` to only get the variables or 'sf' to also get geographic data
#' @param use_cache logical, passed to `cancensus::get_census` to regulate caching
#' @param quiet suppress download progress output, default is `TRUE`
#' @param na.rm logical, determines how NA values should be treated when aggregating variables
#' @return dataframe with variables on common geography
#' @export
get_tongfen_ca_census_ct_from_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,quiet=TRUE) {
  lifecycle::deprecate_warn("0.2.0", "get_tongfen_census_da()", "get_tongfen_census_ca()")
  meta <- meta_for_ca_census_vectors(vectors)
  base_geo <- ifelse(is.na(geo_format),NULL,meta$geo_dataset %>% unique %>% sort %>% first )

  get_tongfen_ca_census(regions = regions,
                        vectors = vectors,
                        level = 'CT',
                        method = 'statcan',
                        base_geo = base_geo,
                        na.rm = na.rm,
                        quiet = quiet,
                        refresh = !use_cache)
}


#' Canadian census CT level tongfen via identifier matching
#'
#' @description
#' \lifecycle{deprecated}
#'
#' Aggregate variables to common CTs, returns data2 on new tiling matching data1 geography
#' @param data1 cancensus CT level datatset for year1 < year2 to serve as base for common geography
#' @param data2 cancensus CT level datatset for year2 to be aggregated to common geography
#' @param data2_sum_vars vector of variable names to by summed up when aggregating geographies
#' @param data2_group_vars optional vector of grouping variables
#' @param na.rm optional parameter to remove NA values when summing, default = `TRUE`
#' @export
tongfen_ca_census_ct <- function(data1,data2,data2_sum_vars,data2_group_vars=c(),na.rm=TRUE) {
  lifecycle::deprecate_warn("0.2.0", "tongfen_ca_census_ct()", "tongfen_aggregate()")
  cts_1 <- data1$GeoUID
  cts_2 <- data2$GeoUID
  cts_diff_1 <- setdiff(cts_1,cts_2) %>% sort
  cts_diff_2 <- setdiff(cts_2,cts_1) %>% sort

  d<-st_intersection(
    data2 %>% filter(.data$GeoUID %in% cts_diff_2) %>%
      rename(GeoUID2=.data$GeoUID) %>%
      select(.data$GeoUID2) %>% mutate(area2=st_area(.data$geometry)),
    data1 %>% filter(.data$GeoUID %in% cts_diff_1) %>%
      select(.data$GeoUID) %>% mutate(area=st_area(.data$geometry))
  )

  d <- d %>% mutate(area3=st_area(.data$geometry)) %>%
    mutate(ratio=as.numeric(.data$area3/.data$area2)) %>%
    filter(.data$ratio>0.1) %>%
    arrange(.data$ratio)

  dd<- d %>%
    sf::st_set_geometry(NULL) %>%
    group_by(.data$GeoUID) %>%
    summarize(ratio=sum(.data$ratio)/n(),n=n())

  if(dd %>% filter(.data$n<=1) %>% nrow >0) {base::stop("problem with computing common ct data")}

  ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2 %>% unique)
  ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID %>% unique)

  new2 <- data2 %>%
    filter(.data$GeoUID %in% cts_diff_2) %>%
    mutate(GeoUID2=.data$GeoUID) %>%
    mutate(GeoUID=as.character(ct_translation2[.data$GeoUID2])) %>%
    group_by(!!!c("GeoUID",data2_group_vars) %>% purrr::map(as.name) %>% unlist)

  nnew <- new2 %>% summarize_at(data2_sum_vars,function(x)sum(x,na.rm=na.rm))

  data_2 <- rbind(data2 %>% filter(!(.data$GeoUID %in% cts_diff_2)) %>%
                    select("GeoUID",data2_group_vars,data2_sum_vars), nnew)
  return(data_2)
}

#' Get StatCan DA or DB level correspondence file
#'
#' @description
#' \lifecycle(deprecated)
#' Joins the StatCan correspodence files for several census years
#'
#' @param years list of census years
#' @param level geographic level, DA or DB
#' @param refresh reload the correspondence files, default is `FALSE`
#' @return tibble with correspondence table`spanning all years
get_correspondence_ca_census_for <- function(years,level,refresh=FALSE){
  #if (length(years)!=2) stop("Sorry, right now this only works for two years")
  years<-as.integer(years)
  all_years=seq(min(years),max(years),5)[-1]

  d <- get_single_correspondence_ca_census_for(all_years[1],level,refresh) %>%
    rename(!!paste0("flag",all_years[1]):=.data$flag)
  all_years=all_years[-1]
  while (length(all_years)>0) {
    d <- left_join(d,get_single_correspondence_ca_census_for(all_years[1],level,refresh) %>%
                     rename(!!paste0("flag",all_years[1]):=.data$flag))
    all_years=all_years[-1]
  }
  dd<-d %>% select_if(grepl(years %>% paste0(collapse = "|"),names(.))) %>%
    select_if(!grepl("flag",names(.))) %>%
    unique

  ddd <- get_tongfen_correspondence(dd)


  ddd
}

