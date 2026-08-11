fips_code_for_state <- function(s){
  tidycensus::fips_codes %>%
    filter(.data$state==s | .data$state_code==s) %>%
    select("state","state_code") %>%
    unique()
}

# census tract vintages the correspondence layer can bridge, in chronological order,
# together with the name of the GEOID column identifying tracts of that vintage
us_ct_geoid_columns <- c(dec1990 = "GEOID90",
                         dec2000 = "GEOID00",
                         dec2010 = "GEOID10",
                         dec2020 = "GEOID20")

# `year` is the later of the two censuses the relationship file links
get_us_ct_correspondence_path <- function(state,year){
  states <- fips_code_for_state(state)
  if (nrow(states)!= 1) {
    stop(paste0("Could not determine state: ",state))
  }
  if (year=="2000") {
    path <- paste0("https://www2.census.gov/geo/relfiles/tract/",
                   tolower(states$state),"/",
                   tolower(states$state),
                   states$state_code,"pop.txt")
  } else if (year=="2010") {
    path <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/",
                   tolower(states$state),
                   states$state_code,"trf.txt")
  } else if (year=="2020") {
    path <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel2020/t10t20/TAB2010_TAB2020_ST",
                   states$state_code,".zip")
  } else {
    stop(paste0("No census tract relationship file available for ",year))
  }
  path
}

get_us_ct_correspondence_2020 <- function(state,cache_path=getOption("tongfen.cache_path")) {
  states <- fips_code_for_state(state)
  cache_path = file.path(cache_path %||% tempdir(),"us_data")

  path <- get_us_ct_correspondence_path(state,2020)
  local_path <-  file.path(cache_path,basename(path))
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path)
    utils::download.file(path,local_path,quiet = TRUE)
  }
  readr::read_delim(local_path,delim="|", col_types = "cccccnncccnncnn") %>%
    mutate(GEOID10=paste0(.data$STATE_2010,.data$COUNTY_2010,.data$TRACT_2010),
           GEOID20=paste0(.data$STATE_2020,.data$COUNTY_2020,.data$TRACT_2020)) %>%
    select(.data$GEOID10,.data$GEOID20)%>%
    unique
}

get_us_ct_correspondence_2010 <- function(state,cache_path=getOption("tongfen.cache_path")){
  path <- get_us_ct_correspondence_path(state,"2010")
  file <- basename(path)
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  local_path <- file.path(cache_path,file)
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path)
    utils::download.file(path,local_path,quiet=TRUE)
  }
  d<-readr::read_csv(local_path,
                     col_names=c("STATE00","COUNTY00","TRACT00","GEOID00",
                                 "POP00","HU00","PART00","AREA00","AREALAND00",
                                 "STATE10","COUNTY10","TRACT10","GEOID10",
                                 "POP10","HU10","PART10","AREA10","AREALAND10",
                                 "AREAPT","AREALANDPT","AREAPCT00PT",
                                 "AREALANDPCT00PT","AREAPCT10PT","AREALANDPCT10PT",
                                 "POP10PT","POPPCT00","POPPCT10","HU10PT","HUPCT00","HUPCT10"),
                     col_types = "cccciiccccccciicccnnnnnnnnnnnn")
}

# the 1990 to 2000 relationship files are fixed width, the "pop" variant is the
# complete one, listing every tract rather than only the ones that changed
get_us_ct_correspondence_2000 <- function(state,cache_path=getOption("tongfen.cache_path")){
  path <- get_us_ct_correspondence_path(state,"2000")
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  local_path <- file.path(cache_path,basename(path))
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path)
    utils::download.file(path,local_path,quiet=TRUE)
  }
  readr::read_fwf(local_path,
                  readr::fwf_cols(STATE90=c(1,2),COUNTY90=c(3,5),TRACT90BASE=c(6,9),
                                  TRACT90SUF=c(10,11),PART90=c(12,12),POP90TRACT=c(13,21),
                                  PCT90=c(22,25),STATE00=c(26,27),COUNTY00=c(28,30),
                                  TRACT00BASE=c(31,34),TRACT00SUF=c(35,36),PART00=c(37,37),
                                  POP00TRACT=c(38,46),PCT00=c(47,50),POPPART=c(51,59),
                                  AREALAND=c(60,73),STAB=c(74,75),COUNTYNAME=c(76,135)),
                  col_types=readr::cols(.default="c")) %>%
    mutate(GEOID90=paste0(.data$STATE90,.data$COUNTY90,.data$TRACT90BASE,
                          coalesce(.data$TRACT90SUF,"00")),
           GEOID00=paste0(.data$STATE00,.data$COUNTY00,.data$TRACT00BASE,
                          coalesce(.data$TRACT00SUF,"00"))) %>%
    select("GEOID90","GEOID00") %>%
    unique()
}

# stitch relationship files for consecutive censuses into one table spanning all
# requested censuses, dropping the vintages that only served as stepping stones
join_us_ct_correspondence <- function(links, datasets){
  c <- links[[1]]
  for (l in links[-1]) c <- full_join(c,l,by=intersect(names(c),names(l)))
  c %>%
    select(all_of(unname(us_ct_geoid_columns[datasets]))) %>%
    unique()
}

get_us_ct_correspondence <- function(state, datasets,
                                     cache_path=getOption("tongfen.cache_path")){
  years <- names(us_ct_geoid_columns)
  invalid_datasets <- setdiff(datasets,years)
  if (length(invalid_datasets) > 0) {
    stop(paste0("Invalid census years ",paste0(invalid_datasets,collapse=", "),
                ", can only match censuses ",paste0(years,collapse=", ")))
  }
  datasets <- intersect(years,datasets)
  if (length(datasets) < 2) {
    stop("Need at least two censuses to build a correspondence table.")
  }
  # censuses in between the requested ones still have to be traversed, there are no
  # relationship files skipping a census
  span <- years[seq(match(datasets[1],years),match(utils::tail(datasets,1),years))]
  links <- utils::head(span,-1) %>%
    lapply(function(year){
      link <- switch(year,
                     dec1990 = get_us_ct_correspondence_2000(state,cache_path=cache_path),
                     dec2000 = get_us_ct_correspondence_2010(state,cache_path=cache_path),
                     dec2010 = get_us_ct_correspondence_2020(state,cache_path=cache_path))
      link %>% select(matches("^GEOID\\d{2}$")) %>% unique()
    })
  join_us_ct_correspondence(links,datasets)
}

get_us_county_subdivision_correspondence <- function(cache_path=getOption("tongfen.cache_path")){
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  file <- "Cousub_comparability.xlsx"
  local_path <- file.path(cache_path,file)
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path)
    tmp=tempfile(fileext = ".zip")
    path="https://www2.census.gov/geo/docs/maps-data/data/comp/cousub_comparabilityxls.zip"
    utils::download.file(path,tmp,quiet=TRUE)
    utils::unzip(tmp,exdir = cache_path)
  }
  readxl::read_xlsx(local_path)
}


#' Get correspondence table for US census geographies
#'
#' @description
#' \lifecycle{maturing}
#'
#' Builds a correspondence table matching US census geographies across censuses, based on the
#' relationship files published by the US Census Bureau. Censuses that aren't requested but sit
#' in between two that are get traversed on the way, the Census Bureau only publishes
#' relationship files between consecutive censuses.
#'
#' The correspondence layer reaches back one census further than
#' \code{\link{get_tongfen_us_census}}. The 1990 census is available as `dec1990` here, but the
#' Census Bureau has retired the 1990 API endpoint, so 1990 data has to be brought in by other
#' means, for example from NHGIS via the ipumsr package, and handed to
#' \code{\link{tongfen_aggregate}} together with this correspondence table.
#'
#' @param datasets vector of censuses to match up, valid values are `dec1990`, `dec2000`,
#' `dec2010` and `dec2020` for census tracts, `dec2000` and `dec2010` for county subdivisions.
#' At least two censuses are needed.
#' @param regions list with regions to query the correspondence for. At this stage, the only
#' valid list is a vector of states, i.e. `regions = list(state=c("CA","OR"))`
#' @param level aggregation level, at this stage the only valid levels are 'tract' and
#' 'county subdivision'.
#' @param cache_path optional path to cache the relationship files in, defaults to the
#' `tongfen.cache_path` option and falls back to a temporary directory
#' @return tibble with one row per census geography, a GEOID column for each requested census,
#' and the common geography identified by `TongfenID` and `TongfenUID`.
#' @export
#'
#' @examples
#' # Match up census tracts for the 1990 and 2000 censuses in Rhode Island
#' \dontrun{
#' correspondence <- get_tongfen_correspondence_us_census(datasets = c("dec1990","dec2000"),
#'                                                        regions = list(state="RI"))
#'}
get_tongfen_correspondence_us_census <- function(datasets, regions, level='tract',
                                                 cache_path=getOption("tongfen.cache_path")){
  assert(level %in% c('tract','county subdivision'),
         "Only census tracts and county subdivisions are supported right now.")
  if (level=="county subdivision") {
    invalid_datasets <- setdiff(datasets,c("dec2000","dec2010"))
    assert(length(invalid_datasets)==0,
           paste0("County subdivisions can only be matched between the 2000 and 2010 censuses, got: ",
                  paste0(invalid_datasets,collapse=", ")))
  }

  regions$state %>%
    lapply(function(state){
      if (level=='tract') {
        get_us_ct_correspondence(state,datasets,cache_path=cache_path)
      } else {
        fips <- fips_code_for_state(state)$state_code
        get_us_county_subdivision_correspondence(cache_path=cache_path) %>%
          filter(.data$STATEFP10==fips) %>%
          select("GEOID00","GEOID10")
      }
    }) %>%
    bind_rows() %>%
    get_tongfen_correspondence()
}


valid_us_census_datasets <- c(
  dec2000 = "US decentennial census 2000",
  dec2010 = "US decentennial census 2010",
  dec2020 = "US decentennial census 2020"
)

#' Get US census data for 2000 and 2010 census on common census tract based geography
#'
#' @description
#' \lifecycle{maturing}
#'
#' This wraps data acquisition via the tidycensus package and tongfen on a common geography into
#' a single convenience function.
#'
#' Data is only available for the 2000, 2010 and 2020 censuses, the Census Bureau has retired the
#' 1990 API endpoint. To tongfen 1990 data, obtain it elsewhere and combine it with a
#' correspondence table from \code{\link{get_tongfen_correspondence_us_census}} via
#' \code{\link{tongfen_aggregate}}.
#'
#' @param regions list with regions to query the data for. At this stage, the only
#' valid list is a vector of states, i.e. `regions = list(state=c("CA","OR"))``
#' @param meta metadata for variables to retrieve
#' @param level aggregation level to return the data on. At this stage, the only valid levels are 'tract' and 'county subdivision'.
#' @param survey survey to get data for, supported options is "census"
#' @param base_geo census year to use as base geography, default is `2010`.
#' @return sf object with (wide form) census variables with census year as suffix (separated by underdcore "_").
#' @export
#'
#' @examples
#' # Get US census data on population and households for 2000 and 2010 censuses on a uniform geography
#' # based on census tracts.
#' \dontrun{
#' variables=c(population="H011001",households="H013001")
#'
#' meta <- c(2000,2010) %>%
#'   lapply(function(year){
#'     v <- variables %>% setNames(paste0(names(.),"_",year))
#'     meta_for_additive_variables(paste0("dec",year),v)
#'   }) %>%
#'   bind_rows()
#' census_data <- get_tongfen_us_census(regions = list(state="CA"), meta=meta, level="tract") %>%
#'   mutate(change=population_2010/households_2010-population_2000/households_2000)
#'
#'}
get_tongfen_us_census <- function(regions,meta,level='tract',survey="census",
                                  base_geo = NULL){

  datasets <- meta$dataset %>% unique
  if (is.null(base_geo)) base_geo=datasets[1]
  assert(base_geo %in% datasets,paste0("base_geo has to be one of the datasets ",paste0(datasets,collapse=", ")))
  invalid_datasets <- setdiff(datasets,names(valid_us_census_datasets))
  assert(length(invalid_datasets)==0, paste0("Invalid datasets :",paste0(invalid_datasets,collapse = ", ")))
  assert(level %in% c('tract','county subdivision'),"Only census tracts and counties are supported right now.")
  assert(survey %in% c('census'),"Only census surveys are supported right now.")

  regions$state %>% lapply(function(state){
    correspondence <- get_tongfen_correspondence_us_census(datasets = datasets,
                                                           regions = list(state=state),
                                                           level = level)

    data <- datasets %>%
      lapply(function(ds){
        m <- meta %>% filter(.data$dataset==ds)
        year=as.numeric(gsub("dec", "", ds))
        short_year <- substr(as.character(year),3,4)
        tidycensus::get_decennial(geography=level, state=state, county=regions$county,
                                  variables = m$variable, year = year,
                                  geometry = base_geo==ds, output="wide") %>%
          rename(!!paste0("GEOID",short_year):=.data$GEOID)
      }) %>%
      setNames(datasets)

    tongfen_aggregate(data,correspondence,meta,base_geo = base_geo)
  }) %>%
    bind_rows()
}
