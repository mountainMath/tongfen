fips_code_for_state <- function(s){
  tidycensus::fips_codes %>%
    filter(.data$state==s | .data$state_code==s) %>%
    select(.data$state,.data$state_code) %>%
    unique()
}

get_us_ct_correspondence_path <- function(state,year){
  if (year=="2010") {
    states <- fips_code_for_state(state)
    if (nrow(states)!= 1) {
      stop(paste0("Could not determine state: ",state))
    }
    path <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/",
                   tolower(states$state),
                   states$state_code,"trf.txt")
  } else if (year=="2020") {
    states <- fips_code_for_state(state)
    if (nrow(states)!= 1) {
      stop(paste0("Could not determine state: ",state))
    }
    path <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel2020/t10t20/TAB2010_TAB2020_ST",
                   states$state_code,".zip")
  }
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

get_us_ct_correspondence <- function(state, datasets,
                                     cache_path=getOption("tongfen.cache_path")){
  c <- NULL
  if (setdiff(datasets,c("dec2000","dec2010","dec2020")) %>% length() > 0) {
    stop("Invalid census years, can only match censuses 2000 through 2020")
  }
  if ("dec2000" %in% datasets) {
    c<-get_us_ct_correspondence_2010(state,cache_path=cache_path) %>%
      select(matches("^GEOID\\d{2}$"))
    if ("dec2020" %in% datasets) {
      c2 <- get_us_ct_correspondence_2020(state,cache_path=cache_path) %>%
        select(matches("^GEOID\\d{2}$"))
      c <- full_join(c,c2,by="GEOID10")
    }
    if (!("dec2010" %in% datasets)) c <- c %>% select(-.data$GEOIOD10)
    c <- c %>% unique
  } else if ("dec2020" %in% datasets) {
    c<-get_us_ct_correspondence_2020(state,cache_path=cache_path) %>%
      select(matches("^GEOID\\d{2}$"))
  } else stop("Invalid census years, can only match censuses 2000 through 2020")
  c
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
    if (level=='tract') {
      correspondence <-  get_us_ct_correspondence(state,datasets)
    } else  if (level=="county subdivision") {
      fips <- fips_code_for_state(state)$state_code
      correspondence <-  get_us_county_subdivision_correspondence() %>%
        filter(.data$STATEFP10==fips) %>%
        select(.data$GEOID00,.data$GEOID10)
    } else {
      stop("Ooops, should have caught this earler.")
    }
    correspondence <- correspondence %>%
      get_tongfen_correspondence()

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
