fips_code_for_state <- function(s){
  require_suggested("tidycensus")
  tidycensus::fips_codes %>%
    filter(.data$state==s | .data$state_code==s) %>%
    select("state","state_code") %>%
    unique()
}

# census vintages the correspondence layer can bridge, in chronological order, together
# with the name of the GEOID column identifying geographies of that vintage
us_geoid_columns <- c(dec1990 = "GEOID90",
                      dec2000 = "GEOID00",
                      dec2010 = "GEOID10",
                      dec2020 = "GEOID20")

# Relationship files list every geometric overlap between two censuses, including slivers
# along boundaries that only shifted slightly. Chaining those merges unrelated regions into
# one common geography, so they get cut. The share is taken over the larger of the two sides,
# a region carved out of a bigger one is only a small share of the old one but most of the
# new one. Regions never get dropped: if all parts of a region are slivers its largest part
# is kept, so every region still ends up in some common geography.
area_share <- function(part, total) ifelse(total > 0, part/total, 0)

cut_correspondence_slivers <- function(d, share, min_area_share){
  keep <- share >= min_area_share
  for (column in names(d)) {
    g <- d[[column]]
    largest <- share == unname(tapply(share, g, max)[g])
    keep <- keep | (!(g %in% g[keep]) & largest)
  }
  d[keep,,drop=FALSE] %>% unique()
}

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

# the 2010 to 2020 tract relationship file is block based, tract areas get summed up from
# the blocks they are made up of
get_us_ct_correspondence_2020 <- function(state,min_area_share=0.01,
                                          cache_path=getOption("tongfen.cache_path")) {
  cache_path = file.path(cache_path %||% tempdir(),"us_data")

  path <- get_us_ct_correspondence_path(state,2020)
  local_path <-  file.path(cache_path,basename(path))
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    utils::download.file(path,local_path,quiet = TRUE)
  }
  blocks <- readr::read_delim(local_path,delim="|",progress=FALSE,
                              col_types=readr::cols_only(
                                STATE_2010="c",COUNTY_2010="c",TRACT_2010="c",BLK_2010="c",
                                AREALAND_2010="d",AREAWATER_2010="d",
                                STATE_2020="c",COUNTY_2020="c",TRACT_2020="c",BLK_2020="c",
                                AREALAND_2020="d",AREAWATER_2020="d",
                                AREALAND_INT="d",AREAWATER_INT="d")) %>%
    mutate(GEOID10=paste0(.data$STATE_2010,.data$COUNTY_2010,.data$TRACT_2010),
           GEOID20=paste0(.data$STATE_2020,.data$COUNTY_2020,.data$TRACT_2020),
           area10=.data$AREALAND_2010+.data$AREAWATER_2010,
           area20=.data$AREALAND_2020+.data$AREAWATER_2020,
           area_part=.data$AREALAND_INT+.data$AREAWATER_INT)
  tracts10 <- blocks %>% select("GEOID10","BLK_2010","area10") %>% unique() %>%
    group_by(.data$GEOID10) %>% summarize(area10=sum(.data$area10),.groups="drop")
  tracts20 <- blocks %>% select("GEOID20","BLK_2020","area20") %>% unique() %>%
    group_by(.data$GEOID20) %>% summarize(area20=sum(.data$area20),.groups="drop")
  d <- blocks %>%
    group_by(.data$GEOID10,.data$GEOID20) %>%
    summarize(area_part=sum(.data$area_part),.groups="drop") %>%
    left_join(tracts10,by="GEOID10") %>%
    left_join(tracts20,by="GEOID20")
  cut_correspondence_slivers(d %>% select("GEOID10","GEOID20"),
                             pmax(area_share(d$area_part,d$area10),
                                  area_share(d$area_part,d$area20)),
                             min_area_share)
}

get_us_ct_correspondence_2010 <- function(state,min_area_share=0.01,
                                          cache_path=getOption("tongfen.cache_path")){
  path <- get_us_ct_correspondence_path(state,"2010")
  file <- basename(path)
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  local_path <- file.path(cache_path,file)
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    utils::download.file(path,local_path,quiet=TRUE)
  }
  d<-readr::read_csv(local_path,progress=FALSE,
                     col_names=c("STATE00","COUNTY00","TRACT00","GEOID00",
                                 "POP00","HU00","PART00","AREA00","AREALAND00",
                                 "STATE10","COUNTY10","TRACT10","GEOID10",
                                 "POP10","HU10","PART10","AREA10","AREALAND10",
                                 "AREAPT","AREALANDPT","AREAPCT00PT",
                                 "AREALANDPCT00PT","AREAPCT10PT","AREALANDPCT10PT",
                                 "POP10PT","POPPCT00","POPPCT10","HU10PT","HUPCT00","HUPCT10"),
                     col_types = "cccciiccccccciicccnnnnnnnnnnnn") %>%
    group_by(.data$GEOID00,.data$GEOID10) %>%
    summarize(area_part=sum(.data$AREAPT),
              area00=max(as.numeric(.data$AREA00)),
              area10=max(as.numeric(.data$AREA10)),
              .groups="drop")
  cut_correspondence_slivers(d %>% select("GEOID00","GEOID10"),
                             pmax(area_share(d$area_part,d$area00),
                                  area_share(d$area_part,d$area10)),
                             min_area_share)
}

# the 1990 to 2000 relationship files are fixed width, the "pop" variant is the complete one,
# listing every tract rather than only the ones that changed. It only carries the land area of
# each part, tract areas get summed up from those
get_us_ct_correspondence_2000 <- function(state,min_area_share=0.01,
                                          cache_path=getOption("tongfen.cache_path")){
  path <- get_us_ct_correspondence_path(state,"2000")
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  local_path <- file.path(cache_path,basename(path))
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    utils::download.file(path,local_path,quiet=TRUE)
  }
  d <- readr::read_fwf(local_path,
                  readr::fwf_cols(STATE90=c(1,2),COUNTY90=c(3,5),TRACT90BASE=c(6,9),
                                  TRACT90SUF=c(10,11),PART90=c(12,12),POP90TRACT=c(13,21),
                                  PCT90=c(22,25),STATE00=c(26,27),COUNTY00=c(28,30),
                                  TRACT00BASE=c(31,34),TRACT00SUF=c(35,36),PART00=c(37,37),
                                  POP00TRACT=c(38,46),PCT00=c(47,50),POPPART=c(51,59),
                                  AREALAND=c(60,73),STAB=c(74,75),COUNTYNAME=c(76,135)),
                  col_types=readr::cols(.default="c"),progress=FALSE) %>%
    mutate(GEOID90=paste0(.data$STATE90,.data$COUNTY90,.data$TRACT90BASE,
                          coalesce(.data$TRACT90SUF,"00")),
           GEOID00=paste0(.data$STATE00,.data$COUNTY00,.data$TRACT00BASE,
                          coalesce(.data$TRACT00SUF,"00"))) %>%
    group_by(.data$GEOID90,.data$GEOID00) %>%
    summarize(area_part=sum(as.numeric(.data$AREALAND)),.groups="drop") %>%
    group_by(.data$GEOID90) %>% mutate(area90=sum(.data$area_part)) %>%
    group_by(.data$GEOID00) %>% mutate(area00=sum(.data$area_part)) %>%
    ungroup()
  cut_correspondence_slivers(d %>% select("GEOID90","GEOID00"),
                             pmax(area_share(d$area_part,d$area90),
                                  area_share(d$area_part,d$area00)),
                             min_area_share)
}

# stitch relationship files for consecutive censuses into one table spanning all
# requested censuses, dropping the vintages that only served as stepping stones
join_us_correspondence <- function(links, datasets){
  c <- links[[1]]
  for (l in links[-1]) c <- full_join(c,l,by=intersect(names(c),names(l)))
  c %>%
    select(all_of(unname(us_geoid_columns[datasets]))) %>%
    unique()
}

# the censuses that have to be traversed to get from the earliest to the latest requested
# one, the Census Bureau only publishes relationship files between consecutive censuses
us_correspondence_span <- function(datasets, available){
  invalid_datasets <- setdiff(datasets,available)
  if (length(invalid_datasets) > 0) {
    stop(paste0("Invalid census years ",paste0(invalid_datasets,collapse=", "),
                ", can only match censuses ",paste0(available,collapse=", ")))
  }
  datasets <- intersect(available,datasets)
  if (length(datasets) < 2) {
    stop("Need at least two censuses to build a correspondence table.")
  }
  available[seq(match(datasets[1],available),match(utils::tail(datasets,1),available))]
}

get_us_ct_correspondence <- function(state, datasets, min_area_share=0.01,
                                     cache_path=getOption("tongfen.cache_path")){
  available <- names(us_geoid_columns)
  span <- us_correspondence_span(datasets,available)
  links <- utils::head(span,-1) %>%
    lapply(function(year){
      f <- switch(year,
                  dec1990 = get_us_ct_correspondence_2000,
                  dec2000 = get_us_ct_correspondence_2010,
                  dec2010 = get_us_ct_correspondence_2020)
      f(state,min_area_share=min_area_share,cache_path=cache_path)
    })
  join_us_correspondence(links,intersect(available,datasets))
}

# the 2000 to 2010 county subdivision comparability file, covering all states
get_us_county_subdivision_correspondence <- function(cache_path=getOption("tongfen.cache_path")){
  require_suggested("readxl")
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  file <- "Cousub_comparability.xlsx"
  local_path <- file.path(cache_path,file)
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    tmp=tempfile(fileext = ".zip")
    path="https://www2.census.gov/geo/docs/maps-data/data/comp/cousub_comparabilityxls.zip"
    utils::download.file(path,tmp,quiet=TRUE)
    utils::unzip(tmp,exdir = cache_path)
  }
  readxl::read_xlsx(local_path)
}

# The 2010 to 2020 county subdivision relationship file, covering all states. Unlike the
# 2000 to 2010 comparability file this is a geometric overlay, most rows are slivers along
# boundaries that shifted slightly rather than actual relationships. Keeping them chains
# unrelated subdivisions into one common geography, so they get cut. A subdivision carved
# out of a larger one is only a small share of the old one but most of the new one, hence
# the share is taken over the larger of the two.
get_us_county_subdivision_correspondence_2020 <- function(min_area_share=0.01,
                                                          cache_path=getOption("tongfen.cache_path")){
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  path <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel2020/cousub/",
                 "tab20_cousub20_cousub10_natl.txt")
  local_path <- file.path(cache_path,basename(path))
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
    utils::download.file(path,local_path,quiet=TRUE)
  }
  d <- readr::read_delim(local_path,delim="|",progress=FALSE,
                    col_types=readr::cols_only(GEOID_COUSUB_10="c",GEOID_COUSUB_20="c",
                                               AREALAND_COUSUB_10="d",AREAWATER_COUSUB_10="d",
                                               AREALAND_COUSUB_20="d",AREAWATER_COUSUB_20="d",
                                               AREALAND_PART="d",AREAWATER_PART="d")) %>%
    group_by(GEOID10=.data$GEOID_COUSUB_10,GEOID20=.data$GEOID_COUSUB_20) %>%
    summarize(area_part=sum(.data$AREALAND_PART+.data$AREAWATER_PART),
              area10=max(.data$AREALAND_COUSUB_10+.data$AREAWATER_COUSUB_10),
              area20=max(.data$AREALAND_COUSUB_20+.data$AREAWATER_COUSUB_20),
              .groups="drop")
  cut_correspondence_slivers(d %>% select("GEOID10","GEOID20"),
                             pmax(area_share(d$area_part,d$area10),
                                  area_share(d$area_part,d$area20)),
                             min_area_share)
}

get_us_county_subdivision_correspondence_for <- function(state, datasets, min_area_share=0.01,
                                                         cache_path=getOption("tongfen.cache_path")){
  available <- setdiff(names(us_geoid_columns),"dec1990")
  span <- us_correspondence_span(datasets,available)
  fips <- fips_code_for_state(state)$state_code
  links <- utils::head(span,-1) %>%
    lapply(function(year){
      link <- switch(year,
                     dec2000 = get_us_county_subdivision_correspondence(cache_path=cache_path) %>%
                       select("GEOID00","GEOID10"),
                     dec2010 = get_us_county_subdivision_correspondence_2020(
                       min_area_share=min_area_share,cache_path=cache_path))
      # both files are national, county subdivisions don't cross state lines
      link %>% filter(substr(.data$GEOID10,1,2)==fips) %>% unique()
    })
  join_us_correspondence(links,intersect(available,datasets))
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
#' The relationship files are geometric overlays that list every sliver along boundaries that
#' only shifted slightly. Those get cut via `min_area_share`, keeping them would chain
#' unrelated regions into one common geography.
#'
#' The correspondence layer reaches back one census further than
#' \code{\link{get_tongfen_us_census}}. The 1990 census is available as `dec1990` here, but the
#' Census Bureau has retired the 1990 API endpoint, so 1990 data has to be brought in by other
#' means, for example from NHGIS via the ipumsr package, and handed to
#' \code{\link{tongfen_aggregate}} together with this correspondence table.
#'
#' @param datasets vector of censuses to match up, valid values are `dec1990`, `dec2000`,
#' `dec2010` and `dec2020` for census tracts, `dec2000` through `dec2020` for county
#' subdivisions. At least two censuses are needed.
#' @param regions list with regions to query the correspondence for. At this stage, the only
#' valid list is a vector of states, i.e. `regions = list(state=c("CA","OR"))`
#' @param level aggregation level, at this stage the only valid levels are 'tract' and
#' 'county subdivision'.
#' @param min_area_share minimum share of area two geographies have to have in common to count
#' as related, default is `0.01`. The Census Bureau relationship files list every geometric
#' overlap, lowering this pulls in slivers along boundaries that only shifted slightly and
#' chains unrelated regions into one common geography. Raising it gives finer common
#' geographies at the risk of separating regions that did change. No region is ever dropped,
#' if all of its parts are slivers its largest part is kept.
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
                                                 min_area_share=0.01,
                                                 cache_path=getOption("tongfen.cache_path")){
  assert(level %in% c('tract','county subdivision'),
         "Only census tracts and county subdivisions are supported right now.")

  regions$state %>%
    lapply(function(state){
      if (level=='tract') {
        get_us_ct_correspondence(state,datasets,min_area_share=min_area_share,
                                 cache_path=cache_path)
      } else {
        get_us_county_subdivision_correspondence_for(state,datasets,
                                                     min_area_share=min_area_share,
                                                     cache_path=cache_path)
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

# Censuses that published several summary files need to be told which one to read. tidycensus
# picks a default per census year, for 2020 that is the PL 94-171 redistricting file which only
# carries a handful of variables, most 2020 variables live in the DHC file. `sumfile` is either a
# single value for all censuses or a vector named by dataset, `NULL` leaves the choice to
# tidycensus.
sumfile_for_dataset <- function(sumfile, ds){
  if (is.null(sumfile)) return(NULL)
  if (is.null(names(sumfile))) return(unname(sumfile))
  if (!(ds %in% names(sumfile))) return(NULL)
  unname(sumfile[[ds]])
}

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
#' @param min_area_share minimum share of area two geographies have to have in common to count
#' as related, default is `0.01`, see \code{\link{get_tongfen_correspondence_us_census}}.
#' @param sumfile summary file to read the variables from, either a single value used for all
#' censuses or a vector named by dataset, for example `c(dec2010="sf1", dec2020="dhc")`. Default
#' is `NULL`, which leaves the choice to tidycensus. Note that tidycensus defaults the 2020
#' census to the PL 94-171 redistricting file, most 2020 variables need `sumfile="dhc"`.
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
                                  base_geo = NULL, min_area_share = 0.01, sumfile = NULL){
  require_suggested("tidycensus")

  datasets <- meta$dataset %>% unique
  if (is.null(base_geo)) base_geo=datasets[1]
  assert(base_geo %in% datasets,paste0("base_geo has to be one of the datasets ",paste0(datasets,collapse=", ")))
  invalid_datasets <- setdiff(datasets,names(valid_us_census_datasets))
  assert(length(invalid_datasets)==0, paste0("Invalid datasets :",paste0(invalid_datasets,collapse = ", ")))
  assert(level %in% c('tract','county subdivision'),"Only census tracts and counties are supported right now.")
  assert(survey %in% c('census'),"Only census surveys are supported right now.")
  if (!is.null(sumfile)) {
    if (is.null(names(sumfile))) {
      assert(length(sumfile)==1,
             "sumfile has to be a single value or a vector named by dataset")
    } else {
      invalid_sumfiles <- setdiff(names(sumfile),datasets)
      assert(length(invalid_sumfiles)==0,
             paste0("Invalid datasets in sumfile: ",paste0(invalid_sumfiles,collapse=", ")))
    }
  }

  regions$state %>% lapply(function(state){
    correspondence <- get_tongfen_correspondence_us_census(datasets = datasets,
                                                           regions = list(state=state),
                                                           level = level,
                                                           min_area_share = min_area_share)

    data <- datasets %>%
      lapply(function(ds){
        m <- meta %>% filter(.data$dataset==ds)
        year=as.numeric(gsub("dec", "", ds))
        short_year <- substr(as.character(year),3,4)
        tidycensus::get_decennial(geography=level, state=state, county=regions$county,
                                  variables = m$variable, year = year,
                                  sumfile = sumfile_for_dataset(sumfile,ds),
                                  geometry = base_geo==ds, output="wide") %>%
          rename(!!paste0("GEOID",short_year):="GEOID")
      }) %>%
      setNames(datasets)

    tongfen_aggregate(data,correspondence,meta,base_geo = base_geo)
  }) %>%
    bind_rows()
}
