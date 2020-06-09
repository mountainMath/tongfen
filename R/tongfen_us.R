get_us_ct_correspondence_path <- function(state,year){
  if (year=="2010") {
    states <- tidycensus::fips_codes %>%
      select(.data$state,.data$state_code) %>%
      unique %>%
      filter(toupper(!!state)==.data$state | !!state==.data$state_code)
    if (nrow(states)!= 1) {
      stop(paste0("Could not determine state: ",state))
    }
    path <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/",
                   tolower(states$state),
                   states$state_code,"trf.txt")
  }
}


get_us_ct_correspondence <- function(state,cache_path=getOption("tongfen.cache_path")){
  path <- get_us_ct_correspondence_path(state,"2010")
  file <- basename(path)
  cache_path = file.path(cache_path %||% tempdir(),"us_data")
  local_path <- file.path(cache_path,file)
  if (!file.exists(local_path)) {
    if (!dir.exists(cache_path)) dir.create(cache_path)
    utils::download.file(path,local_path)
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


#' Get US census data for 2000 and 2010 census on common census tract based geography
#' @param state US state to get the data for
#' @param variables vector with census variable names, possibly named
#' @return sf object with (wide form) census variables with census year as suffix (separated by underdcore "_").
#' @export
get_tongfen_us_census_ct <- function(state,variables){
  correspondence <- get_us_ct_correspondence(state) %>%
    select(.data$GEOID00,.data$GEOID10) %>%
    get_tongfen_correspondence()

  d <- tidycensus::get_decennial("tract",state=state,variables=c("H001001"),geometry=TRUE,year = 2010) %>%
    left_join(correspondence,by=c("GEOID"="GEOID10")) %>%
    group_by(.data$TongfenID,.data$TongfenUID) %>%
    summarize()
  d1 <- tidycensus::get_decennial("tract",state=state,variables,year = 2010) %>%
    left_join(correspondence,by=c("GEOID"="GEOID10")) %>%
    group_by(.data$TongfenID,.data$variable) %>%
    summarize(value=sum(.data$value))
  d2 <- tidycensus::get_decennial("tract",state=state,variables,year = 2000) %>%
    left_join(correspondence,by=c("GEOID"="GEOID00")) %>%
    group_by(.data$TongfenID,.data$variable) %>%
    summarize(value=sum(.data$value))

  # dd <- bind_rows(d1 %>% mutate(Year="2010"),
  #                 d2 %>% mutate(Year="2000"))

  recode_variables <- set_names(names(variables),as.character(variables))
  if (length(names(variables))==length(variables)) {
    d1 <- d1 %>% mutate(variable=recode(.data$variable,!!!recode_variables))
    d2 <- d2 %>% mutate(variable=recode(.data$variable,!!!recode_variables))
  }

  ddd <- d1 %>%
    mutate(variable=paste0(.data$variable,"_","2010")) %>%
    tidyr::pivot_wider(names_from = "variable",values_from = "value") %>%
    full_join(d2 %>%
                mutate(variable=paste0(.data$variable,"_","2000")) %>%
                tidyr::pivot_wider(names_from = "variable",values_from = "value"),by="TongfenID")

  d %>% left_join(ddd, by="TongfenID") %>% ungroup()
}
