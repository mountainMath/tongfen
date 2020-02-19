years_from_datasets <- function(ds) {
  ds %>%
    stringr::str_extract("\\d+") %>%
    stringr::str_pad(width=3,side="left",pad="0") %>%
    stringr::str_pad(width=4,side="left",pad="2") %>%
    as.integer()
}

datasets_from_vectors <- function(vs){
  vs %>%
    stringr::str_split("_") %>%
    purrr::map(function(v)v[[2]]) %>%
    unlist()
}

GEO_DATASET_LOOKUP <- c(
  "TX2000"="CA1996",
  "TX2001"="CA01",
  "TX2002"="CA01",
  "TX2003"="CA01",
  "TX2004"="CA01",
  "TX2005"="CA01",
  "TX2006"="CA06",
  "TX2006"="CA06",
  "TX2007"="CA06",
  "TX2008"="CA06",
  "TX2009"="CA06",
  "TX2010"="CA06",
  "TX2011"="CA11",
  "TX2012"="CA11",
  "TX2013"="CA11",
  "TX2014"="CA11",
  "TX2015"="CA11",
  "TX2016"="CA16",
  "TX2017"="CA16",
  "TX2018"="CA16",
  "TX2019"="CA16",
  "TX2020"="CA16"
)

geo_dataset_from_dataset <- function(ds){
  result <- tibble(dataset=ds,geo_dataset=GEO_DATASET_LOOKUP[ds]) %>%
    mutate(geo_dataset=ifelse(is.na(.data$geo_dataset),.data$dataset %>%
                                years_from_datasets() %>%
                                as.character() %>%
                                substr(3,4) %>%
                                paste0("CA",.),
                              .data$geo_dataset))
  result$geo_dataset
}

get_tongfen_correspondence <- function(dd){
  hs <- names(dd)
  ddd<- dd %>%
    mutate(TongfenID=!!as.name(hs[1]))


  num_grp_old=ddd$TongfenID %>% unique() %>% length()
  iterations <- 0
  repeat {
    for (n in hs) {
      ddd <- ddd %>%
        group_by(!!as.name(n)) %>%
        mutate(TongfenID=min(.data$TongfenID))
    }
    num_grp_new=ddd$TongfenID %>% unique() %>% length()
    if(num_grp_new == num_grp_old) {break}
    num_grp_old=num_grp_new
    iterations=iterations+1
  }

  groups <- unique(ddd$TongfenID)
  grp_lookup <- setNames(seq(1,length(groups)),groups)

  ddd <- ddd %>%
    group_by(.data$TongfenID) %>%
    mutate(TongfenUID=paste0(hs[1],":",paste0(sort(unique(!!as.name(hs[1]))),collapse=",")))
  for (n in hs[-1]) {
    ddd <- ddd %>%
      mutate(TongfenUID=paste0(.data$TongfenUID," ",n,":",paste0(sort(unique(!!as.name(n))),collapse=",")))
  }
  ddd
}

get_correspondence_for <- function(years,level,refresh=FALSE){
  #if (length(years)!=2) stop("Sorry, right now this only works for two years")
  years<-as.integer(years)
  all_years=seq(min(years),max(years),5)[-1]

  d <- get_single_correspondence_for(all_years[1],level,refresh) %>%
    rename(!!paste0("flag",all_years[1]):=.data$flag)
  all_years=all_years[-1]
  while (length(all_years)>0) {
    d <- left_join(d,get_single_correspondence_for(all_years[1],level,refresh) %>%
                            rename(!!paste0("flag",all_years[1]):=.data$flag))
    all_years=all_years[-1]
  }
  dd<-d %>% select_if(grepl(years %>% paste0(collapse = "|"),names(.))) %>%
    select_if(!grepl("flag",names(.))) %>%
    unique

  ddd <- get_tongfen_correspondence(dd)


  ddd
}

#' @import dplyr
#' @importFrom stats setNames
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

