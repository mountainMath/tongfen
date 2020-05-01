correspondence_urls <- list(
  "2006"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DA_AD_txt.zip"),
  "2011"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DA_AD_txt.zip"),
  "2016"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DB_ID_csv.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DA_AD_csv.zip")
)

tongfen_cache_dir <- function(){
  getOption("tongfen.cache_path") %||%
    Sys.getenv("tongfen.cache_path") %||%
    getOption("custom_data_path") %||%
    tempdir()
}

#' Get StatCan DA or DB level correspondence file
#' @param year census year
#' @param level geographic level, DA or DB
#' @param refresh reload the correspondence files, default is `FALSE`
#' @return tibble with correspondence table`
get_single_correspondence_for <- function(year,level=c("DA","DB"),refresh=FALSE) {
  level=level[1]
  year=as.character(year)[1]
  if (!(level %in% c("DA","DB"))) stop("Level needs to be DA or DB")
  if (!(year %in% c("2006","2011","2016"))) stop("Year needds to be 2006, 2011 or 2016")
  new_field=paste0(level,"UID",year)
  old_field=paste0(level,"UID",as.integer(year)-5)
  path=file.path(tongfen_cache_dir(),paste0("statcan_correspondence_",year,"_",level,".csv"))
  if (refresh || !file.exists(path)) {
    url=correspondence_urls[[year]][[level]]
    tmp=tempfile()
    utils::download.file(url,tmp)
    exdir=file.path(tempdir(),"correspondence")
    dir.create(exdir,showWarnings = FALSE)
    utils::unzip(tmp,exdir=exdir)
    file=dir(exdir,"\\.txt|\\.csv")
    if (level=="DB") headers=c(new_field,old_field,"flag") else headers=c(new_field,old_field,paste0("DBUID",year),"flag")
    d<-readr::read_csv(file.path(exdir,file),col_types = readr::cols(.default = "c"),col_names = headers) %>%
      select(c(new_field,old_field,"flag")) %>%
      unique()
    readr::write_csv(d,path)
    unlink(tmp)
    unlink(exdir,recursive = TRUE)
  }
  readr::read_csv(path,col_types = readr::cols(.default = "c"))
}




#' Grab variables from several censuses on a common geography. Requires sf package to be available
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format `NA` to only get the variables or 'sf' to also get geographic data
#' @param na.rm logical, determines how NA values should be treated when aggregating variables
#' @param use_cache logical, passed to `cancensus::get_census` to regulate caching
#' @param quiet suppress download progress output, default is `TRUE`
#' @param census_data_transform optional transform function to be applied to census data after being returned from cancensus
#' @return dataframe with variables on common geography
#' @export
get_tongfen_census_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,census_data_transform=function(id){id},quiet=TRUE) {
  meta <- meta_for_vectors(vectors,also_for_first=TRUE)
  datasets <- meta$geo_dataset %>% unique() %>% sort()
  years <- meta$year %>% unique() %>% sort()

  correspondence <- get_correspondence_for(years,"DA")
  base <- c("Population","Dwellings","Households")

  data <- lapply(datasets,function(ds){
    if (ds==datasets[1]) gf=geo_format else gf=NA
    match_column <- ds %>%  years_from_datasets() %>% paste0("DAUID",.)
    cancensus::get_census(dataset=ds,regions=regions,
               vectors=meta %>% filter(.data$geo_dataset==ds) %>% pull(.data$variable),
               level="DA",geo_format=gf,labels="short",use_cache = use_cache, quiet=quiet) %>%
      census_data_transform %>%
      left_join(correspondence %>%
                         select(c(match_column,"TongfenID","TongfenUID")) %>%
                         unique,
                       by=c("GeoUID"=match_column)) %>%
      group_by(.data$TongfenID,.data$TongfenUID) %>%
      aggregate_data_with_meta(.,bind_rows(meta %>% filter(.data$geo_dataset==ds),
                                                  tibble::tibble(variable=base)),
                               geo=ds==datasets[1],na.rm=na.rm) %>%
      rename_at(base,function(x){paste0(x,"_",ds)}) %>%
      ungroup
  }) %>% stats::setNames(datasets)

  ds=datasets[1]
  new_data <- data[[ds]]

  for (ds in datasets[-1]) {
    new_data <- new_data %>% left_join(data[[ds]],by = c("TongfenID","TongfenUID"))
  }

  if (length(names(vectors))==length(vectors)) {
    new_data <- new_data %>% rename(!!!vectors)
  }

  new_data
}


#' Grab variables from several censuses on a common geography. Requires sf package to be available
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format `NA` to only get the variables or 'sf' to also get geographic data
#' @param use_cache logical, passed to `cancensus::get_census` to regulate caching
#' @param quiet suppress download progress output, default is `TRUE`
#' @param na.rm logical, determines how NA values should be treated when aggregating variables
#' @param census_data_transform optional transform function to be applied to census data after being returned from cancensus
#' @return dataframe with variables on common geography
#' @export
get_tongfen_census_ct_from_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,census_data_transform=function(id){id},quiet=TRUE) {
  meta <- meta_for_vectors(vectors,also_for_first=TRUE)
  datasets <- meta$dataset %>% unique %>% sort
  geo_datasets <- meta$geo_dataset %>% unique %>% sort
  years <- meta$year %>% unique %>% sort

  correspondence <- get_correspondence_for(years,"DA")
  for (ds in geo_datasets) {
    da_column <- ds %>% years_from_datasets() %>% paste0("DAUID",.)
    match_column <- ds %>% years_from_datasets() %>% paste0("CTUID",.)
    ct_link <-cancensus::get_census(dataset=ds,regions=regions,level="DA",use_cache = use_cache,quiet=quiet) %>%
      select(.data$GeoUID,.data$CT_UID) %>%
      rename(!!match_column:=.data$CT_UID,
                    !!da_column:=.data$GeoUID)

    correspondence <- inner_join(correspondence,ct_link,by=da_column) %>%
      ungroup()
  }

  correspondence <- correspondence %>%
    select(matches("CTUID")) %>%
    get_tongfen_correspondence()

  base <- c("Population","Dwellings","Households")

  data <- lapply(geo_datasets,function(ds){
    if (ds==geo_datasets[1]) gf=geo_format else gf=NA
    match_column=ds %>% years_from_datasets() %>% paste0("CTUID",.)
    cancensus::get_census(dataset=ds,regions=regions,
               vectors=filter(meta,.data$geo_dataset==ds)$variable,
               level="CT",geo_format=gf,labels="short",use_cache = use_cache,quiet=quiet) %>%
      census_data_transform %>%
      left_join(correspondence %>%
                         select(c(match_column,"TongfenID","TongfenUID")) %>%
                         unique,
                       by=c("GeoUID"=match_column)) %>%
      group_by(.data$TongfenID,.data$TongfenUID) %>%
      aggregate_data_with_meta(.,bind_rows(filter(meta,.data$geo_dataset==ds),
                                               tibble::tibble(variable=base)),
                               geo=ds==geo_datasets[1],na.rm=na.rm) %>%
      rename_at(base,function(x){paste0(x,"_",ds)}) %>%
      ungroup()
  }) %>% stats::setNames(geo_datasets)

  ds=geo_datasets[1]
  new_data <- data[[ds]]

  for (ds in geo_datasets[-1]) {
    new_data <- new_data %>% left_join(data[[ds]],by = c("TongfenID","TongfenUID"))
  }

  if (length(names(vectors))==length(vectors)) {
    new_data <- new_data %>% rename(!!!vectors)
  }

  new_data
}


#' @import dplyr
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


