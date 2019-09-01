correspondence_urls <- list(
  "2006"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DA_AD_txt.zip"),
  "2011"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DA_AD_txt.zip"),
  "2016"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DB_ID_csv.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DA_AD_csv.zip")
)


#' Get StatCan DA or DB level correspondence file
#' @param year census year
#' @param level geographic level, DA or DB
#' @param refresh reload the correspondence files, default is `FALSE``
get_single_correspondence_for <- function(year,level=c("DA","DB"),refresh=FALSE) {
  year=as.character(year)
  if (!(level %in% c("DA","DB"))) stop("Level needs to be DA or DB")
  if (!(year %in% c("2006","2011","2016"))) stop("Year needds to be 2006, 2011 or 2016")
  new_field=paste0(level,"UID",year)
  old_field=paste0(level,"UID",as.integer(year)-5)
  path=file.path(getOption("custom_data_path"),paste0("statcan_correspondence_",year,"_",level,".csv"))
  if (refresh || !file.exists(path)) {
    url=correspondence_urls[[year]][[level]]
    tmp=tempfile()
    download.file(url,tmp)
    exdir=file.path(tempdir(),"correspondence")
    dir.create(exdir,showWarnings = FALSE)
    unzip(tmp,exdir=exdir)
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


get_tongfen_correspondence <- function(dd){
  hs <- names(dd)
  ddd<- dd %>%
    dplyr::mutate(TongfenID=!!as.name(hs[1]))


  num_grp_old=ddd$TongfenID %>% unique() %>% length()
  iterations <- 0
  repeat {
    for (n in hs) {
      ddd <- ddd %>%
        dplyr::group_by(!!as.name(n)) %>%
        dplyr::mutate(TongfenID=min(TongfenID))
    }
    num_grp_new=ddd$TongfenID %>% unique() %>% length()
    if(num_grp_new == num_grp_old) {break}
    num_grp_old=num_grp_new
    iterations=iterations+1
  }

  groups <- unique(ddd$TongfenID)
  grp_lookup <- setNames(seq(1,length(groups)),groups)

  ddd <- ddd %>%
    dplyr::group_by(TongfenID) %>%
    dplyr::mutate(TongfenUID=paste0(hs[1],":",paste0(sort(unique(!!as.name(hs[1]))),collapse=",")))
  for (n in hs[-1]) {
    ddd <- ddd %>%
      dplyr::mutate(TongfenUID=paste0(TongfenUID," ",n,":",paste0(sort(unique(!!as.name(n))),collapse=",")))
  }
  ddd
}

get_correspondence_for <- function(years,level,refresh=FALSE){
  #if (length(years)!=2) stop("Sorry, right now this only works for two years")
  years<-as.integer(years)
  all_years=seq(min(years),max(years),5)[-1]

  d <- get_single_correspondence_for(all_years[1],level,refresh) %>%
    dplyr::rename(!!paste0("flag",all_years[1]):=flag)
  all_years=all_years[-1]
  while (length(all_years)>0) {
    d <- dplyr::left_join(d,get_single_correspondence_for(all_years[1],level,refresh) %>%
                     dplyr::rename(!!paste0("flag",all_years[1]):=flag))
    all_years=all_years[-1]
  }
  dd<-d %>% dplyr::select_if(grepl(years %>% paste0(collapse = "|"),names(.))) %>%
    dplyr::select_if(!grepl("flag",names(.))) %>%
    unique

  ddd <- get_tongfen_correspondence(dd)


  ddd
}


#' Grab variables from several censuses on a common geography. Requires sf package to be avaialbe
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format `NA` to only get the variables or 'sf' to also get geographic data
#' @param na.rm logical, determines how NA values should be treated when aggregating variables
#' @param use_cache logical, passed to `cancensus::get_census` to regulate caching
#' @param census_data_transform optional transofrm function to be abllied to census data after being returned from cancensus
#' @export
get_tongfen_census_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,census_data_transform=function(id){id}) {
  meta <- meta_for_vectors(vectors)
  datasets <- meta$dataset %>% unique %>% sort
  years=datasets %>% gsub("CA","",.) %>% paste0("20",.) %>% as.integer

  correspondence <- get_correspondence_for(years,"DA")
  base <- c("Population","Dwellings","Households")

  data <- lapply(datasets,function(ds){
    if (ds==datasets[1]) gf=geo_format else gf=NA
    match_column <- ds %>% gsub("CA","",.) %>% paste0("DAUID20",.)
    get_census(dataset=ds,regions=regions,vectors=meta %>% dplyr::filter(dataset==ds) %>% dplyr::pull(variable),level="DA",geo_format=gf,labels="short",use_cache = use_cache) %>%
      census_data_transform %>%
      dplyr::left_join(correspondence %>% select(c(match_column,"TongfenID","TongfenUID")) %>% unique,by=c("GeoUID"=match_column)) %>%
      dplyr::group_by(TongfenID,TongfenUID) %>%
      aggregate_data_with_meta(.,dplyr::bind_rows(meta %>% filter(dataset==ds),tibble::tibble(variable=base)),geo=ds==datasets[1],na.rm=na.rm) %>%
      dplyr::rename_at(base,function(x){paste0(x,"_",ds)}) %>%
      ungroup
  }) %>% stats::setNames(datasets)

  ds=datasets[1]
  new_data <- data[[ds]]

  for (ds in datasets[-1]) {
    new_data <- new_data %>% left_join(data[[ds]],by = c("TongfenID","TongfenUID"))
  }

  new_data
}


#' Grab variables from several censuses on a common geography. Requires sf package to be avaialbe
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format `NA` to only get the variables or 'sf' to also get geographic data
#' @param use_cache logical, passed to `cancensus::get_census` to regulate caching
#' @param na.rm logical, determines how NA values should be treated when aggregating variables
#' @param census_data_transform optional transofrm function to be abllied to census data after being returned from cancensus
#' @export
get_tongfen_census_ct_from_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,census_data_transform=function(id){id}) {
  meta <- meta_for_vectors(vectors)
  datasets <- meta$dataset %>% unique %>% sort
  years=datasets %>% gsub("CA","",.) %>% paste0("20",.) %>% as.integer

  correspondence <- get_correspondence_for(years,"DA")
  for (ds in datasets) {
    da_column <- ds %>% gsub("CA","",.) %>% paste0("DAUID20",.)
    match_column=ds %>% gsub("CA","",.) %>% paste0("CTUID20",.)
    ct_link <-get_census(dataset=ds,regions=regions,level="DA",use_cache = use_cache) %>%
      dplyr::select(GeoUID,CT_UID) %>%
      dplyr::rename(!!match_column:=CT_UID,
                    !!da_column:=GeoUID)

    correspondence <- dplyr::inner_join(correspondence,ct_link,by=da_column) %>%
      dplyr::ungroup()
  }

  correspondence <- correspondence %>%
    dplyr::select(matches("CTUID")) %>%
    get_tongfen_correspondence()

  base <- c("Population","Dwellings","Households")

  data <- lapply(datasets,function(ds){
    if (ds==datasets[1]) gf=geo_format else gf=NA
    match_column=ds %>% gsub("CA","",.) %>% paste0("CTUID20",.)

    get_census(dataset=ds,regions=regions,vectors=meta %>% dplyr::filter(dataset==ds) %>% dplyr::pull(variable),level="CT",geo_format=gf,labels="short",use_cache = use_cache) %>%
      census_data_transform %>%
      dplyr::left_join(correspondence %>% select(c(match_column,"TongfenID","TongfenUID")) %>% unique,by=c("GeoUID"=match_column)) %>%
      dplyr::group_by(TongfenID,TongfenUID) %>%
      aggregate_data_with_meta(.,dplyr::bind_rows(meta %>% dplyr::filter(dataset==ds),tibble::tibble(variable=base)),geo=ds==datasets[1],na.rm=na.rm) %>%
      dplyr::rename_at(base,function(x){paste0(x,"_",ds)}) %>%
      dplyr::ungroup()
  }) %>% stats::setNames(datasets)

  ds=datasets[1]
  new_data <- data[[ds]]

  for (ds in datasets[-1]) {
    new_data <- new_data %>% dplyr::left_join(data[[ds]],by = c("TongfenID","TongfenUID"))
  }

  if (length(names(vectors))==length(vectors)) {
    new_data <- new_data %>% dplyr::rename(!!!vectors)
  }

  new_data
}




