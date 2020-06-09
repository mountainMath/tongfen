correspondence_ca_census_urls <- list(
  "2006"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DA_AD_txt.zip"),
  "2011"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DA_AD_txt.zip"),
  "2016"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DB_ID_csv.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DA_AD_csv.zip")
)

#' Get StatCan DA or DB level correspondence file
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
  setNames(rep("CA1996",1),paste0("TX",seq(2000,2000))),
  setNames(rep("CA01",5),paste0("TX",seq(2001,2005))),
  setNames(rep("CA06",6),paste0("TX",seq(2006,2011))),
  setNames(rep("CA11",4),paste0("TX",seq(2012,2015))),
  setNames(rep("CA16",5),paste0("TX",seq(2016,2020))),
  setNames(rep("CA16",21),paste0("CA",seq(2000,2020),"RMS"))
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


#' Build tibble with information on how to aggregate variables given vectors
#' Queries list_census_variables to obtain needed information and add in vectors needed for aggregation
#' @param vectors list of variables to query
#' @return tidy dataframe with metadata information for requested variables and additional variables
#' needed for tongfen operations
#' @export
meta_for_ca_census_vectors <- function(vectors){
  vectors <- as.character(vectors) ## strip names just in case
  meta <- tibble::tibble(variable=vectors,dataset=datasets_from_vectors(vectors)) %>%
    mutate(type="Original", aggregation="0",units=NA)
  datasets <- meta$dataset %>%
    unique %>%
    sort
  for (dataset in datasets){
    d <- cancensus::list_census_vectors(dataset) %>%
      filter(.data$vector %in% (filter(meta,.data$dataset==dataset)$variable)) %>%
      select(.data$vector,.data$aggregation,.data$units)
    aggregation_lookup <- setNames(d$aggregation,d$vector)
    units_lookup <- setNames(d$units %>% as.character,d$vector)
    meta <- meta %>%
      mutate(aggregation=ifelse(.data$variable %in% names(aggregation_lookup),aggregation_lookup[.data$variable],.data$aggregation),
             units=ifelse(.data$variable %in% names(units_lookup),units_lookup[.data$variable],.data$units))
  }
  get_vector <- function(g){
    g %>% strsplit(" ") %>% purrr::map(function(a){ifelse(length(a)==3,a[3],NA)}) %>% unlist
  }
  meta <- meta %>%
    mutate(rule=case_when(grepl("Average of",.data$aggregation) ~ "Average",
                          grepl("Median of",.data$aggregation) ~ "Median",
                          .data$aggregation=="Not additive" ~ "Not additive",
                          .data$aggregation=="Additive" ~ "Additive",
                          grepl("Average to",.data$aggregation) ~ "AverageTo",
                          TRUE ~ sub(" .+$","",.data$aggregation)),
           parent=get_vector(.data$aggregation))

  extras <- meta %>%
    select(variable=.data$parent,.data$dataset) %>%
    mutate(type="Extra",aggregation="Additive",rule="Additive") %>%
    filter(!is.na(.data$variable),!.data$variable %in% meta$variable)

  if (nrow(extras)>0) {
    meta <- meta %>% bind_rows(extras)
  }

  meta <- meta %>%
    mutate(geo_dataset=geo_dataset_from_dataset(.data$dataset),
           year=years_from_datasets(.data$dataset))
  meta
}

#' Get StatCan DA or DB level correspondence file
#' @param year census year
#' @param level geographic level, DA or DB
#' @param refresh reload the correspondence files, default is `FALSE`
#' @return tibble with correspondence table`
get_single_correspondence_ca_census_for <- function(year,level=c("DA","DB"),refresh=FALSE) {
  level=level[1]
  year=as.character(year)[1]
  if (!(level %in% c("DA","DB"))) stop("Level needs to be DA or DB")
  if (!(year %in% c("2006","2011","2016"))) stop("Year needds to be 2006, 2011 or 2016")
  new_field=paste0(level,"UID",year)
  old_field=paste0(level,"UID",as.integer(year)-5)
  path=file.path(tongfen_cache_dir(),paste0("statcan_correspondence_",year,"_",level,".csv"))
  if (refresh || !file.exists(path)) {
    url=correspondence_ca_census_urls[[year]][[level]]
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
get_tongfen_ca_census_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,census_data_transform=function(id){id},quiet=TRUE) {
  meta <- meta_for_ca_census_vectors(vectors)
  datasets <- meta$geo_dataset %>% unique() %>% sort()
  years <- meta$year %>% unique() %>% sort()

  correspondence <- get_correspondence_ca_census_for(years,"DA")
  base <- c("Population","Dwellings","Households")

  data <- lapply(datasets,function(ds){
    if (ds==datasets[1]) gf=geo_format else gf=NA
    match_column <- ds %>%  years_from_datasets() %>% paste0("DAUID",.)
    cancensus::get_census(dataset=ds,regions=regions,
               vectors=meta %>% filter(.data$geo_dataset==ds) %>% pull(.data$variable) %>% as.character(),
               level="DA",geo_format=gf,labels="short",use_cache = use_cache, quiet=quiet) %>%
      census_data_transform %>%
      inner_join_tongfen_correspondence(correspondence,match_column) %>%
      # left_join(correspondence %>%
      #                    select(c(match_column,"TongfenID","TongfenUID")) %>%
      #                    unique,
      #                  by=c("GeoUID"=match_column)) %>%
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
get_tongfen_ca_census_ct_from_da <- function(regions,vectors,geo_format=NA,use_cache=TRUE,na.rm=TRUE,census_data_transform=function(id){id},quiet=TRUE) {
  meta <- meta_for_ca_census_vectors(vectors)
  datasets <- meta$dataset %>% unique %>% sort
  geo_datasets <- meta$geo_dataset %>% unique %>% sort
  years <- meta$year %>% unique %>% sort

  correspondence <- get_correspondence_ca_census_for(years,"DA")
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
               vectors=filter(meta,.data$geo_dataset==ds)$variable %>% as.character(),
               level="CT",geo_format=gf,labels="short",use_cache = use_cache,quiet=quiet) %>%
      census_data_transform %>%
      inner_join_tongfen_correspondence(correspondence,match_column) %>%
      # left_join(correspondence %>%
      #                    select(c(match_column,"TongfenID","TongfenUID")) %>%
      #                    unique,
      #                  by=c("GeoUID"=match_column)) %>%
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


#' Aggregate variables to common CTs, returns data2 on new tiling matching data1 geography
#' @param data1 cancensus CT level datatset for year1 < year2 to serve as base for common geography
#' @param data2 cancensus CT level datatset for year2 to be aggregated to common geography
#' @param data2_sum_vars vector of variable names to by summed up when aggregating geographies
#' @param data2_group_vars optional vector of grouping variables
#' @param na.rm optional parameter to remove NA values when summing, default = `TRUE`
#' @export
tongfen_ca_census_ct <- function(data1,data2,data2_sum_vars,data2_group_vars=c(),na.rm=TRUE) {
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


#' Grab variables from several censuses on a common geography. Requires sf package to be available
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_base optional parameter with allowed values one of the geo_datasets to base the geographic data on,
#' @param match_common_GeoUID logical, if `TRUE` (default) assumes CTs with same GeoUID are identical
#' @param area_mismatch_cutoff cutoff to disregard tracts that can't be matched within threshold
#' @param tolerance tolerance in metres when matching changing CT geographies.
#' @param na.rm remove NA values when aggregating up values, default is `TRUE`
#' @param quiet suppress download progress output, default is `FALSE`
#' @param refresh optional character, refresh data cache for this call
#' for no geographic data, (default `NA`)
#' or `NULL` (the default) for not returning geographic data`
#' @return dataframe with census variables on common geography
#' @export
get_tongfen_ca_census_ct <- function(regions,vectors,geo_base=NULL,
                              match_common_GeoUID=TRUE, area_mismatch_cutoff=0.1,
                              tolerance = 500,
                              na.rm=FALSE,quiet=FALSE,refresh=FALSE) {
  labels="short"
  meta <- meta_for_ca_census_vectors(vectors %>% as.character())
  geo_datasets <- meta$geo_dataset %>% unique %>% sort
  base <- c("Population","Dwellings","Households")

  data <- lapply(geo_datasets,function(g_ds){
    cancensus::get_census(dataset=g_ds,regions=regions,
                          vectors=filter(meta,.data$geo_dataset==g_ds)$variable %>% as.character(),
                          level="CT",geo_format='sf',labels="short",quiet=quiet,use_cache = !refresh) %>%
      mutate(!!paste0("GeoUID",g_ds):=.data$GeoUID) %>%
      rename(!!!setNames(base,paste0(base,"_",g_ds)))

  }) %>%
    setNames(geo_datasets)

  data1 <- data[[1]]

  meta <- meta %>%
    bind_rows(lapply(geo_datasets,function(ds)
      tibble(variable=paste0(base,"_",ds),dataset=ds,
             aggregation="Additive",units="Number",rule="Additive",geo_dataset=ds,
             year=years_from_datasets(ds))) %>%
        bind_rows)

  # get common GeoUIDs through all datasets
  if (match_common_GeoUID) {
    common_ids <- data1$GeoUID
    for (ds in geo_datasets[-1]) {
      common_ids <- intersect(common_ids,data[[ds]]$GeoUID)
      # base correspondence data for common ids
      gu1 <- paste0("GeoUID",geo_datasets[1])
      base_corresondence <- geo_datasets[-1] %>%
        lapply(function(ds){
          gu2 <- paste0("GeoUID",ds)
          r <- tibble(!!as.name(gu1):=common_ids,!!as.name(gu2):=common_ids,TongfenID=common_ids) %>%
            mutate(TongfenUID=paste0(gu1,":",!!as.name(gu1)," ",gu2,":",!!as.name(gu2)))
          gu1 <<- gu2
          r
        })%>%
        setNames(paste0(geo_datasets[-length(geo_datasets)],"-",geo_datasets[-1]))
    }
  } else {
    common_ids=c()
  }


  # estimate coorrespondence data from tongfen
  g1 <- data1 %>% filter(!(.data$GeoUID %in% common_ids))
  gu1 <- paste0("GeoUID",geo_datasets[1])
  old_ds <- geo_datasets[1]
  mismatch_correspondences <- geo_datasets[-1] %>%
    lapply(function(ds){
      g2 <- data[[ds]] %>% filter(!(.data$GeoUID %in% common_ids))
      gu2 <- paste0("GeoUID",ds)
      c <- estimate_tongfen_correspondence(g1,g2,gu1,gu2,tolerance=tolerance,computation_crs=3347)
      if (!is.null(area_mismatch_cutoff)) {
        cc <- check_tongfen_areas(g1,g2,c) %>%
          mutate(ratio = log(as.numeric(.data$area1)/as.numeric(.data$area2)))
        ccc <- cc %>% filter(abs(.data$ratio)>area_mismatch_cutoff)
        if (nrow(ccc)>0) {
          if (!quiet) {
            message(paste0("Removing ",nrow(ccc)," regions due to area mismatch: ",old_ds,"-",ds))
            print(ccc)
          }
          c <- c %>% filter(!(.data$TongfenID %in% ccc$TongfenID))
        }
      }
      g1 <<- g2
      gu1 <<- gu2
      old_ds <<- ds
      c
    }) %>%
    setNames(paste0(geo_datasets[-length(geo_datasets)],"-",geo_datasets[-1]))


  if (match_common_GeoUID) {
    # merge base and tongfen correspondence data
    full_correspondence <- names(base_corresondence) %>%
      lapply(function(ds){
        bind_rows(base_corresondence[[ds]],mismatch_correspondences[[ds]])
      }) %>%
      setNames(names(base_corresondence))
  } else {
    full_correspondence <- mismatch_correspondences
  }

  # compute full correspondence
  correspondence <- full_correspondence[[1]] %>%
    select(matches("GeoUID"))
  for (ds in names(full_correspondence)[-1]) {
    c <- full_correspondence[[ds]] %>%
      select(matches("GeoUID"))
    correspondence <- inner_join(correspondence,c,by=intersect(names(correspondence),names(c)))
  }
  correspondence <- get_tongfen_correspondence(correspondence)

  # aggregate up data
  data_new <- geo_datasets %>%
    lapply(function(ds){
      data[[ds]] %>%
        inner_join(correspondence %>% select(c(paste0("GeoUID",ds),"TongfenID","TongfenUID")),
                   by=paste0("GeoUID",ds)) %>%
        group_by(.data$TongfenID,.data$TongfenUID) %>%
        aggregate_data_with_meta(meta %>% filter(.data$geo_dataset==ds))
    }) %>%
    setNames(geo_datasets)

  start_ds <-geo_base
  if (is.null(start_ds)) start_ds <- geo_datasets[1]
  aggregated_data <- data_new[[start_ds]]
  if (is.null(start_ds)) aggregated_data <- aggregated_data %>% st_set_geometry(NULL)
  for (ds in geo_datasets[geo_datasets!=start_ds]) {
    aggregated_data <- aggregated_data %>%
      inner_join(data_new[[ds]] %>%
                   st_set_geometry(NULL) %>%
                   select(-.data$TongfenUID),
                 by="TongfenID")
  }

  aggregated_data <- aggregated_data %>%
    select_at(names(aggregated_data) %>% setdiff(c(filter(meta,.data$type=="Extra")$variable)))
  if (!is.null(names(vectors))){
    v <- vectors[names(vectors)!=""]
    aggregated_data <- aggregated_data %>% rename(!!!v)
  }

  aggregated_data %>%
    ungroup()
}



#' @import dplyr
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


