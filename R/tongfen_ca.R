correspondence_ca_census_urls <- list(
  "2006"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DA_AD_txt.zip"),
  "2011"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DB_ID_txt.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DA_AD_txt.zip"),
  "2016"=list("DB"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DB_ID_csv.zip",
              "DA"="http://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DA_AD_csv.zip")
)

ca_census_base <- c("Population","Dwellings","Households")


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

geo_dataset_for_years <- function(years){
  dataset_list <- cancensus::list_census_datasets()
  years %>%
    lapply(function(year){
      dataset_list %>%
        filter(.data$description==paste0(year," Canada Census")|.data$description==paste0(year," Canada Census and NHS")) %>%
        pull(.data$geo_dataset) %>%
        unique()
    }) %>%
    unlist()
}

geo_dataset_from_dataset <- function(datasets){
  if (TRUE) { # legacy until cancensus updates
  datasets <- datasets %>% gsub("^CA11[NF]$","CA11",.)
  dataset_list <- cancensus::list_census_datasets()
  lapply(datasets, function(ds){
    dataset_list %>%
      filter(.data$dataset == ds) %>%
      pull(.data$geo_dataset) %>%
      unique()
  }) %>%
    unlist()
  } else {
    result <- tibble(dataset=datasets,geo_dataset=GEO_DATASET_LOOKUP[datasets]) %>%
      mutate(geo_dataset=ifelse(is.na(.data$geo_dataset),.data$dataset %>%
                                  years_from_datasets() %>%
                                  as.character() %>%
                                  substr(3,4) %>%
                                  paste0("CA",.),
                                .data$geo_dataset))
    result$geo_dataset
  }
}

#' Generate metadata from Candian census vectors
#'
#' @description
#' \lifecycle{maturing}
#'
#' Build tibble with information on how to aggregate variables given vectors
#' Queries list_census_variables to obtain needed information and add in vectors needed for aggregation
#'
#' @param vectors list of variables to query
#' @return tidy dataframe with metadata information for requested variables and additional variables
#' needed for tongfen operations
#' @export
meta_for_ca_census_vectors <- function(vectors){
  nn <- names(vectors)
  vectors <- as.character(vectors) ## strip names just in case
  if (is.null(nn)) {
    nn <- vectors
  } else {
    nn[nn==""]=vectors[nn==""]
  }

  if (length(vectors)==0) {
    meta <- tibble::tibble(variable=NA,label=NA,dataset=datasets_from_vectors(vectors))
  }
  meta <- tibble::tibble(variable=vectors,label=nn,dataset=datasets_from_vectors(vectors)) %>%
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
           year=years_from_datasets(.data$dataset)) %>%
    mutate(label = coalesce(.data$label,.data$variable))
  meta
}



#' Generate metadata from Candian census vectors
#'
#' @description
#' \lifecycle{maturing}
#'
#' Add Population, Dwellings, and Household counts to metadata
#' @param meta ribble with metadata as for example provided by `meta_for_ca_census_vectors`
#' @return tibble with metadata
add_census_ca_base_variables <- function(meta){
  new_meta <- meta$geo_dataset %>%
    unique() %>%
    lapply(function(ds) {
      meta_for_additive_variables(ds,ca_census_base) %>%
        mutate(units="Number",
               year=years_from_datasets(ds))
    }) %>%
    bind_rows()
  meta %>%
    bind_rows(new_meta)
}

#' Get StatCan DA or DB level correspondence file
#'
#' @description
#' \lifecycle{maturing}
#'
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



#' Get StatCan correspondence data
#'
#' @description
#' \lifecycle{maturing}
#'
#' Get correspondence file for several Candian censuses on a common geography. Requires sf and cancensus package to be available
#'
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param geo_datasets vector of census geography dataset identifiers
#' @param level aggregation level to return data on (default is "CT")
#' @param method tongfen method, options are "statcan" (the default), "estimate", "identifier".
#' * "statcan" method builds up the common geography using Statistics Canada correspondence files, at this point
#' this method only works for "DB", "DA" and "CT" levels.
#' * "estimate" uses `estimate_tongfen_correspondence` to build up the common geography from scratch based on geographies.
#' * "identifier" assumes regions with identical geographic identifier are identical, and builds up the the correspondence for regions with unmatched geographic identifiers.
#' @param tolerance tolerance for `estimate_tongen_correspondence` in metres, default value is 50 metres.
#' @param area_mismatch_cutoff discard areas returned by `estimate_tongfen_correspondence` with area mismatch (log ratio) greater than cutoff.
#' @param quiet suppress download progress output, default is `FALSE`
#' @param refresh optional character, refresh data cache for this call, (default `FALSE`)
#' @return dataframe with the multi-census correspondence file
#' @export
get_tongfen_correspondence_ca_census <- function(geo_datasets, regions, level="CT", method="statcan",
                                                 tolerance = 50, area_mismatch_cutoff = 0.1,
                                                 quiet = FALSE, refresh = FALSE) {
  if (method=="statcan") {
    assert(level %in% c("DB","DA","CT"),"Level has to be one of DB, DA, or CT when using method = 'statcan'.")
    assert(length(setdiff(geo_datasets,  c("CA16","CA11","CA06","CA01")))==0,"Method 'statcan' only works for census years 2001 through 2016.")
  } else if (method=="estimate") {

  } else if (method=="identifier") {

  } else {
    stop(paste0("Unknown method ",method,", has to be one of 'statcan', 'estimate', 'identifier'"))
  }

  use_cache <- !refresh

  data <- lapply(geo_datasets,function(g_ds){
    cancensus::get_census(dataset=g_ds, regions=regions, level=level, geo_format='sf',
                          labels="short", quiet=quiet, use_cache = use_cache) %>%
      mutate(!!paste0("GeoUID",g_ds):=.data$GeoUID)
  }) %>%
    setNames(geo_datasets)

  if (method=="statcan") {
    statcan_level <- level
    if (!(statcan_level %in% c("DB","DA"))) statcan_level <- "DA"
    geo_years <- geo_datasets %>% years_from_datasets()
    years<-as.integer(geo_years)
    all_geo_years=seq(min(years),max(years),5)
    all_geo_datasets <- geo_dataset_for_years(all_geo_years)
    for (g_ds in setdiff(all_geo_datasets,geo_datasets)) {
      data[[g_ds]] <- cancensus::get_census(dataset=g_ds, regions=regions, level=level, geo_format='sf',
                                       labels="short", quiet=quiet, use_cache = use_cache) %>%
        mutate(!!paste0("GeoUID",g_ds):=.data$GeoUID)
    }
    prefix=paste0(statcan_level,"UID")

    if (level=="CT") {
      c_links <- geo_datasets %>%
        lapply(function(ds){
          da_column <- ds %>% years_from_datasets() %>% paste0("DAUID",.)
          match_column <- ds %>% paste0("GeoUID",.)
          cancensus::get_census(dataset=ds,regions=regions,level="DA",use_cache = use_cache,quiet=quiet) %>%
            select(.data$GeoUID,.data$CT_UID) %>%
            rename(!!match_column:=.data$CT_UID,
                   !!da_column:=.data$GeoUID)
        }) %>%
        setNames(geo_datasets)
    } else if (level %in% c("DB","DA")){
      c_links <- all_geo_datasets %>%
        lapply(function(ds){
          year <- years_from_datasets(ds)
          base_column <- paste0(prefix,year)
          match_column <- paste0("GeoUID",ds)
          data[[ds]] %>%
            st_set_geometry(NULL) %>%
            select_at(match_column) %>%
            mutate(!!base_column:=!!as.name(match_column))
        }) %>%
        setNames(all_geo_datasets)
    } else {
      stop("Oops, should have caught this earlier.")
    }

    correspondence_years=all_geo_years[-1]
    correspondence <- correspondence_years %>%
      lapply(function(year){
        c <- get_single_correspondence_ca_census_for(year,statcan_level) %>%
          select(-.data$flag)
        previous_year <- all_geo_years[which(all_geo_years==year)-1]
        ds1 <- all_geo_datasets[all_geo_years==year]
        ds2 <- all_geo_datasets[all_geo_years==previous_year]
        if (!is.null(ds1) && length(ds1)>0) {
          match_column <- intersect(names(c),names(c_links[[ds1]]))
          if (!is.null(match_column)) {
            c <- c %>%
              inner_join(c_links[[ds1]],by=match_column) %>%
              select(-all_of(match_column)) %>%
              unique()
          }
        }
        if (!is.null(ds2) && length(ds2)>0) {
          match_column <- intersect(names(c),names(c_links[[ds2]]))
          if (!is.null(match_column)) {
            c <- c %>%
              inner_join(c_links[[ds2]],by=match_column) %>%
              select(-all_of(match_column)) %>%
              unique()
          }
        }
        c %>%
          mutate(TongfenMethod="statcan")
      }) %>%
      aggregate_correspondences() %>%
      select(c(paste0("GeoUID",geo_datasets),"TongfenMethod")) %>%
      unique() %>%
      get_tongfen_correspondence()
    #setNames(correspondence_years)
  } else {
    geo_identifiers <- paste0("GeoUID",geo_datasets)
    correspondence <- estimate_tongfen_correspondence(data,
                                                      geo_identifiers,
                                                      method = method,
                                                      tolerance=200,
                                                      computation_crs=3347)
  }

  correspondence
}


#' Togfen data from several Canadian censuses
#'
#' @description
#' \lifecycle{maturing}
#'
#' Get data from several Candian censuses on a common geography. Requires sf and cancensus package to be available
#'
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param meta metadata for the census veraiables to aggregate, for example as returned
#' by \code{meta_for_ca_census_vectors}.
#' @param level aggregation level to return data on (default is "CT")
#' @param method tongfen method, options are "statcan" (the default), "estimate", "identifier".
#' * "statcan" method builds up the common geography using Statistics Canada correspondence files, at this point
#' this method only works for "DB", "DA" and "CT" levels.
#' * "estimate" uses `estimate_tongfen_correspondence` to build up the common geography from scratch based on geographies.
#' * "identifier" assumes regions with identical geographic identifier are identical, and builds up the the correspondence for regions with unmatched geographic identifiers.
#' @param base_geo base census year to build up common geography from, `NULL` (the default) to not return
#' any geographi data
#' @param na.rm logical, determines how NA values should be treated when aggregating variables
#' @param tolerance tolerance for `estimate_tongen_correspondence` in metres, default value is 50 metres,
#' only used when method is 'estimate' or 'identifier'
#' @param area_mismatch_cutoff discard areas returned by `estimate_tongfen_correspondence` with area mismatch (log ratio) greater than cutoff,
#' only used when method is 'estimate' or 'identifier'
#' @param quiet suppress download progress output, default is `FALSE`
#' @param refresh optional character, refresh data cache for this call, (default `FALSE`)
#' @param data_transform optional transform function to be applied to census data after being returned from cancensus
#' @return dataframe with variables on common geography
#' @export
get_tongfen_ca_census <- function(regions,meta,level="CT",method="statcan",
                                  base_geo=NULL,na.rm=FALSE,
                                  tolerance = 50,
                                  area_mismatch_cutoff = 0.1,
                                  quiet=FALSE,
                                  refresh=FALSE,
                                  data_transform=function(d)d) {
  use_cache <- !refresh

  geo_datasets <- meta$geo_dataset %>% unique() %>% sort()

  data <- lapply(geo_datasets,function(g_ds){
    vectors <- meta %>%
      filter(.data$geo_dataset == g_ds,
             .data$type != "Base") %>%
      pull(.data$variable) %>%
      as.character()
    c <- cancensus::get_census(dataset=g_ds, regions=regions,
                               vectors=vectors,
                               level=level, geo_format='sf',
                               labels="short", quiet=quiet, use_cache = use_cache) %>%
      mutate(!!paste0("GeoUID",g_ds):=.data$GeoUID)
  }) %>%
    setNames(geo_datasets)


  if (length(geo_datasets)==1) {
    # no need to tongfen
    aggregated_data <- data[[1]]
  } else {
    correspondence <- get_tongfen_correspondence_ca_census(geo_datasets = geo_datasets,
                                                           regions = regions,
                                                           level = level,
                                                           method = method,
                                                           tolerance = tolerance,
                                                           area_mismatch_cutoff = area_mismatch_cutoff,
                                                           quiet = quiet,
                                                           refresh = refresh)
    aggregated_data <- tongfen_aggregate(data,correspondence,meta)
  }
  aggregated_data %>%
    rename_with_meta(meta)
}


#' @import dplyr
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


