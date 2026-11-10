# StatCan correspondence files as parquet, one file for each census year that links it to the previous
# census. The file for a year has the DA and DB level links from 2006 on, the DA and DB to EA links for
# 2001, and the EA level links before that.
correspondence_ca_census_url <- function(year){
  base_url <- nullify_blank(getOption("tongfen.statcan_correspondence_url")) %||%
    "https://data.mountainmath.ca/census/correspondence"
  paste0(sub("/+$","",base_url),"/correspondence_",year,".parquet")
}

# census years with StatCan correspondence files to the previous census
statcan_correspondence_years <- as.character(seq(1981,2021,5))

# census datasets the "statcan" method works for
statcan_geo_datasets <- c("CA1981","CA1986","CA1991","CA1996","CA01","CA06","CA11","CA16","CA21")

# census datasets that only have patchy geographies, these can only be used with the "statcan" method,
# and the geography of the common regions has to come from one of the other censuses
patchy_geo_datasets <- c("CA1981","CA1986")

# CensusMapper serves enumeration areas at the DA level, accept EA as an alias for those
normalize_ca_level <- function(level){
  level <- as.character(level)[1]
  if (identical(level,"EA")) "DA" else level
}

# StatCan geographic level for the requested DA or DB level in the given census year. Up to 1996 the
# smallest census geography were enumeration areas, CensusMapper serves those at the DA level.
statcan_unit_level <- function(year,level){
  ifelse(as.integer(year) <= 1996, "EA", level)
}

ca_census_base <- c("Population","Dwellings","Households")


years_from_datasets <- function(ds) {
  ds %>%
    stringr::str_extract("\\d+") %>%
    stringr::str_pad(width=3,side="left",pad="0") %>%
    stringr::str_pad(width=4,side="left",pad="2") %>%
    as.integer()
}


datasets_from_vectors <- function(vs){
  ds<-vs %>%
    stringr::str_split("_") %>%
    purrr::map(function(v)v[[2]]) %>%
    unlist()
  ds[grepl("^\\d{4}$",ds)]<-geo_dataset_for_years(ds[grepl("^\\d{4}$",ds)])
  ds
}

geo_dataset_for_years <- function(years){
  require_suggested("cancensus")
  dataset_list <- with_tongfen_user_agent(cancensus::list_census_datasets())
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
  require_suggested("cancensus")
  datasets <- datasets %>% gsub("^CA11[NF]$","CA11",.) %>% gsub("\\d{4}x","",.)
  dataset_list <- with_tongfen_user_agent(cancensus::list_census_datasets())
  lapply(datasets, function(ds){
    dataset_list %>%
      filter(.data$dataset == ds) %>%
      pull(.data$geo_dataset) %>%
      unique()
  }) %>%
    unlist()
}

#' Generate metadata from Canadian census vectors
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
#'
#' @examples
#' # Build metadata for vectors
#' \dontrun{
#' meta <- meta_for_ca_census_vectors(c("v_CA16_4836","v_CA16_4838","v_CA16_4899"))
#'}
meta_for_ca_census_vectors <- function(vectors){
  require_suggested("cancensus")
  nn <- names(vectors)
  vectors <- as.character(vectors) ## strip names just in case
  if (is.null(nn)) {
    nn <- vectors
  } else {
    nn[nn==""]=vectors[nn==""]
  }

  meta <- tibble::tibble(variable=vectors,label=nn,dataset=datasets_from_vectors(vectors)) %>%
    mutate(type="Original", aggregation="0",units=NA)
  datasets <- meta$dataset %>%
    unique %>%
    sort
  for (dataset in datasets){
    d <- with_tongfen_user_agent(cancensus::list_census_vectors(dataset)) %>%
      filter(.data$vector %in% (filter(meta,.data$dataset==dataset)$variable)) %>%
      select("vector","aggregation","units")
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
    select(variable="parent","dataset") %>%
    mutate(type="Extra",aggregation="Additive",rule="Additive") %>%
    filter(!is.na(.data$variable),!.data$variable %in% meta$variable) %>%
    distinct(.data$variable,.data$dataset,.keep_all=TRUE) %>%
    mutate(label=.data$variable)

  if (nrow(extras)>0) {
    meta <- meta %>%
      bind_rows(extras) %>%
      distinct(.data$variable,.data$dataset,.keep_all=TRUE)
  }

  meta <- meta %>%
    mutate(geo_dataset=geo_dataset_from_dataset(.data$dataset),
           year=years_from_datasets(.data$dataset))
  meta
}



#' Generate metadata from Canadian census vectors
#'
#' @description
#' \lifecycle{maturing}
#'
#' Add Population, Dwellings, and Household counts to metadata
#' @param meta tibble with metadata as for example provided by `meta_for_ca_census_vectors`
#' @return tibble with metadata
add_census_ca_base_variables <- function(meta){
  new_meta <- meta$geo_dataset %>%
    unique() %>%
    lapply(function(ds) {
      ca_base <- setdiff(ca_census_base,meta %>%
                           filter(.data$geo_dataset==ds) %>%
                           pull(.data$variable))
      meta_for_additive_variables(ds,ca_base) %>%
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
#' The correspondence files are downloaded from a mirror of the Statistics Canada correspondence files
#' and cached in the tongfen cache directory. The cached files are checked against the mirror once per session
#' and get downloaded again if they changed. The location of the mirror can be changed via the
#' `tongfen.statcan_correspondence_url` option.
#'
#' Up to 1996 the smallest census geography were enumeration areas (EA), so the 2001 file links DAs or DBs
#' to 1996 EAs, and the files before that link EAs to EAs, independent of `level`.
#'
#' @param year census year, the correspondence links this year to the previous census, 1981 through 2021
#' @param level geographic level, DA or DB
#' @param refresh reload the correspondence files, default is `FALSE`
#' @param links which version of the links to use, one of "statcan" (the default), "adjusted" or "linkflag".
#' Some of the older correspondence files carry a `LINKFLAG` from an audit of the StatCan links against the
#' geography of the regions, at this point these are the 1991 to 1986 and 1986 to 1981 files. The flag is 0 for
#' StatCan links the audit has no problem with, 1 for StatCan links the geography contradicts, and 2 for links
#' added by the audit for newer regions that are left without a link once the flagged links are dropped.
#' * "statcan" returns the links as supplied by Statistics Canada (flags 0 and 1).
#' * "adjusted" returns the audited links (flags 0 and 2).
#' * "linkflag" returns all links together with the `LINKFLAG` and `LINKREASON` columns.
#'
#' Files without link flags are returned as is, with a warning if "adjusted" or "linkflag" was requested.
#' @return tibble with correspondence table with the identifiers of the regions in both years, named
#' by level and year, e.g. `DAUID2021` and `DAUID2016` or `DAUID2001` and `EAUID1996`, and the `flag`
#' column with the flag from the source file. From 2006 on this is the relationship flag, with values 1 (one
#' new region was one old region), 2 (one new was many old), 3 (many new were one old) and 4 (many new were
#' many old). For 2001 and before it is the part flag, 1 if the older enumeration area corresponds to more
#' than one of the newer regions (more than one DA in 2001) and 0 otherwise. The part flags disagree with the
#' links in a few cases, and refer to the StatCan links, so they don't account for the links dropped or added
#' with `links = "adjusted"`. The "statcan" method only uses the links. With `links = "linkflag"` the table
#' also has the `LINKFLAG` and `LINKREASON` columns, if the file has link flags.
get_single_correspondence_ca_census_for <- function(year,level=c("DA","DB"),refresh=FALSE,
                                                    links=c("statcan","adjusted","linkflag")) {
  links <- match.arg(links)
  result <- read_statcan_correspondence(year,level,refresh=refresh,links=links)
  report_link_adjustments(list(attr(result,"link_adjustment")),links)
  attr(result,"link_adjustment") <- NULL
  result
}

# reads the StatCan correspondence file for the year and selects the links for the level, without messages
# about the link adjustments. The "link_adjustment" attribute has the details for `report_link_adjustments`.
read_statcan_correspondence <- function(year,level=c("DA","DB"),refresh=FALSE,links="statcan") {
  level=level[1]
  year=as.character(year)[1]
  if (!(level %in% c("DA","DB"))) stop("Level needs to be DA or DB")
  if (!(year %in% statcan_correspondence_years)) {
    stop(paste0("Year needs to be one of ",paste0(statcan_correspondence_years,collapse=", ")))
  }
  path=file.path(tongfen_cache_dir(),paste0("statcan_correspondence_",year,".parquet"))
  cached_download(correspondence_ca_census_url(year),path,refresh=refresh)

  new_level <- statcan_unit_level(year,level)
  old_level <- statcan_unit_level(as.integer(year)-5,level)
  flag_column <- if (old_level=="EA") "EAPARTFLAG" else paste0(level,"RELFLAG")
  columns <- c(paste0(new_level,"UID",year),paste0(old_level,"UID",as.integer(year)-5),flag_column)
  result <- tibble::as_tibble(nanoparquet::read_parquet(path))
  missing_columns <- setdiff(columns,names(result))
  if (length(missing_columns)>0) {
    stop(paste0("Correspondence file for ",year," does not have the expected columns ",
                paste0(missing_columns,collapse=", "),"."))
  }

  has_flags <- "LINKFLAG" %in% names(result)
  adjustment <- list(year=year, has_flags=has_flags)
  if (has_flags) {
    link_flag <- as.integer(result$LINKFLAG)
    adjustment$dropped <- sum(link_flag==1L, na.rm=TRUE)
    adjustment$added <- sum(link_flag==2L, na.rm=TRUE)
    adjustment$data_version <- correspondence_data_version(path)
    if (links=="statcan") {
      result <- result[link_flag %in% c(0L,1L),]
    } else if (links=="adjusted") {
      result <- result[link_flag %in% c(0L,2L),]
    } else {
      columns <- c(columns,intersect(c("LINKFLAG","LINKREASON"),names(result)))
    }
  }

  # the DA links are repeated for each DB in the files that have both
  result <- result %>%
    select(all_of(columns)) %>%
    rename(flag=all_of(flag_column)) %>%
    distinct()

  # manual corrections
  if (year=="2021" && level=="DB") {
    # inconsequential boundary shift Cov/UBC
    result <- result %>% filter(.data$DBUID2021!="59150934010")
  } else if (year=="2021" && level=="DA") {
    # inconsequential boundary shift Cov/UBC
    result <- result %>% filter(!(.data$DAUID2021=="59150934" & .data$DAUID2016 == "59150936"))
  }

  attr(result,"link_adjustment") <- adjustment
  result
}

# the data version from the parquet metadata of the correspondence file, NULL if not available
correspondence_data_version <- function(path){
  tryCatch({
    kv <- nanoparquet::read_parquet_metadata(path)$file_meta_data$key_value_metadata[[1]]
    nullify_blank(kv$value[kv$key=="data_version"][1])
  }, error=function(e) NULL)
}

# messages about the link adjustments for a list of "link_adjustment" attributes, one for each correspondence
# step. Steps with link flags get a message, if none of the steps have link flags and adjusted links or link flags
# were requested this issues a warning.
report_link_adjustments <- function(adjustments,links){
  step_label <- function(a) paste0(a$year," to ",as.integer(a$year)-5)
  flagged <- Filter(function(a) isTRUE(a$has_flags),adjustments)
  if (length(flagged)==0) {
    if (links!="statcan") {
      steps <- vapply(adjustments,step_label,character(1))
      warning(paste0("No link adjustment data is available for the correspondence ",
                     ifelse(length(steps)==1,"step ","steps "),paste0(steps,collapse=", "),
                     ", using the StatCan correspondence as is. ",
                     "If the cached correspondence files predate the link adjustments, use `refresh = TRUE`."),
              call.=FALSE)
    }
    return(invisible(NULL))
  }
  details <- vapply(flagged,function(a){
    version <- if (is.null(a$data_version)) "" else paste0(", data version ",a$data_version)
    paste0(step_label(a),": ",a$dropped," StatCan ",ifelse(a$dropped==1,"link","links")," flagged, ",
           a$added," ",ifelse(a$added==1,"link","links")," added",version)
  },character(1))
  intro <- switch(links,
                  statcan=paste0("Using the StatCan links. Audited link adjustments are available, ",
                                 "use `links = \"adjusted\"` to apply them:"),
                  adjusted="Using the adjusted links, StatCan links flagged by the audit are dropped and audited links added:",
                  linkflag="Returning all links with link flags:")
  message(paste0(c(intro,paste0("  ",details)),collapse="\n"))
  invisible(NULL)
}



#' Get StatCan correspondence data
#'
#' @description
#' \lifecycle{maturing}
#'
#' Get correspondence file for several Canadian censuses on a common geography. Requires sf and cancensus package to be available
#'
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param geo_datasets vector of census geography dataset identifiers
#' @param level aggregation level to return data on (default is "CT"), "EA" works as an alias for "DA"
#' @param method tongfen method, options are "statcan" (the default), "estimate", "identifier".
#' * "statcan" method builds up the common geography using Statistics Canada correspondence files, at this point
#' this method only works for "DB", "DA" and "CT" levels, and for the censuses from 1981 through 2021. Up to 1996
#' the smallest census geography were enumeration areas (EA), at the "DA" level these take the place of
#' dissemination areas for those censuses. The "DB" level only works for the censuses from 2001 on.
#' * "estimate" uses `estimate_tongfen_correspondence` to build up the common geography from scratch based on geographies.
#' * "identifier" assumes regions with identical geographic identifier are identical, and builds up the the correspondence for regions with unmatched geographic identifiers.
#'
#' The geographies of the 1981 and 1986 censuses are incomplete, these censuses only work with the "statcan" method.
#' @param links which version of the StatCan links to use with the "statcan" method, "statcan" (the default) for
#' the links as supplied by Statistics Canada, or "adjusted" for the audited links. The audit checked the StatCan links
#' against the geography of the regions, flagged links the geography contradicts and added links for regions left
#' without one. At this point audited links are available for the 1991 to 1986 and 1986 to 1981 steps, the other steps
#' use the StatCan links either way. A message lists the steps with audited links, and with "adjusted" there is a
#' warning if none of the steps have them. Ignored for the other methods.
#' @param tolerance tolerance for `estimate_tongfen_correspondence` in metres, default value is 50 metres,
#' only used when method is 'estimate' or 'identifier'
#' @param quiet suppress download progress output, default is `FALSE`
#' @param refresh optional character, refresh data cache for this call, (default `FALSE`)
#' @param crs CRS to use for the spatial intersections if method is 'identifier' or
#' 'estimate', default is `3347` (Statistics Canada Lambert)
#' @return dataframe with the multi-census correspondence file
#' @export
#'
#' @examples
#' # Get correspondance files between CTs in 2006 and 2016 censuses in Vancouver CMA
#' \dontrun{
#' correspondence <- get_tongfen_correspondence_ca_census(geo_datasets=c('CA06','CA16'),
#'                                                        regions=list(CMA="59933"),level='CT')
#'}
get_tongfen_correspondence_ca_census <- function(geo_datasets, regions, level="CT", method="statcan",
                                                 links = c("statcan","adjusted"),
                                                 tolerance = 50,
                                                 quiet = FALSE, refresh = FALSE, crs = 3347) {
  require_suggested("cancensus")

  geo_datasets <- normalize_datasets(geo_datasets)
  level <- normalize_ca_level(level)
  links <- match.arg(links)
  assert(length(unique(geo_datasets)) >= 2,
         "Need at least two census geographies to build a correspondence table.")
  if (method=="statcan") {
    assert(level %in% c("DB","DA","CT"),"Level has to be one of DB, DA, or CT when using method = 'statcan'.")
    assert(length(setdiff(geo_datasets, statcan_geo_datasets))==0,
           "Method 'statcan' only works for census years 1981 through 2021.")
    assert(level!="DB" || min(years_from_datasets(geo_datasets))>=2001,
           "Level DB only works for census years 2001 through 2021, use level DA or CT for earlier years.")
  } else if (method %in% c("estimate","identifier")) {
    patchy <- intersect(geo_datasets,patchy_geo_datasets)
    assert(length(patchy)==0,
           paste0("The geographies of ",paste0(patchy,collapse=" and "),
                  " are incomplete, use method = 'statcan' for these censuses."))
  } else {
    stop(paste0("Unknown method ",method,", has to be one of 'statcan', 'estimate', 'identifier'"))
  }

  use_cache <- !refresh

  # the "statcan" method only ever looks at geographic identifiers, only the
  # geometry based methods need to download the (potentially very large) geometries
  geo_format <- if (method=="statcan") NA else 'sf'

  if (method=="statcan" && level=="CT") {
    # correspondence is built from DA level links below, the CT level data is not needed
    data <- list()
  } else {
    data <- lapply(geo_datasets,function(g_ds){
      with_tongfen_user_agent(cancensus::get_census(dataset=g_ds, regions=regions, level=level, geo_format=geo_format,
                            labels="short", quiet=quiet, use_cache = use_cache)) %>%
        mutate(!!paste0("GeoUID",g_ds):=.data$GeoUID)
    }) %>%
      setNames(geo_datasets)
  }

  if (method=="statcan") {
    statcan_level <- level
    if (!(statcan_level %in% c("DB","DA"))) statcan_level <- "DA"
    geo_years <- geo_datasets %>% years_from_datasets()
    years<-as.integer(geo_years)
    all_geo_years=seq(min(years),max(years),5)
    all_geo_datasets <- geo_dataset_for_years(all_geo_years)
    if (level!="CT") {
      for (g_ds in setdiff(all_geo_datasets,geo_datasets)) {
        data[[g_ds]] <- with_tongfen_user_agent(cancensus::get_census(dataset=g_ds, regions=regions, level=level, geo_format=geo_format,
                                         labels="short", quiet=quiet, use_cache = use_cache)) %>%
          mutate(!!paste0("GeoUID",g_ds):=.data$GeoUID)
      }
    }

    if (level=="CT") {
      c_links <- geo_datasets %>%
        lapply(function(ds){
          year <- years_from_datasets(ds)
          da_column <- paste0(statcan_unit_level(year,"DA"),"UID",year)
          match_column <- ds %>% paste0("GeoUID",.)
          with_tongfen_user_agent(cancensus::get_census(dataset=ds,regions=regions,level="DA",use_cache = use_cache,quiet=quiet)) %>%
            select("GeoUID","CT_UID") %>%
            rename(!!match_column:="CT_UID",
                   !!da_column:="GeoUID")
        }) %>%
        setNames(geo_datasets)
    } else if (level %in% c("DB","DA")){
      c_links <- all_geo_datasets %>%
        lapply(function(ds){
          year <- years_from_datasets(ds)
          base_column <- paste0(statcan_unit_level(year,statcan_level),"UID",year)
          match_column <- paste0("GeoUID",ds)
          data[[ds]] %>%
            sf::st_drop_geometry() %>%
            select_at(match_column) %>%
            mutate(!!base_column:=!!as.name(match_column))
        }) %>%
        setNames(all_geo_datasets)
    } else {
      stop("Oops, should have caught this earlier.")
    }

    correspondence_years=all_geo_years[-1]
    steps <- correspondence_years %>%
      lapply(function(year){
        read_statcan_correspondence(year,statcan_level,refresh=refresh,links=links)
      })
    report_link_adjustments(lapply(steps,attr,"link_adjustment"),links)
    correspondence <- seq_along(correspondence_years) %>%
      lapply(function(i){
        year <- correspondence_years[i]
        c <- steps[[i]] %>%
          select(-"flag")
        previous_year <- all_geo_years[which(all_geo_years==year)-1]
        ds1 <- all_geo_datasets[all_geo_years==year]
        ds2 <- all_geo_datasets[all_geo_years==previous_year]
        if (!is.null(ds1) && length(ds1)>0) {
          match_column <- intersect(names(c),names(c_links[[ds1]]))
          if (length(match_column)>0) {
            c <- c %>%
              inner_join(c_links[[ds1]],by=match_column) %>%
              select(-all_of(match_column)) %>%
              unique()
          }
        }
        if (!is.null(ds2) && length(ds2)>0) {
          match_column <- intersect(names(c),names(c_links[[ds2]]))
          if (length(match_column)>0) {
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
                                                      tolerance=tolerance,
                                                      computation_crs=crs)
  }

  correspondence
}


#' Tongfen data from several Canadian censuses
#'
#' @description
#' \lifecycle{maturing}
#'
#' Get data from several Canadian censuses on a common geography. Requires sf and cancensus package to be available
#'
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param meta metadata for the census variables to aggregate, for example as returned
#' by \code{meta_for_ca_census_vectors}.
#' @param level aggregation level to return data on (default is "CT"). Enumeration areas, the smallest census
#' geography up to 1996, are at the "DA" level, "EA" works as an alias for "DA".
#' @param method tongfen method, options are "statcan" (the default), "estimate", "identifier".
#' * "statcan" method builds up the common geography using Statistics Canada correspondence files, at this point
#' this method only works for "DB", "DA" and "CT" levels, and for the censuses from 1981 through 2021. The
#' "DB" level only works for the censuses from 2001 on.
#' * "estimate" uses `estimate_tongfen_correspondence` to build up the common geography from scratch based on geographies.
#' * "identifier" assumes regions with identical geographic identifier are identical, and builds up the the correspondence for regions with unmatched geographic identifiers.
#'
#' The geographies of the 1981 and 1986 censuses are incomplete, these censuses only work with the "statcan" method.
#' @param links which version of the StatCan links to use with the "statcan" method, "statcan" (the default)
#' or "adjusted", see `get_tongfen_correspondence_ca_census` for details.
#' @param base_geo base census year to build up common geography from, `NULL` (the default) to not return
#' any geographic data. The geographies of the 1981 and 1986 censuses are incomplete, use one of the later
#' censuses for the common geography.
#' @param na.rm logical, determines how NA values should be treated when aggregating variables,
#' default is `FALSE`
#' @param tolerance tolerance for `estimate_tongfen_correspondence` in metres, default value is 50 metres,
#' only used when method is 'estimate' or 'identifier'
#' @param quiet suppress download progress output, default is `FALSE`
#' @param refresh optional character, refresh data cache for this call, (default `FALSE`)
#' @param crs optional CRS to transform data to, and use for spatial intersections if method is
#' 'identifier' or 'estimate', defaults to `3347` (Statistics Canada Lambert) for the intersections
#' @param data_transform optional transform function to be applied to census data after being returned from cancensus
#' @return dataframe with variables on common geography
#' @export
#'
#' @examples
#' # Get rent data for census years 2001 through 2016
#' \dontrun{
#' rent_variables <- c(rent_2001="v_CA01_1667",rent_2016="v_CA16_4901",
#'                     rent_2011="v_CA11N_2292",rent_2006="v_CA06_2050")
#' meta <- meta_for_ca_census_vectors(rent_variables)
#'
#' regions=list(CMA="59933")
#' rent_data <- get_tongfen_ca_census(regions=regions, meta=meta, quiet=TRUE,
#'                                    method="estimate", level="CT", base_geo = "CA16")
#'
#'}
get_tongfen_ca_census <- function(regions,meta,level="CT",method="statcan",
                                  links=c("statcan","adjusted"),
                                  base_geo=NULL,na.rm=FALSE,
                                  tolerance = 50,
                                  quiet=FALSE,
                                  refresh=FALSE,
                                  crs=NULL,
                                  data_transform=function(d)d) {
  require_suggested("cancensus")
  use_cache <- !refresh

  geo_datasets <- meta$geo_dataset %>% unique() %>% sort()
  level <- normalize_ca_level(level)
  links <- match.arg(links)

  if (!is.null(base_geo)) {
    base_geo <- normalize_datasets(base_geo)
    assert(length(base_geo)==1,"base_geo has to be a single dataset")
    assert(base_geo %in% geo_datasets,
           paste0("base_geo has to be one of the datasets ",paste0(geo_datasets,collapse=", ")))
    assert(!(base_geo %in% patchy_geo_datasets),
           paste0("The geography of ",base_geo," is incomplete, base_geo has to be one of the censuses ",
                  "from 1991 on."))
  }

  meta <- meta %>% add_census_ca_base_variables()

  data <- lapply(geo_datasets,function(g_ds){
    vectors <- meta %>%
      filter(.data$geo_dataset == g_ds,
             .data$type != "Base") %>%
      pull(.data$variable) %>%
      as.character()
    # only the base geography is returned, no need to download geometries for the others
    geo_format <- if (!is.null(base_geo) && g_ds==base_geo) 'sf' else NA
    c <- with_tongfen_user_agent(cancensus::get_census(dataset=g_ds, regions=regions,
                               vectors=vectors,
                               level=level, geo_format=geo_format,
                               labels="short", quiet=quiet, use_cache = use_cache)) %>%
      mutate(!!paste0("GeoUID",g_ds):=.data$GeoUID)
    if (!is.null(crs) && !is.null(base_geo) && g_ds==base_geo) c <- c %>% sf::st_transform(crs)
    c %>% data_transform()
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
                                                           links = links,
                                                           tolerance = tolerance,
                                                           quiet = quiet,
                                                           refresh = refresh,
                                                           crs = crs %||% 3347)
    aggregated_data <- tongfen_aggregate(data,correspondence,meta,
                                         base_geo=base_geo,na.rm=na.rm)
  }
  aggregated_data %>%
    rename_with_meta(meta)
}


#' @import dplyr
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


