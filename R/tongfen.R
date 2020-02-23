#' Aggregate variables to common CTs, returns data2 on new tiling matching data1 geography
#' @param data1 cancensus CT level datatset for year1 < year2 to serve as base for common geography
#' @param data2 cancensus CT level datatset for year2 to be aggregated to common geography
#' @param data2_sum_vars vector of variable names to by summed up when aggregating geographies
#' @param data2_group_vars optional vector of grouping variables
#' @param na.rm optional parameter to remove NA values when summing, default = `TRUE`
#' @export
tongfen_ct <- function(data1,data2,data2_sum_vars,data2_group_vars=c(),na.rm=TRUE) {
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

#' Estimate variable values for custom geography
#' @param data1 custom geography to estimate values for
#' @param data2 input geography with values
#' @param data2_sum_vars columns of data2 to be summed up
#' @param unique_key optional unique key for each geography in data1
#' @export
tongfen_estimate <- function(data1,data2,data2_sum_vars,unique_key=NA) {
  # right now this only works for variables that are sums. Does not quite sum up to total in case areas don't overlap all that well.
  # but makes no assumptions on geograhies and is quite fast
  # the exlicit use of the "Group.1" variable feels fishy
  if (is.na(unique_key)) {
    unique_key="tongfen_row_number"
    data1 <- data1 %>% mutate(!!unique_key:=row_number())
  }
  # rename variables, st_interpolate_aw does not handle column names with special characters
  rename_vars <- purrr::set_names(data2_sum_vars,paste0("v",seq(1,length(data2_sum_vars))))
  rename_back <- purrr::set_names(names(rename_vars),as.character(rename_vars))
  result <- sf::st_interpolate_aw(data2 %>%
                                    select(data2_sum_vars) %>%
                                    rename(!!!rename_vars) %>%
                                    mutate_all(function(x)tidyr::replace_na(x,0)),
                                  data1,
                                  extensive = TRUE) %>%
    left_join(data1 %>%
                sf::st_set_geometry(NULL) %>%
                select(unique_key) %>%
                mutate(Group.1=row_number()),
              by="Group.1") %>%
    select(-.data$Group.1) %>%
    rename(!!!rename_back)

 result
}



#' Build tibble with information on how to aggregate variables given vectors
#' Queries list_census_variables to obtain needed information and add in vectors needed for aggregation
#' @param vectors list of variables to query
#' @param also_for_first also get extra variables for first dataset
#' @return tidy dataframe with metadata information for requested variables and additional variables
#' needed for tongfen operations
#' @export
meta_for_vectors <- function(vectors,also_for_first=FALSE){
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

  if (!also_for_first) {
    extras <- extras %>% filter(.data$dataset != datasets[1] | .data$type=="Original")
  }

  meta <- meta %>%
    bind_rows(extras) %>%
    mutate(geo_dataset=geo_dataset_from_dataset(.data$dataset),
                  year=years_from_datasets(.data$dataset))
  meta
}

#' Aggregate census data up, assumes data is grouped for aggregation
#' Uses data from meta to determine how to aggregate up
#' @param data census data as obtained from get_census call, grouped by TongfenID
#' @param meta list with variables and aggregation information as obtained from meta_for_vectors
#' @param geo logical, should also aggregate geographic data
#' @param na.rm logical, should NA values be ignored or carried through.
#' @return data frame with variables aggregated to new common geography
#' @export
aggregate_data_with_meta <- function(data,meta,geo=FALSE,na.rm=TRUE){
  grouping_var=groups(data) %>% as.character
  parent_lookup <- setNames(meta$parent,meta$variable)
  to_scale <-  filter(meta,.data$rule %in% c("Median","Average"))$variable
  to_scale_from <- filter(meta,.data$rule %in% c("AverageTo"))$variable
  not_additive <- filter(meta,.data$rule == "Not additive")$variable
  median_vars <- filter(meta,.data$rule %in% c("Median"))$variable

  if (length(not_additive)>0)
    warning(paste0("Don't know how to TongFen: ",paste0(not_additive,collapse = ", ")))
  if (length(median_vars)>0)
    message(paste0("Can't TongFen medians, will approximate by treating as averages: ",paste0(median_vars,collapse = ", ")))

  for (x in to_scale) {
    data <- data %>% mutate(!!x := !!as.name(x)*!!as.name(parent_lookup[x]))
  }
  base_variables <- c()
  for (x in to_scale_from) {
    scale_type <- meta %>% filter(.data$variable==x) %>% pull(units) %>% as.character()
    base_vector <- paste0("base_",parent_lookup[x])
    base_variables <- c(base_variables,base_vector)
    if (scale_type=="Percentage ratio (0.0-1.0)") {
      data <- data %>% mutate(!!base_vector:=!!as.name(parent_lookup[x])/(!!as.name(x)+1))
    } else if (scale_type=="Percentage (0-100)") {
      data <- data %>% mutate(!!base_vector:=!!as.name(parent_lookup[x])/(!!as.name(x)/100+1))
    } else {
      data <- data %>% mutate(!!base_vector:=!!as.name(parent_lookup[x])/(!!as.name(x)))
    }
    data <- data %>% mutate(!!x := !!as.name(x)*!!as.name(base_vector))
  }

  if (length(base_variables)>0)
    meta <- meta %>% bind_rows(tibble(variable=base_variables,type="Base"))

  if ("sf" %in% class(data)) {
    geo_column=attr(data,"sf_column")
    data <- left_join(data %>%
                        select(c(geo_column,grouping_var)) %>%
                        summarize(!!geo_column:=sf::st_union(!!as.name(geo_column))),
                      data %>% sf::st_set_geometry(NULL) %>% summarize_at(meta$variable,sum,na.rm=na.rm),
                      by=grouping_var)
  } else {
    data <- data %>% summarize_at(meta$variable,sum,na.rm=na.rm)
  }
  for (x in to_scale) {
    data <- data %>% mutate(!!x := !!as.name(x)/!!as.name(parent_lookup[x]))
  }
  for (x in to_scale_from) {
    scale_type <- meta %>% filter(.data$variable==x) %>% pull(units) %>% as.character()
    base_vector <- paste0("base_",parent_lookup[x])
    data <- data %>% mutate(!!x := !!as.name(x)/!!as.name(base_vector))
  }
  data
}

#' Grab variables from several censuses on a common geography. Requires sf package to be available
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format geographic format for returned data, 'sf' for sf format and `NA``
#' @param quiet suppress download progress output, default is `TRUE`
#' @param refresh optional character, refresh data cache for this call
#' for no geographic data, (default `NA`)
#' @return dataframe with census variables on common geography
#' @export
get_tongfen_census_ct <- function(regions,vectors,geo_format=NA,quiet=TRUE,refresh=FALSE) {
  labels="short"
  meta <- meta_for_vectors(vectors)
  geo_datasets <- meta$geo_dataset %>% unique %>% sort

  data <- lapply(geo_datasets,function(g_ds){
    cancensus::get_census(dataset=g_ds,regions=regions,
                          vectors=filter(meta,.data$geo_dataset==g_ds)$variable,
                          level="CT",geo_format='sf',labels="short",quiet=quiet,use_cache = !refresh)
  })

  base <- c("Population","Dwellings","Households")

  geo_dataset=geo_datasets[1]
  data1=data[[1]] %>% rename_at(base,function(x){paste0(x,"_",geo_dataset)}) %>% sf::st_as_sf()

  number_of_datasets=length(geo_datasets)
  if (number_of_datasets>1) { # only do this if we actually need to tongfen!
    for (index in seq(2,number_of_datasets)) {
      dataset = geo_datasets[index]
      data2=data[[index]] %>% sf::st_as_sf()
      cts_1 <- data1$GeoUID
      cts_2 <- data2$GeoUID
      cts_diff_1 <- setdiff(cts_1,cts_2) %>% sort
      cts_diff_2 <- setdiff(cts_2,cts_1) %>% sort

      # Some CTs change boundaries even through they don't change GeoUID
      # <sticking finger in ears and singing ...lalalala.../>
      d<-sf::st_intersection(
        data2 %>% filter(.data$GeoUID %in% cts_diff_2) %>%
          rename(GeoUID2=.data$GeoUID) %>%
          select(.data$GeoUID2,.data$geometry) %>% mutate(area2=sf::st_area(.data$geometry)),
        data1 %>% filter(.data$GeoUID %in% cts_diff_1) %>%
          select(.data$GeoUID,.data$geometry) %>% mutate(area=sf::st_area(.data$geometry))
      )

      d <- d %>% mutate(area3=sf::st_area(.data$geometry)) %>%
        mutate(ratio=as.numeric(.data$area3/.data$area2)) %>%
        filter(.data$ratio>0.1) %>%
        arrange(.data$ratio)

      dd<- d %>% as.data.frame %>%
        group_by(.data$GeoUID) %>%
        summarize(ratio=sum(.data$ratio)/n(),n=n())

      if(dd %>% filter(.data$n<=1) %>% nrow >0) {base::stop("problem with computing common ct data")}

      ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2)
      ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID)

      new2 <- data2 %>% as.data.frame %>%
        select(-.data$geometry) %>%
        mutate(GeoUID2=.data$GeoUID) %>%
        mutate(GeoUID=ifelse(.data$GeoUID %in% names(ct_translation2),
                                    as.character(ct_translation2[.data$GeoUID2]),.data$GeoUID)) %>%
        group_by(.data$GeoUID)

      nnew <- aggregate_data_with_meta(new2,
                                       bind_rows(meta %>% filter(.data$geo_dataset==!!dataset),
                                                        tibble(variable=base)))

      nnew <- nnew %>% rename_at(base,function(x){paste0(x,"_",dataset)})

      data1 <- data1 %>% left_join(nnew,by="GeoUID")
    }
  }
  data1 <- data1 %>%
    select_at(names(data1) %>% setdiff(filter(meta,.data$type=="Extra")$variable))
  if (is.na(geo_format)) {
    data1 <- data1 %>% sf::st_set_geometry(NULL)
  } else if (geo_format=="sp") {
    data1 <- data1 %>% sf::as_Spatial()
  }
  if (labels=="detailed") {
    stop("NOT IMPLEMENTED YET")
  }
  if (!is.null(names(vectors))){
    data1 <- data1 %>% rename(!!!vectors)
  }
  data1
}



#' Proportionally re-aggregate hierarchical data to lower-level w.r.t. values of the *base* variable
#' Also handles cases where lower level data may be available but blinded at times by filling in data from higher level
#'
#' Data at lower aggregation levels may not add up to the more accurate aggregate counts.
#' This function distributes the aggregate level counts proportionally (by population) to the containing lower
#' level geographic regions.
#'
#' @param data The base geographic data
#' @param parent_data Higher level geographic data
#' @param geo_match A named string informing on what column names to match data and parent_data
#' @param categories Vector of column names to re-aggregate
#' @param base Column name to use for proportional weighting when re-aggregating
#' @return dataframe with downsampled variables from parent_data
#' @keywords reaggregate proportionally wrt base variable
#' @export
proportional_reaggregate <- function(data,parent_data,geo_match,categories,base="Population"){
  # create zero categories if we don't have them on base (for example DB geo)
  for (v in setdiff(categories,names(data))) {
    data <- data %>% mutate(!!v := 0)
  }
  ## join and compute the weights
  ## maybe should be left join, but then have to worry about what happens if there is no match. For hierarchial data should always have higher level geo!
  d1 <- inner_join(data  %>% mutate(!!base:=tidyr::replace_na(!!as.name(base),0)),
                   select(parent_data %>% as.data.frame,c(categories,c(as.character(geo_match)))),
                   by=geo_match) %>%
    group_by(!!as.name(names(geo_match))) %>%
    mutate(weight=!!as.name(base)/sum(!!as.name(base),na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(weight=tidyr::replace_na(.data$weight,0))
  ## aggregate variables up and down
  ## lower level geography counts might have been suppressed, reaggregating these makes sure that the total number of
  ## dots on the map are given by more accurate higher level geo counts, difference is distributed proportionally by *base*
  for (v in categories) {
    vss=paste(v,'s',sep=".")
    vs=as.name(vss)
    vx=as.name(paste(v,'x',sep="."))
    vy=as.name(paste(v,'y',sep="."))
    d1 <- d1 %>%
      mutate(!!vx:=tidyr::replace_na(!!vx,0)) %>%
      group_by(!!as.name(names(geo_match))) %>%
      mutate(!!vss := sum(!!vx,na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(!!v := !!quo(UQ(vx) + .data$weight * (UQ(vy) - UQ(vs))))
  }
  ## clean up and return
  d1 %>%
    select(-one_of(c(paste0(categories,".s"),paste0(categories,".x"),paste0(categories,".y"))))
}


#' @import dplyr
#' @import rlang
#' @import sf
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

