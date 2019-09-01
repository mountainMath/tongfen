#' Aggregate variables to common cts, returns data2 on new tiling matching data1 geography
#' @param data1 Cancensus CT level datatset for year1 < year2 to serve as base for common geography
#' @param data2 Cancensus CT level datatset for year2 to be aggregated to common geography
#' @param data2_sum_vars vector of variable names to by summed up when aggregating geographies
#' @param data_2_group_vars optional vector of grouping variables
#' @param na.rm optional parameter to remove NA values when summing, default = `TRUE`
#' @export
tongfen_ct <- function(data1,data2,data2_sum_vars,data2_group_vars=c(),na.rm=TRUE) {
  cts_1 <- data1$GeoUID
  cts_2 <- data2$GeoUID
  cts_diff_1 <- setdiff(cts_1,cts_2) %>% sort
  cts_diff_2 <- setdiff(cts_2,cts_1) %>% sort

  d<-st_intersection(
    data2 %>% dplyr::filter(.data$GeoUID %in% cts_diff_2) %>%
      dplyr::rename(GeoUID2=GeoUID) %>%
      dplyr::select(GeoUID2) %>% dplyr::mutate(area2=st_area(.data$geometry)),
    data1 %>% dplyr::filter(.data$GeoUID %in% cts_diff_1) %>%
      dplyr::select(GeoUID) %>% dplyr::mutate(area=st_area(.data$geometry))
  )

  d <- d %>% dplyr::mutate(area3=st_area(geometry)) %>%
    dplyr::mutate(ratio=as.numeric(area3/area2)) %>%
    dplyr::filter(ratio>0.1) %>%
    dplyr::arrange(ratio)

  dd<- d %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(GeoUID) %>%
    summarize(ratio=sum(ratio)/n(),n=n())

  if(dd %>% dplyr::filter(n<=1) %>% nrow >0) {base::stop("problem with computing common ct data")}

  ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2 %>% unique)
  ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID %>% unique)

  new2 <- data2 %>%
    dplyr::filter(GeoUID %in% cts_diff_2) %>%
    dplyr::mutate(GeoUID2=GeoUID) %>%
    dplyr::mutate(GeoUID=as.character(ct_translation2[GeoUID2])) %>%
    dplyr::group_by(!!!c("GeoUID",data2_group_vars) %>% purrr::map(as.name) %>% unlist)

  nnew <- new2 %>% dplyr::summarize_at(data2_sum_vars,function(x)sum(x,na.rm=na.rm))

  data_2 <- rbind(data2 %>% dplyr::filter(!(GeoUID %in% cts_diff_2)) %>% dplyr::select("GeoUID",data2_group_vars,data2_sum_vars), nnew)
  return(data_2)
}

#' Estimate variable values for custom geography
#' @param data1 custom geography to estimate values for
#' @param data2 input geography with values
#' @param data_2_sum_vars columns of data2 to be summed up
#' @param unique_key optional unique key for each geography in data1
#' @export
tongfen_estimate <- function(data1,data2,data2_sum_vars,unique_key=NA) {
  # right now this only works for variables that are sums. Does not quite sum up to total in case areas don't overlap all that well.
  # but makes no assumptions on geograhies and is quite fast
  # the exlicit use of the "Group.1" variable feels fishy
  if (is.na(unique_key)) {
    unique_key="tongfen_row_number"
    data1 <- data1 %>% dplyr::mutate(!!unique_key:=dplyr::row_number())
  }
  # rename variables, st_interpolate_aw does not handle column names with special characters
  rename_vars <- purrr::set_names(data2_sum_vars,paste0("v",seq(1,length(data2_sum_vars))))
  rename_back <- purrr::set_names(names(rename_vars),as.character(rename_vars))
  result <- sf::st_interpolate_aw(data2 %>%
                                    dplyr::select(data2_sum_vars) %>%
                                    dplyr::rename(!!!rename_vars) %>%
                                    dplyr::mutate_all(function(x)tidyr::replace_na(x,0)),
                                  data1,
                                  extensive = TRUE) %>%
    dplyr::left_join(data1 %>%
                sf::st_set_geometry(NULL) %>%
                dplyr::select(unique_key) %>%
                dplyr::mutate(Group.1=dplyr::row_number()),
              by="Group.1") %>%
    dplyr::select(-Group.1) %>%
    dplyr::rename(!!!rename_back)

 result
}




#' Build tibble with information on how to aggregate variables given vectors
#' Queries list_census_variables to obtain needed information and add in vectors needed for aggregation
#' @param vectors list of variables to query
#' @param also_for_first also get extra variables for first dataset
#' @export
meta_for_vectors <- function(vectors,also_for_first=FALSE){
  meta <- tibble::tibble(variable=vectors,dataset=vectors %>% substr(3,6)) %>% dplyr::mutate(type="Original",aggregation=NA)
  datasets <- meta$dataset %>% unique %>% sort
  for (dataset in datasets){
    d<- list_census_vectors(dataset) %>%
      dplyr::filter(vector %in% (dplyr::filter(meta,`dataset`==dataset)$variable)) %>%
      dplyr::select(vector,aggregation)
    lookup <- setNames(d$aggregation,d$vector)
    meta <- meta %>% dplyr::mutate(aggregation=ifelse(variable %in% names(lookup),lookup[variable],aggregation))
  }
  get_vector <- function(g){
    g %>% strsplit(" ") %>% purrr::map(function(a){ifelse(length(a)==3,a[3],NA)}) %>% unlist
  }
  meta <- meta %>% dplyr::mutate(rule=sub(" .+$","",aggregation),parent=get_vector(aggregation))
  if (also_for_first) {
    extras <- meta$parent %>% na.omit %>% unique %>% setdiff(meta$variable)
  } else {
    extras <-  dplyr::filter(meta,dataset != datasets[1])$parent %>% na.omit %>% unique %>% setdiff(meta$variable)
  }
  meta <- meta %>% bind_rows(
    tibble(variable=extras) %>% dplyr::mutate(dataset=variable %>% substr(3,6),type="Extra")
  )
  meta
}

#' Aggregate census data up, assumes data is grouped for aggregation
#' Uses data from meta to determine how to aggregate up
#' @param data census data as obtained from get_census call
#' @param meta list with variables and aggregation infromation as obtained from meta_for_vectors
#' @param geo logical, should also aggregate geographic data
#' @param na.rm logical, should NA values be ignored or carried through.
#' @export
aggregate_data_with_meta <- function(data,meta,geo=FALSE,na.rm=TRUE){
  grouping_var=groups(data) %>% as.character
  parent_lookup <- setNames(meta$parent,meta$variable)
  to_scale <-  dplyr::filter(meta,rule %in% c("Median","Average"))$variable

  for (x in to_scale) {
    data <- data %>% dplyr::mutate(!!x := !!as.name(x)*!!as.name(parent_lookup[x]))
  }

  if ("sf" %in% class(data)) {
    geo_column=attr(data,"sf_column")
    data <- dplyr::left_join(data %>%
                               dplyr::select(c(geo_column,grouping_var)) %>%
                               dplyr::summarize(!!geo_column:=sf::st_union(!!as.name(geo_column))),
                             data %>% sf::st_set_geometry(NULL) %>% dplyr::summarize_at(meta$variable,sum,na.rm=na.rm),
                             by=grouping_var
    )
  } else {
    data <- data %>% dplyr::summarize_at(meta$variable,sum,na.rm=na.rm)
  }
  for (x in to_scale) {
    data <- data %>% dplyr::mutate(!!x := !!as.name(x)/!!as.name(parent_lookup[x]))
  }
  data
}

#' Grab variables from several censuses on a common geography. Requires sf package to be avaialbe
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @param geo_format geographic format for returned data, 'sf' for sf format and `NA``
#' for no gegraphic data, (default `NA`)
#' @export
get_tongfen_census_ct <- function(regions,vectors,geo_format=NA) {
  labels="short"
  meta <- meta_for_vectors(vectors)
  datasets <- meta$dataset %>% unique %>% sort

  data <- lapply(datasets,function(ds){
    cancensus::get_census(dataset=ds,regions=regions,vectors=dplyr::filter(meta,dataset==ds)$variable,
                          level="CT",geo_format='sf',labels="short")
  })

  base <- c("Population","Dwellings","Households")

  dataset=datasets[1]
  data1=data[[1]] %>% dplyr::rename_at(base,function(x){paste0(x,"_",dataset)}) %>% sf::st_as_sf()

  number_of_datasets=length(datasets)
  if (number_of_datasets>1) { # only do this if we actually need to tongfen!
    for (index in seq(2,number_of_datasets)) {
      dataset = datasets[index]
      data2=data[[index]] %>% sf::st_as_sf()
      cts_1 <- data1$GeoUID
      cts_2 <- data2$GeoUID
      cts_diff_1 <- setdiff(cts_1,cts_2) %>% sort
      cts_diff_2 <- setdiff(cts_2,cts_1) %>% sort

      # Some CTs change boundaries even through they don't change GeoUID
      # <sticking finger in ears and singing ...lalalala.../>
      d<-sf::st_intersection(
        data2 %>% dplyr::filter(.data$GeoUID %in% cts_diff_2) %>%
          dplyr::rename(GeoUID2=GeoUID) %>%
          dplyr::select(GeoUID2,geometry) %>% dplyr::mutate(area2=sf::st_area(.data$geometry)),
        data1 %>% dplyr::filter(GeoUID %in% cts_diff_1) %>%
          dplyr::select(GeoUID,geometry) %>% dplyr::mutate(area=sf::st_area(.data$geometry))
      )

      d <- d %>% dplyr::mutate(area3=sf::st_area(.data$geometry)) %>%
        dplyr::mutate(ratio=as.numeric(area3/area2)) %>%
        dplyr::filter(ratio>0.1) %>%
        dplyr::arrange(ratio)

      dd<- d %>% as.data.frame %>%
        dplyr::group_by(GeoUID) %>%
        dplyr::summarize(ratio=sum(.data$ratio)/n(),n=n())

      if(dd %>% dplyr::filter(n<=1) %>% nrow >0) {base::stop("problem with computing common ct data")}

      ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2)
      ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID)

      new2 <- data2 %>% as.data.frame %>%
        dplyr::select(-geometry) %>%
        dplyr::mutate(GeoUID2=GeoUID) %>%
        dplyr::mutate(GeoUID=ifelse(GeoUID %in% names(ct_translation2),as.character(ct_translation2[GeoUID2]),GeoUID)) %>%
        dplyr::group_by(GeoUID)

      nnew <- aggregate_data_with_meta(new2, dplyr::bind_rows(meta %>% dplyr::filter(dataset==!!dataset),tibble(variable=base)))

      nnew <- nnew %>% dplyr::rename_at(base,function(x){paste0(x,"_",dataset)})

      data1 <- data1 %>% dplyr::left_join(nnew,by="GeoUID")
    }
  }
  data1 <- data1 %>%
    dplyr::select_at(names(data1) %>% setdiff(dplyr::filter(meta,type=="Extra")$variable))
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



#' Proportionally re-aggregate hierarchichal data to lower-level w.r.t. values of the *base* variable
#' Also handles cases where lower level data may be available but blinded at times by filling in data from higher level
#'
#' Data at lower aggregation levels may not add up to the more accurate aggregate counts.
#' This function distributes the aggregate level counts proprtionally (by population) to the containing lower
#' leve geographic regions.
#'
#' @param data The base geographic data
#' @param parent_data Higher level geographic data
#' @param geo_match A named string informing on what column names to match data and parent_data
#' @param categories Vector of column names to re-aggreagte
#' @param base Column name to use for proportional weighting when re-aggregating
#' @keywords reaggregate proportionally wrt base variable
#' @export
proportional_reaggregate <- function(data,parent_data,geo_match,categories,base="Population"){
  # create zero categories if we don't have them on base (for example DB geo)
  for (v in setdiff(categories,names(data))) {
    data <- data %>% dplyr::mutate(!!v := 0)
  }
  ## join and compute the weights
  ## maybe should be left join, but then have to worry about what happens if there is no match. For hierarchial data should always have higher level geo!
  d1 <- inner_join(data  %>% dplyr::mutate(!!base:=tidyr::replace_na(!!as.name(base),0)),
                   dplyr::select(parent_data %>% as.data.frame,c(categories,c(as.character(geo_match)))),
                   by=geo_match) %>%
    dplyr::group_by(!!as.name(names(geo_match))) %>%
    dplyr::mutate(weight=!!as.name(base)/sum(!!as.name(base),na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weight=tidyr::replace_na(weight,0))
  ## aggregate variables up and down
  ## lower level geography counts might have been suppressed, reaggregating these makes sure that the total number of
  ## dots on the map are given by more accurate higher level geo counts, difference is distributed proportionally by *base*
  for (v in categories) {
    vss=paste(v,'s',sep=".")
    vs=as.name(vss)
    vx=as.name(paste(v,'x',sep="."))
    vy=as.name(paste(v,'y',sep="."))
    d1 <- d1 %>%
      dplyr::mutate(!!vx:=tidyr::replace_na(!!vx,0)) %>%
      dplyr::group_by(!!as.name(names(geo_match))) %>%
      dplyr::mutate(!!vss := sum(!!vx,na.rm=TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!v := !!quo(UQ(vx) + weight * (UQ(vy) - UQ(vs))))
  }
  ## clean up and return
  d1 %>%
    dplyr::select(-dplyr::one_of(c(paste0(categories,".s"),paste0(categories,".x"),paste0(categories,".y"))))
}



#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @import sf
NULL

