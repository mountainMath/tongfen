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
    data2 %>% dplyr::filter(GeoUID %in% cts_diff_2) %>%
      rename(GeoUID2=GeoUID) %>%
      dplyr::select(GeoUID2) %>% dplyr::mutate(area2=st_area(geometry)),
    data1 %>% dplyr::filter(GeoUID %in% cts_diff_1) %>%
      dplyr::select(GeoUID) %>% dplyr::mutate(area=st_area(geometry))
  )

  d <- d %>% dplyr::mutate(area3=st_area(geometry)) %>%
    dplyr::mutate(ratio=as.numeric(area3/area2)) %>%
    dplyr::filter(ratio>0.1) %>%
    arrange(ratio)

  dd<- d %>% as.data.frame %>%
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
    left_join(data1 %>%
                as.data.frame %>%
                dplyr::select(unique_key) %>%
                dplyr::mutate(Group.1=row_number()),
              by="Group.1") %>%
    dplyr::select(-Group.1) %>%
    dplyr::rename(!!!rename_back)

 result
}


#' @export
tongfen_cancensus <- function(data1,data2,prefix=c("x_","y_")) {
  # not impemented yet. will take cancensus outputs data1 and data2 and return a common geography with both variables on it.
  # will call cancensus::list_census_vectors to get aggregation rules and will download the census correspondence files at DA or DB level
  # will also query region information for intermediate regions to aggregate up the hierarchy

  # It might be better to just run this on CensusMapper and make the resulting datasets on common geographies available for general download....
}


#' Build tibble with information on how to aggregate variables given vectors
#' Queries list_census_variables to obtain needed information and add in vectors needed for aggregation
#' @param vectors list of variables to query
#' @param also_for_first also get extra variables for first dataset
#' @export
meta_for_vectors <- function(vectors,also_for_first=FALSE){
  meta <- tibble(variable=vectors,dataset=vectors %>% substr(3,6)) %>% mutate(type="Original",aggregation=NA)
  datasets <- meta$dataset %>% unique %>% sort
  for (dataset in datasets){
    d<- list_census_vectors(dataset) %>% filter(vector %in% (meta %>% filter(`dataset`==dataset) %>% pull(variable))) %>% select(vector,aggregation)
    lookup <- setNames(d$aggregation,d$vector)
    meta <- meta %>% mutate(aggregation=ifelse(variable %in% names(lookup),lookup[variable],aggregation))
  }
  get_vector <- function(g){
    g %>% strsplit(" ") %>% purrr::map(function(a){ifelse(length(a)==3,a[3],NA)}) %>% unlist
  }
  meta <- meta %>% mutate(rule=sub(" .+$","",aggregation),parent=get_vector(aggregation))
  if (also_for_first) {
    extras <- meta %>% pull(parent) %>% na.omit %>% unique %>% setdiff(meta$variable)
  } else {
    extras <- meta %>% filter(dataset != datasets[1]) %>% pull(parent) %>% na.omit %>% unique %>% setdiff(meta$variable)
  }
  meta <- meta %>% bind_rows(
    tibble(variable=extras) %>% mutate(dataset=variable %>% substr(3,6),type="Extra")
  )
  meta
}

#' Aggregate census data up, assumes data is grouped for aggregation
#' Uses data from meta to determine how to aggregate up
#' @param data census data as obtained from get_census call
#' @param meta list with variables and aggregation infromation as obtained from meta_for_vectors
#' @export
aggregate_data_with_meta <- function(data,meta){
  parent_lookup <- setNames(meta$parent,meta$variable)
  to_scale <- meta %>% filter(rule %in% c("Median","Average")) %>% pull(variable)

  for (x in to_scale) {
    data <- data %>% mutate(!!x := !!as.name(x)*!!as.name(parent_lookup[x]))
  }
  data <- data %>% summarize_at(meta$variable,sum,na.rm=TRUE)

  for (x in to_scale) {
    data <- data %>% mutate(!!x := !!as.name(x)/!!as.name(parent_lookup[x]))
  }
  data
}

#' Grab variables from several censuses on a common geography. Requires sf package to be avaialbe
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @export
get_tongfen_census_ct <- function(regions,vectors,geo_format=NA,labels="short") {
  meta <- meta_for_vectors(vectors)
  datasets <- meta$dataset %>% unique %>% sort

  data <- lapply(datasets,function(ds){
    get_census(dataset=ds,regions=regions,vectors=meta %>% filter(dataset==ds) %>% pull(variable),level="CT",geo_format='sf',labels="short")
  })

  base <- c("Population","Dwellings","Households")

  dataset=datasets[1]
  data1=data[[1]] %>% rename_at(base,function(x){paste0(x,"_",dataset)}) %>% sf::st_as_sf()

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
        data2 %>% dplyr::filter(GeoUID %in% cts_diff_2) %>%
          rename(GeoUID2=GeoUID) %>%
          select(GeoUID2,geometry) %>% dplyr::mutate(area2=sf::st_area(geometry)),
        data1 %>% dplyr::filter(GeoUID %in% cts_diff_1) %>%
          select(GeoUID,geometry) %>% dplyr::mutate(area=sf::st_area(geometry))
      )

      d <- d %>% dplyr::mutate(area3=sf::st_area(geometry)) %>%
        dplyr::mutate(ratio=as.numeric(area3/area2)) %>%
        dplyr::filter(ratio>0.1) %>%
        arrange(ratio)

      dd<- d %>% as.data.frame %>%
        dplyr::group_by(GeoUID) %>%
        summarize(ratio=sum(ratio)/n(),n=n())

      if(dd %>% dplyr::filter(n<=1) %>% nrow >0) {base::stop("problem with computing common ct data")}

      ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2)
      ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID)

      # parent_lookup <- set_names(meta$parent,meta$variable)
      # to_scale <- meta %>% filter(dataset==!!dataset,rule %in% c("Median","Average")) %>% pull(variable)
      #
      # new2 <- data2 %>% as.data.frame %>%
      #   dplyr::select(-geometry) %>%
      #   #dplyr::filter(GeoUID %in% cts_diff_2) %>%
      #   dplyr::mutate(GeoUID2=GeoUID) %>%
      #   dplyr::mutate(GeoUID=ifelse(GeoUID %in% names(ct_translation2),as.character(ct_translation2[GeoUID2]),GeoUID))
      #
      # for (x in to_scale) {
      #   new2 <- new2 %>% mutate(!!x := !!as.name(x)*!!as.name(parent_lookup[x]))
      # }
      # sum_vars <- c(base,meta %>% filter(dataset==!!dataset) %>% pull(variable))
      # nnew <- new2 %>% dplyr::group_by(GeoUID) %>% summarize_at(sum_vars,sum,na.rm=TRUE)
      #
      # for (x in to_scale) {
      #   nnew <- nnew %>% mutate(!!x := !!as.name(x)/!!as.name(parent_lookup[x]))
      # }

      new2 <- data2 %>% as.data.frame %>%
        dplyr::select(-geometry) %>%
        dplyr::mutate(GeoUID2=GeoUID) %>%
        dplyr::mutate(GeoUID=ifelse(GeoUID %in% names(ct_translation2),as.character(ct_translation2[GeoUID2]),GeoUID)) %>%
        group_by(GeoUID)

      nnew <- aggregate_data_with_meta(new2, bind_rows(meta %>% filter(dataset==!!dataset),tibble(variable=base)))

      nnew <- nnew %>% rename_at(base,function(x){paste0(x,"_",dataset)})

      data1 <- data1 %>% left_join(nnew,by="GeoUID")
    }
  }
  data1 <- data1 %>% select_at(names(data1) %>% setdiff(dplyr::filter(meta,type=="Extra")$variable))
  if (is.na(geo_format)) {
    data1 <- data1 %>% as.data.frame %>% dplyr::select(-geometry)
  } else if (geo_format=="sp") {
    data1 <- data1 %>% as("Spatial")
  }
  if (labels=="detailed") {
    stop("NOT IMPLEMENTED YET")
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
#' @examples
#' proportional_re_aggregate(data=geo_db@data,parent_data=geo_da@data,geo_match=setNames("GeoUID","DA_UID"),categories=categories)
proportional_reaggregate <- function(data,parent_data,geo_match,categories,base="Population"){
  # create zero categories if we don't have them on base (for example DB geo)
  for (v in setdiff(categories,names(data))) {
    data <- data %>% dplyr::mutate(!!v := 0)
  }
  ## join and compute the weights
  ## maybe should be left join, but then have to worry about what happens if there is no match. For hierarchial data should always have higher level geo!
  d1 <- inner_join(data  %>% dplyr::mutate(!!base:=tidyr::replace_na(!!as.name(base),0)),
                   dplyr::select(d2 %>% as.data.frame,c(categories,c(as.character(geo_match)))),
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

