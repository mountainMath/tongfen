#' Aggregate variables to common cts, returns data2 on new tiling matching data1 geography
#' @param data1 Cancensus CT level datatset for year1 < year2 to serve as base for common geography
#' @param data2 Cancensus CT level datatset for year2 to be aggregated to common geography
#' @param data2_sum_vars vector of variable names to by summed up when aggregating geographies
#' @export
tongfen_ct <- function(data1,data2,data2_sum_vars) {
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

  ct_translation <- lapply(split(d, d$GeoUID), function(x) x$GeoUID2)
  ct_translation2 <- lapply(split(d, d$GeoUID2), function(x) x$GeoUID)

  new2 <- data2 %>%
    dplyr::filter(GeoUID %in% cts_diff_2) %>%
    dplyr::mutate(GeoUID2=GeoUID) %>%
    dplyr::mutate(GeoUID=as.character(ct_translation2[GeoUID2])) %>%
    dplyr::group_by(GeoUID)

  nnew <- summarize_at(new2,data2_sum_vars,sum)

  data_2 <- rbind(data2 %>% dplyr::filter(!(GeoUID %in% cts_diff_2)) %>% dplyr::select("GeoUID",data2_sum_vars), nnew)
  return(data_2)
}


#' @export
tongfen_estimate <- function(data1,data2,data2_sum_vars,unique_key=NA) {
  # right now this only works for variables that are sums. Does not quite sum up to total in case areas don't overlap all that well.
  # but makes no assumptions on geograhies and is quite fast
  # the exlicit use of the "Group.1" variable feels fishy
  if (is.na(unique_key)) {
    unique_key="tongfen_row_number"
    data1 <- data1 %>% mutate(!!unique_key:=row_number())
  }
  result <- st_interpolate_aw(data2[data2_sum_vars],data1,extensive = TRUE) %>%
    left_join(data1 %>% as.data.frame %>% select(unique_key) %>% mutate(Group.1=row_number()),by="Group.1") %>%
    select(-Group.1)

 result
}


#' @export
tongfen_cancensus <- function(data1,data2,prefix=c("x_","y_")) {
  # not impemented yet. will take cancensus outputs data1 and data2 and return a common geography with both variables on it.
  # will call cancensus::list_census_vectors to get aggregation rules and will download the census correspondence files at DA or DB level
  # will also query region information for intermediate regions to aggregate up the hierarchy

  # It might be better to just run this on CensusMapper and make the resulting datasets on common geographies available for general download....
}


#' Grab variables from several censuses on a common geography. Requires sf package to be avaialbe
#' Will return CT level data
#' @param regions census region list, should be inclusive list of GeoUIDs across censuses
#' @param vectors List of cancensus vectors, can come from different census years
#' @export
get_tongfen_census_ct <- function(regions,vectors,geo_format=NA,labels="short") {
  meta <- tibble(variable=vectors,dataset=vectors %>% substr(3,6)) %>% mutate(type="Original",aggregation=NA)
  datasets <- meta$dataset %>% unique %>% sort
  for (dataset in datasets){
    d<- list_census_vectors(dataset) %>% filter(vector %in% (meta %>% filter(`dataset`==dataset) %>% pull(variable))) %>% select(vector,aggregation)
    lookup <- set_names(d$aggregation,d$vector)
    meta <- meta %>% mutate(aggregation=ifelse(variable %in% names(lookup),lookup[variable],aggregation))
  }
  get_vector <- function(g){
    g %>% strsplit(" ") %>% map(function(a){ifelse(length(a)==3,a[3],NA)}) %>% unlist
  }
  meta <- meta %>% mutate(rule=sub(" .+$","",aggregation),parent=get_vector(aggregation))
  extras <- meta %>% filter(dataset != datasets[1]) %>% pull(parent) %>% na.omit %>% unique %>% setdiff(meta$variable)
  meta <- meta %>% bind_rows(
    tibble(variable=extras) %>% mutate(dataset=variable %>% substr(3,6),type="Extra")
  )

  data <- lapply(datasets,function(ds){
    get_census(dataset=ds,regions=regions,vectors=meta %>% filter(dataset==ds) %>% pull(variable),level="CT",geo_format='sf',labels="short")
  })

  base <- c("Population","Dwellings","Households")

  dataset=datasets[1]
  data1=data[[1]] %>% rename_at(base,function(x){paste0(x,"_",dataset)}) %>% sf::st_as_sf()

  for (index in seq(2,length(datasets))) {
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
        sf::select.sf(GeoUID2) %>% dplyr::mutate(area2=sf::st_area(geometry)),
      data1 %>% dplyr::filter(GeoUID %in% cts_diff_1) %>%
        sf::select.sf(GeoUID) %>% dplyr::mutate(area=sf::st_area(geometry))
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

    parent_lookup <- set_names(meta$parent,meta$variable)
    to_scale <- meta %>% filter(dataset==!!dataset,rule %in% c("Median","Average")) %>% pull(variable)

    new2 <- data2 %>% as.data.frame %>%
      dplyr::select(-geometry) %>%
      #dplyr::filter(GeoUID %in% cts_diff_2) %>%
      dplyr::mutate(GeoUID2=GeoUID) %>%
      dplyr::mutate(GeoUID=ifelse(GeoUID %in% names(ct_translation2),as.character(ct_translation2[GeoUID2]),GeoUID))

    for (x in to_scale) {
      new2 <- new2 %>% mutate(!!x := !!as.name(x)*!!as.name(parent_lookup[x]))
    }
    sum_vars <- c(base,meta %>% filter(dataset==!!dataset) %>% pull(variable))
    nnew <- new2 %>% dplyr::group_by(GeoUID) %>% summarize_at(sum_vars,sum,na.rm=TRUE)

    for (x in to_scale) {
      nnew <- nnew %>% mutate(!!x := !!as.name(x)/!!as.name(parent_lookup[x]))
    }

    nnew <- nnew %>% rename_at(base,function(x){paste0(x,"_",dataset)})

    data1 <- data1 %>% left_join(nnew,by="GeoUID")
  }

  data1 <- data1 %>% select_at(names(data1) %>% setdiff(extras))
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

#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL

