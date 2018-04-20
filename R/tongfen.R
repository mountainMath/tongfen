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
}


#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL

