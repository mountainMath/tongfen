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




#' Aggregate census data up, assumes data is grouped for aggregation
#' Uses data from meta to determine how to aggregate up
#' @param data census data as obtained from get_census call, grouped by TongfenID
#' @param meta list with variables and aggregation information as obtained from meta_for_vectors
#' @param geo logical, should also aggregate geographic data
#' @param na.rm logical, should NA values be ignored or carried through.
#' @param quiet logical, don't emit messages if set to `TRUE`
#' @return data frame with variables aggregated to new common geography
#' @export
aggregate_data_with_meta <- function(data,meta,geo=FALSE,na.rm=TRUE,quiet=FALSE){
  meta <- meta %>% filter(.data$variable %in% names(data))
  grouping_var=groups(data) %>% as.character
  parent_lookup <- setNames(meta$parent,meta$variable)
  to_scale <-  filter(meta,.data$rule %in% c("Median","Average"))$variable
  to_scale_from <- filter(meta,.data$rule %in% c("AverageTo"))$variable
  not_additive <- filter(meta,.data$rule == "Not additive")$variable
  median_vars <- filter(meta,.data$rule %in% c("Median"))$variable

  if(!quiet) {
    if (length(not_additive)>0)
      warning(paste0("Don't know how to TongFen: ",paste0(not_additive,collapse = ", ")))
    if (length(median_vars)>0)
      message(paste0("Can't TongFen medians, will approximate by treating as averages: ",paste0(median_vars,collapse = ", ")))
  }

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
                        #sf::st_cast("MULTIPOLYGON") %>%
                        summarize(!!geo_column:=sf::st_union(!!as.name(geo_column)) %>%
                                    sf::st_cast("MULTIPOLYGON")),
                      data %>%
                        sf::st_set_geometry(NULL) %>%
                        summarize_at(meta$variable,sum,na.rm=na.rm),
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


#' Get correspondence data for arbitrary congruent geometries. Congruent means that one can obtain a common
#' tiling by aggregating several sub-geometries in each of the two input geo data. Worst case scenario the
#' only common tiling is given by unioning all sub-geometries and there is no finer common tiling.
#' @param geo1 input geometry 1 of class sf
#' @param geo2 input geometry 2 of class sf
#' @param geo1_uid (unique) identifier column for geo1
#' @param geo2_uid (unique) identifier column for geo2
#' @param tolerance tolerance (in projected coordinate units) for feature matching
#' @param computation_crs optional crs in which the computation should be carried out,
#' defaults to crs of geo1
#' @param robust boolean parameter, will ensure geometries are valid if set to TRUE
#' @return A correspondence table linking geo1_uid and geo2_uid with unique TongfenID and TongfenUID columns
#' that enumerate the common geometry.
#' @export
estimate_tongfen_correspondence <- function(geo1,geo2,geo1_uid,geo2_uid,
                                            tolerance=1,
                                            computation_crs=NULL,
                                            robust=FALSE){
  if (geo1_uid==geo2_uid) stop("geo1_uid and geo2_uid can't be the same! Please rename one of the identifier columns.")

  if (geo1 %>% filter(is.na(!!as.name(geo1_uid))) %>% nrow() > 0) stop("Found NA values for some geo1_uid, please make sure that there are no NA values.")
  if (geo2 %>% filter(is.na(!!as.name(geo2_uid))) %>% nrow() > 0) stop("Found NA values for some geo2_uid, please make sure that there are no NA values.")

  if (!is.null(computation_crs)) {
    geo1 <- geo1 %>% st_transform(computation_crs)
    geo2 <- geo2 %>% st_transform(computation_crs)
  } else if (st_crs(geo1)!=st_crs(geo2)) { # make sure same crs
    geo2 <- geo2 %>% st_transform(st_crs(geo1))
  }

  geo1 <- geo1 %>% ungroup()
  geo2 <- geo2 %>% ungroup()

  id1 <- geo1 %>% st_set_geometry(NULL) %>% select(!!geo1_uid) %>% mutate(id1=row_number())
  id2 <- geo2 %>% st_set_geometry(NULL) %>% select(!!geo2_uid) %>% mutate(id2=row_number())


  if (robust) {
    if (!st_is_valid(geo1)) geo1 <- geo1 %>% st_make_valid()
    if (!st_is_valid(geo2)) geo2 <- geo2 %>% st_make_valid()
  }


  robust_tolerance_buffer <- function(geo,geo_uid,tolerance,max_tries=20) {
    t <- tolerance
    d <- geo
    d$geometry=st_buffer(geo$geometry,-t)
    count=0
    empties <- st_is_empty(d)
    while (sum(empties) > 0 & count<max_tries) {
      t <- t/2
      d[empties,]$geometry=st_buffer(geo$geometry[empties],-t)
      empties <- st_is_empty(d)
      count <- count + 1
    }
    if (sum(empties) > 0) {
      stop("Unable to match within given tolerance, some geographies are too fine.")
    }
    d
  }

  cgeo1 <- geo1 %>% robust_tolerance_buffer(geo_uid = geo1_uid,tolerance = tolerance)
  cgeo2 <- geo2 %>% robust_tolerance_buffer(geo_uid = geo2_uid,tolerance = tolerance)

  i1 <- cgeo1 %>%
    st_intersects(geo2,sparse = TRUE) %>%
    as_tibble() %>%
    rename(id1=.data$row.id,id2=.data$col.id) %>%
    left_join(id1,by="id1") %>%
    left_join(id2,by="id2") %>%
    select(-id1,-id2)
  i2 <- cgeo2 %>%
    st_intersects(geo1,sparse = TRUE) %>%
    as_tibble() %>%
    rename(id2=.data$row.id,id1=.data$col.id) %>%
    left_join(id1,by="id1") %>%
    left_join(id2,by="id2") %>%
    select(-id1,-id2)

  correspondence <- bind_rows(i1,i2) %>%
    unique() %>%
    get_tongfen_correspondence()

  correspondence
}


#' Sanity check for areas of estimated tongfen correspondence. This is useful if for example the total extent
#' of geo1 and geo2 differ and there are regions at the edges with large difference in overlap.
#' @param geo1 input geometry 1 of class sf
#' @param geo2 input geometry 2 of class sf
#' @param correspondence Correspondence table between `geo1` and `geo2` as e.g.
#' returned by `estimate_tongfen_correspondence`.
#' @return A table with columns `TongfenID`, `area1` and `area2`, where each row corresponds to a unique
#' `TongfenID` from them `correspondence` table and the other columns hold the areas of the regions
#' aggregated from `geo1` and `geo2`.`
#' @export
check_tongfen_areas <- function(geo1,geo2,correspondence) {
  geo1_uid <- names(correspondence)[1]
  geo2_uid <- names(correspondence)[2]
  if (!(geo1_uid %in% names(geo1))) {
    g <- geo1_uid
    geo1_uid <- geo2_uid
    geo2_uid <- g
  }
  if (!(geo1_uid %in% names(geo1))) {
    stop("Can't match correspondence to geo1.")
  }
  if (!(geo2_uid %in% names(geo2))) {
    stop("Can't match correspondence to geo2.")
  }
  d1<-geo1 %>%
    mutate(area=st_area(.)) %>%
    st_set_geometry(NULL) %>%
    inner_join_tongfen_correspondence(correspondence,geo1_uid) %>%
    group_by(.data$TongfenID) %>%
    summarize(area1=sum(.data$area))
  d2 <- geo2 %>%
    mutate(area=st_area(.)) %>%
    st_set_geometry(NULL) %>%
    inner_join_tongfen_correspondence(correspondence,geo2_uid) %>%
    group_by(.data$TongfenID) %>%
    summarize(area2=sum(.data$area))

   inner_join(d1,d2,by="TongfenID") %>%
     ungroup()
}


#' @import dplyr
#' @import rlang
#' @import sf
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

