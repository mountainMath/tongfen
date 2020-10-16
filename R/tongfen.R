#' Generate tongfen metadata for additive variables
#'
#' @description
#' \lifecycle{maturing}
#'
#' Generates metadata to be used in tongfen_aggregate. Variables need to be additive like counts.
#'
#' @param dataset identifier for the dataset contianing the variable
#' @param variables (named) vecotor with additive variables
#' @return a tibble to be used in tongfen_aggregate
#' @export
meta_for_additive_variables <- function(dataset,variables){
  nn <- names(variables)
  variables <- as.character(variables)
  if (is.null(nn)) {
    nn <- paste0(variables,"_",dataset)
  } else {
    nn[nn==""]=paste0(variables,"_",dataset)[nn==""]
  }
  tibble(variable=variables,dataset=dataset, label=nn,type="Manual",
         aggregation="Additive",rule="Additive",geo_dataset=dataset,parent=NA)
}



#' Estimate variable values for custom geography
#'
#' @description
#' \lifecycle{maturing}
#'
#' Estimates data from source geometry onto target geometry
#'
#' @param target custom geography to estimate values for
#' @param source input geography with values
#' @param meta metadata for variable aggregation
#' @export
tongfen_estimate <- function(target,source,meta) {

  unique_key="tongfen_row_number"
  target <- target %>% mutate(!!unique_key:=row_number())

  meta <- meta %>%
    cut_meta(source,.) %>%
    mutate(var_name=paste0("v",row_number()))

  # rename variables, st_interpolate_aw does not handle column names with special characters
  safe_rename_vars <- setNames(meta$data_var,meta$var_name)
  safe_rename_back <- setNames(meta$var_name,meta$data_var)
  result <- sf::st_interpolate_aw(source %>%
                                    select(meta$data_var) %>%
                                    rename(!!!safe_rename_vars) %>%
                                    mutate_all(function(x)tidyr::replace_na(x,0)) %>%
                                    pre_scale(meta,meta_var = "data_var"),
                                  target,
                                  extensive = TRUE) %>%
    rename(!!unique_key:=.data$Group.1) %>%
    post_scale(meta,meta_var = "data_var") %>%
    left_join(target %>% sf::st_set_geometry(NULL),
              by=unique_key) %>%
    select(-one_of(unique_key)) %>%
    rename(!!!safe_rename_back)

  result
}

cut_meta <- function(data,meta){
  meta <- meta %>%
    filter(.data$variable %in% names(data)|.data$label %in% names(data)) %>%
    mutate(data_var=ifelse(.data$variable %in% names(data),.data$variable,.data$label))
}

pre_scale <- function(data,meta,meta_var="data_var",quiet=FALSE) {
  parent_lookup <- setNames(meta$parent,meta %>% pull(meta_var))
  to_scale <-  filter(meta,.data$rule %in% c("Median","Average")) %>% pull(meta_var)
  not_additive <- filter(meta,.data$rule == "Not additive") %>% pull(meta_var)
  median_vars <- filter(meta,.data$rule %in% c("Median")) %>% pull(meta_var)

  if(!quiet) {
    if (length(not_additive)>0)
      warning(paste0("Don't know how to TongFen: ",paste0(not_additive,collapse = ", ")))
    if (length(median_vars)>0)
      message(paste0("Can't TongFen medians, will approximate by treating as averages: ",paste0(median_vars,collapse = ", ")))
  }


  for (x in to_scale) {
    data <- data %>% mutate(!!x := !!as.name(x)*!!as.name(parent_lookup[x]))
  }

  data
}

post_scale <- function(data,meta,meta_var="data_var") {
  parent_lookup <- setNames(meta$parent,meta %>% pull(meta_var))
  to_scale <-  filter(meta,.data$rule %in% c("Median","Average")) %>% pull(meta_var)

  for (x in to_scale) {
    data <- data %>% mutate(!!x := !!as.name(x)/!!as.name(parent_lookup[x]))
  }

  data
}



#' Aggregate variables in grouped data
#'
#' @description
#' \lifecycle{maturing}
#'
#' Aggregate census data up, assumes data is grouped for aggregation
#' Uses data from meta to determine how to aggregate up
#'
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


rename_with_meta <- function(data,meta,ds=NULL){
  if (is.null(meta)) return(data)
  is_sf = 'sf' %in% class(data) # workaround for rename issues with sf
  m <- meta %>%
    filter(.data$variable %in% names(data))
  if (!is.null(ds)) m <- m %>% filter(.data$geo_dataset == ds)
  if (duplicated(m$variable) %>% sum > 0) stop("Duplicated variable names in metadata for same dataset.")
  data <- data %>%
    as_tibble() %>% # workaround for rename issues with sf
    rename(!!!setNames(m$variable,m$label))
  if (is_sf) data <- data %>% sf::st_sf()  # workaround for rename issues with sf
  data
}

#' Perform tongfen according to correspondence
#'
#' @description
#' \lifecycle{maturing}
#'
#' Aggregate variables secified in meta for several datasets according to correspondence.
#'
#' @param data list of datasets to be aggregated
#' @param correspondence correspondence data for gluing up the datasets
#' @param meta metadata containing aggregation rules as for example returned by `meta_for_ca_census_vectors`
#' @param base_geo identifier for which data element to base the final geography on,
#' uses the first data element if  `NULL` (default),
#' expects that `base_geo` is an element of `names(data)`.
#' @return aggregated dataset of class sf if base_geo is not NULL and data is of type sf or tibble otherwise.
#' @export
tongfen_aggregate <- function(data,correspondence,meta=NULL, base_geo = NULL){
  data <- ensure_names(data)
  nn <- names(data)
  if (is.null(base_geo)) base_geo <- nn[1]
  base_geo <- as.character(base_geo)

  # aggregate up data
  data_new <- nn %>%
    lapply(function(ds){
      d <- data[[ds]]
      if (base_geo != ds && ("sf" %in% class(d))) {
        d <- d %>% sf::st_drop_geometry()
      }
      match_column <- intersect(names(d),names(correspondence))
      if (length(match_column)==0) stop("Did not found matching geographic identifiers.")
      if (length(match_column)>1) warning(paste0("Matching over several geographic identifiers: ",paste0(match_column,collapse=", ")))
      c <- correspondence %>%
        select_at(c(match_column,"TongfenID","TongfenUID")) %>%
        unique()
      cd <- c %>% filter(duplicated(!!as.name(match_column))) # sanity check
      assert(nrow(cd)==0,"Problem in tongfen_aggregate, have more than one TongFenID for some GeoUID")
      d<-d %>%
        inner_join(c,
                   by=match_column) %>%
        group_by(.data$TongfenID,.data$TongfenUID)
      if (!is.null(meta)) {
        d <- d %>%  aggregate_data_with_meta(meta)
      } else {
        d <- d %>% summarize()
      }
    }) %>%
    setNames(nn)

  aggregated_data <- data_new[[base_geo]] %>%
    rename_with_meta(meta,base_geo)

  for (ds in nn[nn!=base_geo]) {
    aggregated_data <- aggregated_data %>%
      inner_join(data_new[[ds]] %>%
                   select(-.data$TongfenUID) %>%
                   rename_with_meta(meta,ds),
                 by="TongfenID")
  }

  if (!is.null(meta)) {
    extras <- meta %>%
      filter(.data$type=="Extra") %>%
      filter(.data$label %in% names(aggregated_data))
    aggregated_data <- aggregated_data %>%
      select(-one_of(extras$label))
  }
  aggregated_data %>%
    ungroup()
}



#' Dasymetric downsampling
#'
#' @description
#' \lifecycle{maturing}
#'
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

#' Generate togfen correspondence for two geographies
#'
#' @description
#' \lifecycle{maturing}
#'
#' Get correspondence data for arbitrary congruent geometries. Congruent means that one can obtain a common
#' tiling by aggregating several sub-geometries in each of the two input geo data. Worst case scenario the
#' only common tiling is given by unioning all sub-geometries and there is no finer common tiling.
#'
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
estimate_tongfen_single_correspondence <- function(geo1,geo2,geo1_uid,geo2_uid,
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
    as.data.frame() %>%
    as_tibble() %>%
    rename(id1=.data$row.id,id2=.data$col.id) %>%
    left_join(id1,by="id1") %>%
    left_join(id2,by="id2") %>%
    select(-id1,-id2)
  i2 <- cgeo2 %>%
    st_intersects(geo1,sparse = TRUE) %>%
    as.data.frame() %>%
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

#' Generate togfen correspondence for list of geographies
#'
#' @description
#' \lifecycle{maturing}
#'
#' Get correspondence data for arbitrary congruent geometries. Congruent means that one can obtain a common
#' tiling by aggregating several sub-geometries in each of the two input geo data. Worst case scenario the
#' only common tiling is given by unioning all sub-geometries and there is no finer common tiling.
#'
#' @param data list of geometries of class sf
#' @param geo_identifiers vector of unique geographic identifiers for each list entry in data.
#' @param method aggregation method. Possible values are "estimate" or "identifier". "estimate" estimates the
#' correspondence purely from the geographic data. "identifier" assumes that regions with identical geo_identifiers are the same,
#' and uses the "estimate" method for the remaining regions. Default is "estimate".
#' @param tolerance tolerance (in projected coordinate units of `computation_crs`) for feature matching
#' @param computation_crs optional crs in which the computation should be carried out,
#' defaults to crs of the first entry in the data parameter.
#' @return A correspondence table linking geo1_uid and geo2_uid with unique TongfenID and TongfenUID columns
#' that enumerate the common geometry.
#' @export
estimate_tongfen_correspondence <- function(data,
                                            geo_identifiers,
                                            method = "estimate",
                                            tolerance = 50,
                                            computation_crs = NULL){
  if (is.null(computation_crs)) computation_crs = sf::st_crs(data[[1]])
  assert(length(geo_identifiers) == length(unique(geo_identifiers)), "geo_identifiers need to be unique.")
  assert(length(geo_identifiers) == length(data), "data and geo_identifiers need to have the same legnth.")

  # assume regions with identical geo_identifiers are identical
  if (method=="identifier") {
    common_ids <- data[[1]] %>% pull(geo_identifiers[1])
    for (index in seq(2,length(data))) {
      common_ids <- intersect(common_ids,data[[index]] %>% pull(geo_identifiers[index]))
      # base correspondence data for common ids
      base_corresondence <- seq(2,length(data)) %>%
        lapply(function(index2){
          gu1 <- geo_identifiers[index2-1]
          gu2 <- geo_identifiers[index2]
          tibble(!!as.name(gu1):=common_ids,!!as.name(gu2):=common_ids,TongfenID=common_ids) %>%
            mutate(TongfenUID=paste0(gu1,":",!!as.name(gu1)," ",gu2,":",!!as.name(gu2)))
        })
    }
  } else {
    common_ids=c()
  }


  # estimate coorrespondence data from tongfen

  mismatch_correspondences <- seq(2,length(data)) %>%
    lapply(function(index){
      gu1 <- geo_identifiers[index-1]
      g1 <- data[[index-1]] %>% filter(!(!!as.name(gu1) %in% common_ids))
      gu2 <- geo_identifiers[index]
      g2 <- data[[index]] %>% filter(!(!!as.name(gu2) %in% common_ids))

      c <- estimate_tongfen_single_correspondence(g1,g2,gu1,gu2,tolerance=tolerance,computation_crs=computation_crs)
    })

  if (method=="identifier") {
    # merge base and tongfen correspondence data
    correspondence_list <- seq(1,length(base_corresondence)) %>%
      lapply(function(index){
        bind_rows(base_corresondence[[index]] %>% mutate(TongfenMethod="identifier"),
                  mismatch_correspondences[[index]] %>% mutate(TongfenMethod="estimate"))
      })
  } else {
    correspondence_list <- mismatch_correspondences %>%
      lapply(function(c) c %>% mutate(TongfenMethod="estimate"))
  }

  correspondence_list  %>%
    aggregate_correspondences() %>%
    get_tongfen_correspondence()
}



#' Check geographic integrety
#'
#' @description
#' \lifecycle{maturing}
#'
#' Sanity check for areas of estimated tongfen correspondence. This is useful if for example the total extent
#' of geo1 and geo2 differ and there are regions at the edges with large difference in overlap.
#'
#' @param data alist of geogrpahic data of class sf
#' @param correspondence Correspondence table with columns the unique geographic identifiers for each of the
#' geographies and the TongfenID (and optionally TongfenUID and TongfenMethod)
#' returned by `estimate_tongfen_correspondence`.
#' @return A table with columns `TongfenID`, geo_identifiers, the areas of the aggregated regions
#' corresponding to each geographic identifier column, the tongfen estimation method and the maximum log ratio
#' of the areas.
#' @export
check_tongfen_areas <- function(data,correspondence) {
  if (!("TongfenMethod" %in% names(correspondence))) {
    correspondence$TongfenMethod <- "unknown"
  }
  data <- ensure_names(data)
  nn <- names(data)
  default_columns <- c("TongfenID","TongfenMethod")
  summary_data <- names(data) %>%
    lapply(function(nn){
      d=data[[nn]]
      match_column <- intersect(names(d),names(correspondence))
      area_column <- paste0("area_",nn)
      dd <- d %>%
        mutate(!!area_column:=sf::st_area(.)) %>%
        sf::st_set_geometry(NULL) %>%
        select_at(c(match_column,area_column)) %>%
        inner_join(correspondence %>%
                     select_at(c(match_column,default_columns)) %>%
                     unique(),
                   by=match_column) %>%
        group_by(.data$TongfenID) %>%
        summarize(!!area_column:=sum(!!as.name(area_column)),
                  TongfenMethod=paste0(unique(.data$TongfenMethod),collapse=", "),
                  .groups = "drop")
    }) %>%
    purrr::reduce(full_join,by="TongfenID")

  method_columns <- names(summary_data)[grepl("TongfenMethod",names(summary_data))]
  summary_data$M  <- apply(summary_data[,method_columns],1,function(d)paste0(unique(d),collapse = ", "))
  summary_data %>%
    select(-method_columns) %>%
    rename(TongfenMethod=.data$M) %>%
    mutate(maxa=apply(select(.,matches("^area_")),1,max),
           mina=apply(select(.,matches("^area_")),1,min)) %>%
    mutate(max_log_ratio=log(.data$maxa/.data$mina)) %>%
    select(-.data$maxa,-.data$mina)
}


#' A dataset with polling station votes data from the 2015 federal election in the Vancouver area
#' @name vancouver_elections_data_2015
#' @docType data
#' @author  Elections Canada
#' @references \url{https://www.elections.ca/content.aspx?section=res&dir=rep/off&document=index&lang=e#42GE}
#' @keywords data
NULL

#' A dataset with polling district geographies from the 2015 federal election in the Vancouver area
#' @name vancouver_elections_geos_2015
#' @docType data
#' @author  Elections Canada
#' @references \url{https://www.elections.ca/content.aspx?section=res&dir=rep/off&document=index&lang=e#42GE}
#' @keywords data
NULL

#' A dataset with polling station votes data from the 2019 federal election in the Vancouver area
#' @name vancouver_elections_data_2019
#' @docType data
#' @author Elections Canada
#' @references \url{https://www.elections.ca/content.aspx?section=res&dir=rep/off&document=index&lang=e#43GE}
#' @keywords data
NULL

#' A dataset with polling district geographies from the 2019 federal election in the Vancouver area
#' @name vancouver_elections_geos_2019
#' @docType data
#' @author Elections Canada
#' @references \url{https://www.elections.ca/content.aspx?section=res&dir=rep/off&document=index&lang=e#43GE}
#' @keywords data
NULL

#' @import dplyr
#' @import rlang
#' @import sf
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

