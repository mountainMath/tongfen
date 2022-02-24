
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
#' @param na.rm remove NA values when aggregating, default is FALSE
#' @return `target` with estimated quantities from `source` as specified by `meta`
#' @export
#'
#' @examples
#' # Estimate 2006 Populatino in the City of Vancouver dissemination ares on 2016 census geoographies
#' \dontrun{
#' geo1 <- cancensus::get_census("CA06",regions=list(CSD="5915022"),geo_format='sf',level='DA')
#' geo2 <- cancensus::get_census("CA16",regions=list(CSD="5915022"),geo_format='sf',level='DA')
#' meta <- meta_for_additive_variables("CA06","Population")
#' result <- tongfen_estimate(geo2 %>% rename(Population_2016=Population),geo1,meta)
#'}
tongfen_estimate <- function(target,source,meta,na.rm=FALSE) {
  # make sure we only have polygon type geomtries
  target_geo_types <- target %>% sf::st_geometry_type() %>% unique
  source_geo_types <- source %>% sf::st_geometry_type() %>% unique
  if (length(setdiff(target_geo_types,c("MULTIPOLYGON","POLYGON")))>0) {
    warning(paste0("Target geometry has to be of type POLYGON or MULTIPOLYGON, but given target has types ",
                   paste0(target_geo_types,collapse=", "),". Dropping other geometries."))
    target <- target %>% sf::st_collection_extract("POLYGON")
  }
  if (length(setdiff(source_geo_types,c("MULTIPOLYGON","POLYGON")))>0) {
    warning(paste0("SOURCE geometry has to be of type POLYGON or MULTIPOLYGON, but given SOURCE has types ",
                   paste0(source_geo_types,collapse=", "),". Dropping other geometries."))
    source <- source %>% sf::st_collection_extract("POLYGON")
  }

  unique_key="...tongfen_row_number"
  target <- target %>% ungroup %>% mutate(!!unique_key:=row_number())

  meta <- meta %>%
    cut_meta(source,.) %>%
    mutate(var_name=paste0("v",row_number()))

  # rename variables, st_interpolate_aw does not handle column names with special characters
  safe_rename_vars <- setNames(meta$data_var,meta$var_name)
  safe_rename_back <- setNames(meta$var_name,meta$data_var)

  # do this manually for better control
  i = suppressMessages(st_intersection(st_geometry(source), st_geometry(target)))
  idx = attr(i, "idx")

  gc = which(st_is(i, "GEOMETRYCOLLECTION"))
  i[gc] = st_collection_extract(i[gc], "POLYGON")
  two_d = which(st_dimension(i) == 2)
  i[two_d] = st_cast(i[two_d], "MULTIPOLYGON")

  source <- source %>% rename(!!!safe_rename_vars)

  x_st <- source[idx[,1],, drop=FALSE] %>%
    select(names(safe_rename_vars)) %>%
    pre_scale(meta,meta_var = "var_name") %>%
    mutate(...area_st = st_area(i) %>% unclass,
           ...area_s = unclass(st_area(.))) %>%
    mutate(...factor = .data$...area_st/.data$...area_s) %>%
    mutate(...partial = .data$...factor < 0.99) %>%
    st_drop_geometry()


  for (var in meta$var_name) {
    # for (ci in naive_CI) {
    #   c <- ci/100
    #   x_st <- x_st %>%
    #     mutate(!!paste0(var,"_lower_",ci) := !!as.name(var) * ifelse(.data$...partial,(1-c) * .data$...factor, 1),
    #            !!paste0(var,"_upper_",ci) := !!as.name(var) * ifelse(.data$...partial,(1-c) * .data$...factor + c, 1))
    #
    # }
    x_st <- x_st %>%
      mutate(!!var:=!!as.name(var) * .data$...factor)
  }

  x_st <- stats::aggregate(x_st, list(idx[,2]), sum, na.rm=na.rm)

  result <- target %>%
    left_join(x_st %>%
                select(-.data$...factor,-.data$...partial,-.data$...area_s,-.data$...area_st) %>%
                rename(!!unique_key:="Group.1"),
              by=unique_key) %>%
    select(-all_of(unique_key)) %>%
    post_scale(meta,meta_var = "var_name") %>%
    rename(!!!safe_rename_back)

  # df = st_sf(x_st, geometry = st_geometry(target)[x_st$Group.1]) %>%
  #   select(-.data$...factor,-.data$...partial,-.data$...area_s,-.data$...area_st,-.data$Group.1) %>%
  #   st_set_agr("aggregate")

  # result <- sf::st_interpolate_aw(source %>%
  #                                   select(meta$data_var) %>%
  #                                   rename(!!!safe_rename_vars) %>%
  #                                   mutate_all(function(x)tidyr::replace_na(x,0)) %>%
  #                                   pre_scale(meta,meta_var = "var_name") ,
  #                                 target,
  #                                 extensive = TRUE) %>%
  #   mutate(!!unique_key:=target %>% pull(unique_key)) %>%
  #   #rename(!!unique_key:=.data$Group.1) %>%
  #   post_scale(meta,meta_var = "var_name") %>%
  #   left_join(target %>% sf::st_set_geometry(NULL),
  #             by=unique_key) %>%
  #   select(-one_of(unique_key)) %>%
  #   rename(!!!safe_rename_back)


  result
}



#' Tag regions by largest overlap
#'
#' @description
#' \lifecycle{maturing}
#'
#' tags regions in `source` by `target_id` of region in `target` with the largest overlap
#'
#' @param target custom geography
#' @param source input geography
#' @param target_id name of the column in `target` table with unique id (character)
#' @return `source` with extra column with name `"target_id"` and column `...overlap_fraction` with
#' the proportion of overlap of the target geometry with the respective `target_id`
#' @export
#'
#' @examples
#' # Estimate 2006 Populatino in the City of Vancouver dissemination ares on 2016 census geoographies
#' \dontrun{
#' geo1 <- cancensus::get_census("CA06",regions=list(CSD="5915022"),geo_format='sf',level='DA')
#' geo2 <- cancensus::get_census("CA16",regions=list(CSD="5915022"),geo_format='sf',level='DA')
#' meta <- meta_for_additive_variables("CA06","Population")
#' result <- tongfen_estimate(geo2 %>% rename(Population_2016=Population),geo1,meta)
#'}
tongfen_tag_largest_overlap <- function(source, target, target_id) {
  target_geo_types <- target %>% sf::st_geometry_type() %>% unique
  source_geo_types <- source %>% sf::st_geometry_type() %>% unique
  if (length(setdiff(target_geo_types,c("MULTIPOLYGON","POLYGON")))>0) {
    warning(paste0("Target geometry has to be of type POLYGON or MULTIPOLYGON, but given target has types ",
                   paste0(target_geo_types,collapse=", "),". Dropping other geometries."))
    target <- target %>% sf::st_collection_extract("POLYGON")
  }
  if (length(setdiff(source_geo_types,c("MULTIPOLYGON","POLYGON")))>0) {
    warning(paste0("SOURCE geometry has to be of type POLYGON or MULTIPOLYGON, but given SOURCE has types ",
                   paste0(source_geo_types,collapse=", "),". Dropping other geometries."))
    source <- source %>% sf::st_collection_extract("POLYGON")
  }

  if (target %>% pull(target_id) %>% duplicated() %>% sum() > 0) {
    stop("Values of target_id on target need to be unique")
  }


  i = suppressMessages(st_intersection(st_geometry(source), st_geometry(target)))
  idx = attr(i, "idx")


  x_st <- source[idx[,1],, drop=FALSE] %>%
    mutate(...area_st = st_area(i) %>% unclass,
           ...area_s = unclass(st_area(.))) %>%
    mutate(...overlap_fraction = .data$...area_st/.data$...area_s) %>%
    as_tibble() %>%
    bind_cols(setNames(as_tibble(idx[,1:2]),c("...source_row_number","...target_row_number"))) %>%
    left_join(target %>%
                mutate(...target_row_number=row_number()) %>%
                st_drop_geometry() %>%
                select(.data$...target_row_number,all_of(target_id)),
              by="...target_row_number") %>%
    select(-.data$...target_row_number,-.data$...area_st,-.data$...area_s) %>%
    group_by(.data$...source_row_number) %>%
    slice_max(order_by = .data$...overlap_fraction,n=1,with_ties = FALSE) %>%
    ungroup() %>%
    select(-.data$...source_row_number)

  x_st %>%
    st_sf()
}
