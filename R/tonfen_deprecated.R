#' Check geographic integrety
#'
#' @description
#' \lifecycle{deprecated}
#'
#' Sanity check for areas of estimated tongfen correspondence. This is useful if for example the total extent
#' of geo1 and geo2 differ and there are regions at the edges with large difference in overlap.
#'
#' @param geo1 input geometry 1 of class sf
#' @param geo2 input geometry 2 of class sf
#' @param correspondence Correspondence table between `geo1` and `geo2` as e.g.
#' returned by `estimate_tongfen_correspondence`.
#' @return A table with columns `TongfenID`, `area1` and `area2`, where each row corresponds to a unique
#' `TongfenID` from them `correspondence` table and the other columns hold the areas of the regions
#' aggregated from `geo1` and `geo2`.`
check_tongfen_single_areas <- function(geo1,geo2,correspondence) {
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
    ungroup() %>%
    mutate(log_area_ratio = log(as.numeric(.data$area1)/as.numeric(.data$area2)))
}
