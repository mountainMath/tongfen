nullify_blank <- function(x){
  if (!is.null(x)) {
    if (is.na(x)) x=NULL else {
      if (x=="") x=NULL
      }
  }
  x
}

tongfen_cache_dir <- function(){
  nullify_blank(getOption("tongfen.cache_path")) %||%
    nullify_blank(Sys.getenv("tongfen.cache_path")) %||%
    nullify_blank(getOption("custom_data_path")) %||%
    tempdir()
}

inner_join_tongfen_correspondence <- function(data,correspondence,link){
  data %>%
    inner_join(correspondence %>%
                 select("TongfenID","TongfenUID",link) %>%
                 unique(),
               by=link)
}



get_tongfen_correspondence <- function(dd){
  hs <- names(dd)
  ddd<- dd %>%
    mutate(TongfenID=!!as.name(hs[1]))

  done_tongfen <- FALSE
  iterations <- 0
  while (!done_tongfen) {
    ddd <- ddd %>%
      mutate(TongfenIDOriginal=.data$TongfenID)
    for (nn in hs) {
      ddd <- ddd %>%
        group_by(!!as.name(nn)) %>%
        mutate(TongfenID=min(.data$TongfenID))
    }
    done_tongfen <- ddd %>% filter(.data$TongfenID!=.data$TongfenIDOriginal) %>% nrow == 0
    iterations <- iterations+1
  }

  ddd <- ddd %>% select(-.data$TongfenIDOriginal)

  tongfen_groups <- unique(ddd$TongfenID)
  grp_lookup <- setNames(seq(1,length(tongfen_groups)),tongfen_groups)

  ddd <- ddd %>%
    group_by(.data$TongfenID) %>%
    mutate(TongfenUID=paste0(hs[1],":",paste0(sort(unique(!!as.name(hs[1]))),collapse=",")))
  for (nn in hs[-1]) {
    ddd <- ddd %>%
      mutate(TongfenUID=paste0(.data$TongfenUID," ",nn,":",paste0(sort(unique(!!as.name(nn))),collapse=",")))
  }
  ddd %>%
    ungroup()
}


#' @import dplyr
#' @importFrom stats setNames
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

