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
  hs <- names(dd)[!grepl("TongfenMethod",names(dd))]
  index = 1
  ddd<- dd %>%
    mutate(TongfenID=!!as.name(hs[index]))

  while (index<length(hs) && filter(ddd,is.na(.data$TongfenID)) %>% nrow > 0) {
    ddd<- ddd %>%
      mutate(TongfenID=coalesce(.data$TongfenID,paste0(index,"_",!!as.name(hs[index]))))
    index <- index + 1
  }

  # Optimized connected components using union-find approach
  # Build a mapping of all unique IDs to their component root
  # This is much faster than repeated group_by operations

  # Create union-find parent mapping
  all_ids <- unique(ddd$TongfenID)
  parent <- setNames(all_ids, all_ids)

  # Find root with path compression
  find_root <- function(x) {
    if (parent[x] == x) return(x)
    parent[x] <<- find_root(parent[x])  # Path compression
    return(parent[x])
  }

  # Union two components
  union_ids <- function(x, y) {
    root_x <- find_root(x)
    root_y <- find_root(y)
    if (root_x != root_y) {
      # Always attach to the smaller ID (alphabetically)
      if (root_x < root_y) {
        parent[root_y] <<- root_x
      } else {
        parent[root_x] <<- root_y
      }
    }
  }

  # For each identifier column, union all IDs that share the same identifier value
  for (nn in hs) {
    id_groups <- ddd %>%
      select(identifier = !!as.name(nn), "TongfenID") %>%
      distinct() %>%
      group_by(.data$identifier) %>%
      summarise(ids = list(.data$TongfenID), .groups = "drop")

    for (i in seq_len(nrow(id_groups))) {
      ids_in_group <- id_groups$ids[[i]]
      if (length(ids_in_group) > 1) {
        # Union all pairs in this group
        for (j in 2:length(ids_in_group)) {
          union_ids(ids_in_group[1], ids_in_group[j])
        }
      }
    }
  }

  # Apply the final mapping - find root for each ID
  final_mapping <- setNames(
    vapply(all_ids, find_root, character(1), USE.NAMES = FALSE),
    all_ids
  )

  # Map all TongfenIDs to their roots
  ddd <- ddd %>%
    mutate(TongfenID = final_mapping[.data$TongfenID])

  # Vectorized UID generation
  # Pre-compute all the grouped values at once
  uid_parts <- ddd %>%
    group_by(.data$TongfenID) %>%
    summarise(
      across(
        all_of(hs),
        ~paste0(cur_column(), ":", paste0(sort(unique(.x)), collapse = ",")),
        .names = "uid_{.col}"
      ),
      .groups = "drop"
    ) %>%
    mutate(
      TongfenUID = do.call(paste, c(select(., starts_with("uid_")), sep = " "))
    ) %>%
    select("TongfenID", "TongfenUID")

  # Join the UIDs back
  ddd %>%
    left_join(uid_parts, by = "TongfenID") %>%
    ungroup()
}

assert <- function (expr, error) {
  if (! expr) stop(error, call. = FALSE)
}


aggregate_correspondences <- function(correspondences){
  clean_correspondence_names <- function(correspondence) {
    correspondence %>%
      select(!matches("Tongfen") | matches("TongfenMethod"))
  }
  # compute full correspondence
  # order by length to speed up the process
  lengths <- correspondences %>% lapply(nrow) %>% unlist %>% rank(ties.method = "first")

  correspondence <- correspondences[[lengths[1]]] %>%
    clean_correspondence_names()
  if (length(correspondences)>1) for (index in lengths[-1]) {
    c <- correspondences[[index]] %>%
      clean_correspondence_names()
    match_columns <- intersect(names(correspondence),names(c))
    match_columns <- match_columns[!grepl("TongfenMethod",match_columns)]
    correspondence <- inner_join(correspondence,c,by=match_columns) %>%
      unique()
  }

  method_columns <- names(correspondence)[grepl("TongfenMethod",names(correspondence))]
  correspondence$M  <- apply(correspondence[,method_columns],1,function(d)paste0(unique(d),collapse = ", "))
  correspondence %>% select(-method_columns) %>%
    rename(TongfenMethod=.data$M)
}


normalize_datasets <- function(geo_datasets) {
  geo_datasets <- as.character(geo_datasets)
  dataset_translation <- setNames(
    c("CA21","CA16","CA11","CA06","CA01") %>% rev(),
    as.character(seq(2001,2021,5)))
  geo_datasets <- geo_datasets %>% dplyr::recode(!!!dataset_translation)
  geo_datasets
}


ensure_names <- function(list,default_names=seq(1,length(list))){
  nn <- names(list)
  if (is.null(nn)) {
    nn=default_names
  } else {
    nn[nn==""]=default_names[nn==""]
  }
  names(list)=nn
  list
}

#' @import dplyr
#' @importFrom stats setNames
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

