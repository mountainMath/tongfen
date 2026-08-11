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



# Connected components of the "rows sharing an identifier value" graph.
#
# Each row of `dd` is a node, two rows are adjacent if they agree on the value of
# at least one identifier column. Rather than materialising the (potentially huge)
# edge set, each row points at a parent row and we alternate between hooking every
# row onto the smallest parent in each of its identifier groups and pointer
# jumping to collapse the resulting chains. This computes connected components in
# a logarithmic number of fully vectorised passes and never allocates more than a
# handful of vectors of length `nrow(dd)`.
#
# `codes` is a list of integer vectors, one per identifier column, with 0 marking
# missing values (which must not link rows).
connected_components <- function(codes, n) {
  comp <- seq_len(n)
  repeat {
    changed <- FALSE
    # hook each row onto the smallest parent among the rows sharing one of its
    # identifier values
    for (cd in codes) {
      keep <- cd > 0L
      if (!any(keep)) next
      k <- cd[keep]
      current <- comp[keep]
      o <- order(k, current, method = "radix")
      ko <- k[o]
      first <- c(TRUE, ko[-1L] != ko[-length(ko)])
      group_min <- integer(max(k))
      group_min[ko[first]] <- current[o][first]
      new <- group_min[k]
      if (any(new != current)) {
        comp[keep] <- new
        changed <- TRUE
      }
    }
    # pointer jumping, halves the depth of every chain per pass
    repeat {
      jumped <- comp[comp]
      if (all(jumped == comp)) break
      comp <- jumped
      changed <- TRUE
    }
    if (!changed) break
  }
  comp
}

# smallest value of `values` within each group of `comp`, returned as a lookup
# vector indexed by component
group_first_sorted <- function(comp, values, k) {
  o <- order(comp, values, method = "radix")
  co <- comp[o]
  first <- c(TRUE, co[-1L] != co[-length(co)])
  out <- character(k)
  out[co[first]] <- values[o][first]
  out
}

get_tongfen_correspondence <- function(dd){
  hs <- names(dd)[!grepl("TongfenMethod",names(dd))]
  n <- nrow(dd)
  if (n == 0) {
    return(dd %>% mutate(TongfenID=character(0),TongfenUID=character(0)) %>% ungroup())
  }

  values <- lapply(hs, function(nn) as.character(dd[[nn]]))
  names(values) <- hs

  # dense integer codes per column, 0 for NA so that missing identifiers never
  # link two rows together
  codes <- lapply(values, function(v) {
    match(v, c(NA_character_, sort(unique(v[!is.na(v)])))) - 1L
  })

  comp <- connected_components(codes, n)
  k <- max(comp)

  # label each row by its first available identifier, later columns are prefixed
  # by their position so that identifiers from different columns cannot collide
  base <- values[[1]]
  if (anyNA(base) && length(hs) > 1) {
    for (i in seq(2, length(hs))) {
      missing <- is.na(base)
      if (!any(missing)) break
      v <- values[[i]][missing]
      base[missing] <- ifelse(is.na(v), NA_character_, paste0(i, "_", v))
    }
  }
  if (anyNA(base)) base[is.na(base)] <- paste0("row_", which(is.na(base)))

  # TongfenID is the smallest row label in the component
  dd$TongfenID <- group_first_sorted(comp, base, k)[comp]

  # TongfenUID enumerates all identifiers making up the component
  uid_parts <- lapply(hs, function(nn) {
    v <- values[[nn]]
    ok <- !is.na(v)
    cc <- comp[ok]
    vv <- v[ok]
    o <- order(cc, vv, method = "radix")
    cc <- cc[o]
    vv <- vv[o]
    keep <- c(TRUE, cc[-1L] != cc[-length(cc)] | vv[-1L] != vv[-length(vv)])
    collapsed <- vapply(split(vv[keep], cc[keep]), paste0, character(1), collapse = ",")
    out <- character(k)
    out[as.integer(names(collapsed))] <- paste0(nn, ":", collapsed)
    out
  })
  dd$TongfenUID <- do.call(paste, c(uid_parts, list(sep = " ")))[comp]

  dd %>% ungroup()
}

assert <- function (expr, error) {
  if (! expr) stop(error, call. = FALSE)
}


# Dissolve the geometries of `data` by `grouping_var`, the geometric equivalent
# of `summarize()`. Groups holding a single geometry - the bulk of the groups
# when tongfen-ing fine geographies - are passed through directly instead of
# being sent through `st_union()`, which is where most of the time in dissolving
# large geographies goes.
summarize_geometry_by_group <- function(data,grouping_var){
  geo_column <- attr(data,"sf_column")
  keys <- data %>% ungroup() %>% sf::st_drop_geometry() %>% select(all_of(grouping_var))
  key <- do.call(paste,c(unname(as.list(keys)),list(sep="\x1f")))
  u <- unique(key)
  u <- u[order(u,method="radix")]
  index <- match(key,u)
  geometry <- data[[geo_column]]

  counts <- tabulate(index,nbins=length(u))
  # first row belonging to each group, groups are in sorted key order
  first_row <- integer(length(u))
  first_row[rev(index)] <- rev(seq_along(index))

  result <- vector("list",length(u))
  singles <- counts==1L
  result[singles] <- geometry[first_row[singles]]
  multi <- which(counts>1L)
  if (length(multi)>0) {
    rows <- split(seq_along(index)[index %in% multi],index[index %in% multi])
    result[as.integer(names(rows))] <- lapply(rows,function(i)
      suppressMessages(sf::st_union(geometry[i]))[[1]])
  }

  out <- keys[first_row,,drop=FALSE]
  out[[geo_column]] <- sf::st_sfc(result,crs=sf::st_crs(geometry)) %>%
    sf::st_cast("MULTIPOLYGON")
  sf::st_sf(out,sf_column_name=geo_column)
}


# row-wise `paste0(unique(...),collapse=", ")` over a set of columns. Values are
# drawn from a small vocabulary, so the collapse is only computed once per
# distinct combination rather than once per row.
collapse_unique_by_row <- function(data,columns){
  sep <- "\x1f"
  key <- do.call(paste,c(unname(as.list(data[columns])),list(sep=sep)))
  u <- unique(key)
  collapsed <- vapply(strsplit(u,sep,fixed=TRUE),
                      function(x) paste0(unique(x),collapse=", "),
                      character(1))
  unname(collapsed[match(key,u)])
}

aggregate_correspondences <- function(correspondences){
  clean_correspondence_names <- function(correspondence) {
    correspondence %>%
      select(!matches("Tongfen") | matches("TongfenMethod"))
  }
  # compute full correspondence, smallest table first to keep intermediate
  # join results as small as possible
  index_order <- correspondences %>% lapply(nrow) %>% unlist() %>% order()

  correspondence <- correspondences[[index_order[1]]] %>%
    clean_correspondence_names()
  if (length(correspondences)>1) for (index in index_order[-1]) {
    c <- correspondences[[index]] %>%
      clean_correspondence_names()
    match_columns <- intersect(names(correspondence),names(c))
    match_columns <- match_columns[!grepl("TongfenMethod",match_columns)]
    correspondence <- inner_join(correspondence,c,by=match_columns) %>%
      unique()
  }

  method_columns <- names(correspondence)[grepl("TongfenMethod",names(correspondence))]
  correspondence$M  <- collapse_unique_by_row(correspondence,method_columns)
  correspondence %>% select(-all_of(method_columns)) %>%
    rename(TongfenMethod="M")
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

