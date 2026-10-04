# Geocoding of census data varies over time, the same dwelling units can get assigned to
# different, neighbouring, regions in different years. In a timeline on a common geography
# this shows up as a surprising drop in one region that is offset by a jump in a neighbouring
# region. The functions in this file detect such patterns and join the affected regions,
# extending the idea behind TongFen from changing boundaries to data that got assigned across
# boundaries. See https://doodles.mountainmath.ca/posts/2024-07-26-geocoding-errors-in-aggregate-data/
# for background, the implementation follows the method described there.

anomaly_params <- function(rel_scale,abs_scale,p,surprise_cutoff,total_surprise_cutoff,
                           cutoff_fact,surprise_reduction_const,sum_fact) {
  params <- list(rel_scale=rel_scale,abs_scale=abs_scale,p=p,
                 surprise_cutoff=surprise_cutoff,total_surprise_cutoff=total_surprise_cutoff,
                 cutoff_fact=cutoff_fact,surprise_reduction_const=surprise_reduction_const,
                 sum_fact=sum_fact)
  valid <- vapply(params,\(x) is.numeric(x) && length(x)==1 && !is.na(x),logical(1))
  assert(all(valid),paste0("Need a single number for ",paste0(names(params)[!valid],collapse=", "),"."))
  assert(rel_scale>0 && abs_scale>0 && p>0,"rel_scale, abs_scale and p have to be positive.")
  params
}

# Surprise of a change in a count, on a scale from 0 to 1. Only decreases are surprising, a
# relative decrease of `rel_scale` and an absolute decrease of `abs_scale` each are half way
# to full surprise, and it takes both for a change to be surprising. The relative change is
# taken with respect to `base`, which does not have to be the count the change started from.
anomaly_surprise <- function(change,base,rel_scale,abs_scale) {
  decrease_surprise <- function(x,scale) {
    r <- x
    r[] <- 0
    decrease <- is.finite(x) & x<=0
    r[decrease] <- 1-0.5^(-x[decrease]/scale)
    r
  }
  decrease_surprise(change/base,rel_scale)*decrease_surprise(change,abs_scale)
}

# One round of looking for regions to join. `V` is a matrix of counts with a row per region
# and a column per year, `edges` a two column matrix with the row indices of neighbouring
# regions. Returns the candidate regions, most surprising first, with the neighbour that
# takes away most of the surprise when joined and if that is enough to join them.
anomaly_round <- function(V,edges,params) {
  nt <- ncol(V)
  change <- V[,-1,drop=FALSE]-V[,-nt,drop=FALSE]
  base <- V[,-nt,drop=FALSE]
  count_surprise <- \(S) rowSums(S>params$surprise_cutoff)
  total_surprise <- \(S) rowSums(S^params$p)^(1/params$p)

  S <- anomaly_surprise(change,base,params$rel_scale,params$abs_scale)
  count <- count_surprise(S)
  total <- total_surprise(S)

  index <- which(count>0 & total>params$total_surprise_cutoff)
  index <- index[order(-count[index],-total[index],-index)]
  result <- tibble(index=index,
                   surprise_count=as.integer(count[index]),
                   surprise_total=total[index],
                   period=max.col(S[index,,drop=FALSE],ties.method="first"),
                   neighbour=NA_integer_,
                   surprise_total_joined=NA_real_,
                   join=FALSE)

  from <- c(edges[,1],edges[,2])
  to <- c(edges[,2],edges[,1])
  keep <- from %in% index
  from <- from[keep]
  to <- to[keep]
  if (length(from)==0) return(result)

  # Joining a region with flat counts lowers the surprise just by growing the denominator
  # of the relative change. To keep the surprise comparable the change of the joined region
  # is measured against the counts of the candidate region alone.
  # A neighbour with unknown change can't make up for a surprising change.
  change_neighbour <- change[to,,drop=FALSE]
  change_neighbour[is.na(change_neighbour)] <- 0
  S_joined <- anomaly_surprise(change[from,,drop=FALSE]+change_neighbour,
                               base[from,,drop=FALSE],params$rel_scale,params$abs_scale)
  count_joined <- count_surprise(S_joined)
  total_joined <- total_surprise(S_joined)

  o <- order(from,count_joined,total_joined,to)
  best <- o[!duplicated(from[o])]
  best <- best[match(result$index,from[best])]

  result$neighbour <- to[best]
  result$surprise_total_joined <- total_joined[best]
  join <- result$surprise_total_joined < params$cutoff_fact*result$surprise_total |
    result$surprise_total-result$surprise_total_joined > params$surprise_reduction_const |
    result$surprise_total_joined < params$cutoff_fact*params$sum_fact*
    (result$surprise_total+total[result$neighbour])
  result$join <- !is.na(join) & join
  result
}

# Joins regions until there are no more pairs of regions left to join. Returns the group
# each region ended up in and the round in which it first got joined to another region.
anomaly_joins <- function(V,edges,params) {
  membership <- seq_len(nrow(V))
  round_joined <- rep(NA_integer_,nrow(V))
  round <- 0L
  repeat {
    candidates <- anomaly_round(V,edges,params)
    candidates <- candidates[candidates$join,]
    # regions can only be part of one join per round, the most surprising ones go first
    matched <- logical(nrow(V))
    target <- seq_len(nrow(V))
    for (r in seq_len(nrow(candidates))) {
      i <- candidates$index[r]
      j <- candidates$neighbour[r]
      if (matched[i] || matched[j]) next
      matched[c(i,j)] <- TRUE
      target[max(i,j)] <- min(i,j)
    }
    if (!any(matched)) break

    round <- round+1L
    round_joined[matched[membership] & is.na(round_joined)] <- round

    # contract the joined regions, the neighbours of a joined region are the neighbours
    # of the regions it is made up of
    target <- match(target,sort(unique(target)))
    membership <- target[membership]
    V <- rowsum(V,target)
    edges <- cbind(target[edges[,1]],target[edges[,2]])
    edges <- edges[edges[,1]!=edges[,2],,drop=FALSE]
    edges <- unique(cbind(pmin(edges[,1],edges[,2]),pmax(edges[,1],edges[,2])))
  }
  list(membership=membership,round=round_joined)
}

# Neighbouring regions as two column matrix of row indices into `ids`, each pair of
# neighbours is listed once.
anomaly_neighbours <- function(data,ids,neighbours) {
  if (is.null(neighbours)) {
    assert("sf" %in% class(data),
           "Need data of class sf to determine neighbouring regions, alternatively specify the neighbours.")
    geometry <- sf::st_geometry(data)
    pairs <- intersects_pairs(geometry,geometry)
    from <- pairs$row.id
    to <- pairs$col.id
  } else if (is.data.frame(neighbours)) {
    assert(ncol(neighbours)>=2,
           "Need neighbours to have two columns with the identifiers of neighbouring regions.")
    from <- match(as.character(neighbours[[1]]),ids)
    to <- match(as.character(neighbours[[2]]),ids)
  } else if (is.list(neighbours)) {
    # neighbours list like the ones from the spdep package, regions without neighbours hold a 0
    region_ids <- attr(neighbours,"region.id") %||% names(neighbours) %||% ids
    assert(length(region_ids)==length(neighbours),
           "Need neighbours to have an entry for each region.")
    from <- rep(seq_along(neighbours),lengths(neighbours))
    to <- as.integer(unlist(neighbours))
    keep <- !is.na(to) & to>0
    from <- match(as.character(region_ids)[from[keep]],ids)
    to <- match(as.character(region_ids)[to[keep]],ids)
  } else {
    stop("Don't know how to interpret neighbours, need a table with pairs of identifiers or a neighbours list.")
  }
  keep <- !is.na(from) & !is.na(to) & from!=to
  unique(cbind(pmin(from[keep],to[keep]),pmax(from[keep],to[keep])))
}

anomaly_input <- function(data,variables,id,neighbours) {
  assert(is.character(variables) && length(variables)>=2,
         "Need at least two variables making up the timeline to look for anomalies.")
  assert(is.character(id) && length(id)==1,"Need id to be the name of the identifier column.")
  missing_columns <- setdiff(c(id,variables),names(data))
  assert(length(missing_columns)==0,
         paste0("Did not find ",paste0(missing_columns,collapse=", ")," in data."))
  d <- data %>% ungroup()
  if ("sf" %in% class(d)) d <- d %>% sf::st_drop_geometry()
  not_numeric <- variables[!vapply(variables,\(v) is.numeric(d[[v]]),logical(1))]
  assert(length(not_numeric)==0,
         paste0("Variables have to be numeric, got ",paste0(not_numeric,collapse=", "),"."))
  id_values <- d[[id]]
  ids <- as.character(id_values)
  assert(!anyNA(ids) && !anyDuplicated(ids),
         paste0("Need ",id," to uniquely identify the regions in data."))

  V <- matrix(as.numeric(unlist(d[variables],use.names=FALSE)),ncol=length(variables))
  edges <- anomaly_neighbours(data,ids,neighbours)

  # Ties are broken by the order of the regions, sort by identifier so that the result
  # does not depend on the order the regions come in.
  o <- order(ids,method="radix")
  position <- order(o)
  edges <- cbind(position[edges[,1]],position[edges[,2]])
  list(V=V[o,,drop=FALSE],
       edges=cbind(pmin(edges[,1],edges[,2]),pmax(edges[,1],edges[,2])),
       ids=ids[o],
       id_values=id_values[o])
}


#' Detect likely geocoding anomalies in timelines on a common geography
#'
#' @description
#' \lifecycle{experimental}
#'
#' TongFen is only as good as the geocoding that assigned the underlying data to geographic regions
#' in the first place. Geocoding varies over time, and the same dwelling units, and the people living
#' in them, can get assigned to different neighbouring regions in different years. In a timeline
#' on a common geography this shows up as a surprising drop in one region that is offset by
#' a corresponding jump in a neighbouring region.
#'
#' This function lists the candidate regions with surprising drops in the given count variable,
#' together with the neighbouring region that takes away most of the surprise when both are joined.
#' Use it to check for possible problems and to calibrate the parameters before joining regions with
#' `tongfen_anomaly_joins`. Not all surprising drops are due to geocoding problems, a drop that is
#' not complemented by a neighbouring region is likely real.
#'
#' The surprise of a change between two consecutive years ranges from 0 to 1. Only decreases are surprising,
#' the surprise is the product of the surprise of the relative and of the absolute decrease, so that it takes
#' a decrease that is large in both relative and absolute terms to be surprising. The total surprise of a
#' region is the `p`-norm of the surprises across all changes in the timeline.
#'
#' A candidate region and its neighbour are flagged for joining if joining reduces the total surprise of the
#' candidate region to below `cutoff_fact` times its total surprise, or reduces it by more than
#' `surprise_reduction_const`, or reduces it to below `cutoff_fact * sum_fact` times the sum of
#' the total surprises of both regions. To keep this comparable the surprise after joining is computed
#' from the change of the joined regions relative to the counts of the candidate region alone.
#'
#' @param data data on a common geography, with one row per region, for example as returned by
#' `tongfen_aggregate` or `get_tongfen_ca_census`. Needs to be of class sf unless `neighbours` is specified.
#' @param variables names of the columns holding the timeline of a count variable like population or
#' dwellings, in temporal order. Changes from or to a missing value are not surprising and don't make up for
#' surprising changes in neighbouring regions, and joined regions are missing a value if one of the regions they
#' are made up of is. Replace missing values by zero beforehand if they stand for regions where nothing got counted
#' @param id name of the column that uniquely identifies the regions, default is "TongfenID"
#' @param neighbours optional, neighbouring regions as a table with the identifiers of pairs of neighbouring
#' regions in the first two columns, or as a neighbours list like the ones returned by `spdep::poly2nb`.
#' By default all regions with intersecting geometries are neighbours, which can miss neighbours
#' if the geometries have been simplified and don't share their boundaries any more.
#' @param rel_scale relative decrease that is half way to full surprise, default is `0.25` for a 25\% drop
#' @param abs_scale absolute decrease that is half way to full surprise, default is `200`
#' @param p exponent of the norm used to combine the surprises across the timeline into the total surprise.
#' Large values focus on the most surprising change, 1 adds up the surprises of all changes, default is `4`
#' @param surprise_cutoff changes with larger surprise count as surprising, default is `0.15`. Only
#' regions with at least one surprising change are candidates
#' @param total_surprise_cutoff only regions with larger total surprise are candidates, default is `0.75`
#' @param cutoff_fact join regions if the total surprise after joining is lower than this share of the
#' total surprise of the candidate region, default is `0.6`
#' @param surprise_reduction_const join regions if joining lowers the total surprise by more than this,
#' default is `0.15`
#' @param sum_fact join regions if the total surprise after joining is lower than `cutoff_fact * sum_fact`
#' times the sum of the total surprises of both regions, default is `0.7`
#' @return A tibble with one row for each candidate region, most surprising first, with the identifier of the
#' region, the number of surprising changes `surprise_count`, the total surprise `surprise_total`,
#' the `period` with the most surprising change, the identifier of the `neighbour` that takes away most of
#' the surprise, the total surprise `surprise_total_joined` after joining both and `join` indicating if both
#' regions qualify to get joined.
#' @export
#'
#' @examples
#' # Check 2001 through 2021 dissemination area level population timelines in the
#' # City of Vancouver for possible geocoding problems
#' \dontrun{
#' datasets <- c("CA01","CA06","CA11","CA16","CA21")
#' meta <- meta_for_additive_variables(datasets,"Population")
#' data <- get_tongfen_ca_census(regions=list(CSD="5915022"),meta=meta,level="DA",base_geo="CA21")
#'
#' anomalies <- tongfen_detect_anomalies(data,paste0("Population_",datasets))
#' }
tongfen_detect_anomalies <- function(data,variables,id="TongfenID",neighbours=NULL,
                                     rel_scale=0.25,abs_scale=200,p=4,
                                     surprise_cutoff=0.15,total_surprise_cutoff=0.75,
                                     cutoff_fact=0.6,surprise_reduction_const=0.15,sum_fact=0.7) {
  params <- anomaly_params(rel_scale,abs_scale,p,surprise_cutoff,total_surprise_cutoff,
                           cutoff_fact,surprise_reduction_const,sum_fact)
  input <- anomaly_input(data,variables,id,neighbours)
  candidates <- anomaly_round(input$V,input$edges,params)
  periods <- paste0(variables[-length(variables)],"-",variables[-1])

  tibble(!!id:=input$id_values[candidates$index],
         surprise_count=candidates$surprise_count,
         surprise_total=candidates$surprise_total,
         period=periods[candidates$period],
         neighbour=input$id_values[candidates$neighbour],
         surprise_total_joined=candidates$surprise_total_joined,
         join=candidates$join)
}


#' Determine regions to join to correct for likely geocoding anomalies
#'
#' @description
#' \lifecycle{experimental}
#'
#' Looks for regions with surprising drops in the timeline of a count variable that are complemented by
#' a neighbouring region, as explained in `tongfen_detect_anomalies`, and joins them. This gets
#' repeated on the joined regions until there are no more regions left that qualify to get joined.
#' In each round a region only gets joined with one other region, the most surprising regions go first.
#'
#' Joining regions trades geographic detail for timelines that are consistent over time. The parameters
#' control how aggressively regions get joined and are best calibrated on the data at hand, erring on the side of
#' joining too few regions risks keeping geocoding problems, erring on the other side risks removing real
#' changes and needlessly coarsens the geography.
#'
#' The result can be used to join the regions via `tongfen_join_regions`, or to update a correspondence
#' via `tongfen_join_correspondence`.
#'
#' @inheritParams tongfen_detect_anomalies
#' @return A tibble with one row for each region that gets joined with other regions, with the identifier
#' of the region, the identifier of the joined region it becomes part of in the column named like the
#' identifier with suffix `_joined`, by default `TongfenID_joined`, and the `round` in which the region
#' first got joined to another region. The identifier of a joined region is the smallest
#' identifier of the regions it is made up of.
#' @export
#'
#' @examples
#' # Correct 2001 through 2021 dissemination area level population timelines in the
#' # City of Vancouver for likely geocoding problems
#' \dontrun{
#' datasets <- c("CA01","CA06","CA11","CA16","CA21")
#' meta <- meta_for_additive_variables(datasets,"Population")
#' data <- get_tongfen_ca_census(regions=list(CSD="5915022"),meta=meta,level="DA",base_geo="CA21")
#'
#' joins <- tongfen_anomaly_joins(data,paste0("Population_",datasets))
#' corrected_data <- tongfen_join_regions(data,joins,meta)
#' }
tongfen_anomaly_joins <- function(data,variables,id="TongfenID",neighbours=NULL,
                                  rel_scale=0.25,abs_scale=200,p=4,
                                  surprise_cutoff=0.15,total_surprise_cutoff=0.75,
                                  cutoff_fact=0.6,surprise_reduction_const=0.15,sum_fact=0.7) {
  params <- anomaly_params(rel_scale,abs_scale,p,surprise_cutoff,total_surprise_cutoff,
                           cutoff_fact,surprise_reduction_const,sum_fact)
  input <- anomaly_input(data,variables,id,neighbours)
  result <- anomaly_joins(input$V,input$edges,params)

  membership <- result$membership
  joined <- tabulate(membership)[membership]>1
  # region with the smallest identifier in each group
  o <- order(membership,input$ids,method="radix")
  first <- !duplicated(membership[o])
  smallest <- integer(max(membership,0L))
  smallest[membership[o][first]] <- o[first]

  joins <- tibble(!!id:=input$id_values[joined],
                  !!paste0(id,"_joined"):=input$id_values[smallest[membership[joined]]],
                  round=result$round[joined])
  joins[order(input$ids[smallest[membership[joined]]],input$ids[joined],method="radix"),]
}


# Combine the TongfenUIDs of regions that get joined. A TongfenUID lists the identifiers
# making up a region as "<column>:<id>,<id> <column>:<id>".
merge_tongfen_uids <- function(uids) {
  uids <- unique(uids[!is.na(uids)])
  # the result must not depend on the order the regions come in
  uids <- uids[order(uids,method="radix")]
  parts <- unlist(strsplit(uids," ",fixed=TRUE))
  parts <- parts[nzchar(parts)]
  split <- regexpr(":",parts,fixed=TRUE)
  # not in the expected format, just string them together
  if (length(parts)==0 || any(split<2)) return(paste0(uids,collapse=" "))
  columns <- substr(parts,1,split-1)
  values <- strsplit(substring(parts,split+1),",",fixed=TRUE)
  vapply(unique(columns),function(column) {
    v <- unique(unlist(values[columns==column]))
    paste0(column,":",paste0(v[order(v,method="radix")],collapse=","))
  },character(1)) %>%
    paste0(collapse=" ")
}

# metadata for aggregating data that already has been aggregated to a common geography,
# where variables are named by their label
meta_for_joining_regions <- function(data,meta) {
  meta <- cut_meta(data,meta)
  # the same variable can be part of several datasets, look up parents within the dataset
  dataset <- if ("geo_dataset" %in% names(meta)) as.character(meta$geo_dataset) else rep("",nrow(meta))
  key <- paste(dataset,meta$variable,sep="\x1f")
  parent <- meta$data_var[match(paste(dataset,meta$parent,sep="\x1f"),key)]

  needs_parent <- meta$rule %in% c("Average","Median","AverageTo") & is.na(parent)
  if (any(needs_parent))
    stop(paste0("Can't join regions for ",paste0(unique(meta$data_var[needs_parent]),collapse=", "),
                ", the data does not have the parent variables needed to aggregate them. ",
                "Use `tongfen_join_correspondence` and `tongfen_aggregate` on the original data instead."),
         call.=FALSE)

  meta %>%
    mutate(variable=.data$data_var,parent=!!parent) %>%
    select(any_of(c("variable","rule","parent","units","type"))) %>%
    unique()
}


#' Join regions in data on a common geography
#'
#' @description
#' \lifecycle{experimental}
#'
#' Joins regions in data that has already been aggregated to a common geography, for example to correct
#' for likely geocoding anomalies as determined by `tongfen_anomaly_joins`. The data, and the geometries if
#' the data is of class sf, of the regions that get joined are aggregated, all other regions are left as they are.
#'
#' Variables are aggregated according to the metadata, numeric variables that are not part of the metadata
#' are assumed to be additive. Variables that are not additive, like averages, can only be aggregated if their
#' parent variable is part of the data. If that is not the case use `tongfen_join_correspondence` to update the
#' correspondence the data was built from and aggregate the original data again with `tongfen_aggregate`.
#'
#' @param data data on a common geography, with one row per region, for example as returned by
#' `tongfen_aggregate` or `get_tongfen_ca_census`
#' @param joins table with the regions to join as returned by `tongfen_anomaly_joins`, with the identifier
#' of the region and the identifier of the joined region it becomes part of in the column named like the
#' identifier with suffix `_joined`
#' @param meta optional metadata containing aggregation rules as for example returned by `meta_for_ca_census_vectors`,
#' variables are matched by their label. Numeric variables that are not part of the metadata are treated as
#' additive, if `NULL` (the default) that is the case for all numeric variables
#' @param id name of the column that uniquely identifies the regions, default is "TongfenID"
#' @param na.rm logical, determines how NA values should be treated when aggregating variables,
#' default is `TRUE`
#' @return The data with the regions joined. Joined regions take the place and the identifier of the
#' region with the smallest identifier among the regions they are made up of. Variables that are
#' not numeric and not part of the metadata are `NA` for joined regions.
#' @export
#'
#' @examples
#' # Correct 2001 through 2021 dissemination area level population timelines in the
#' # City of Vancouver for likely geocoding problems
#' \dontrun{
#' datasets <- c("CA01","CA06","CA11","CA16","CA21")
#' meta <- meta_for_additive_variables(datasets,"Population")
#' data <- get_tongfen_ca_census(regions=list(CSD="5915022"),meta=meta,level="DA",base_geo="CA21")
#'
#' joins <- tongfen_anomaly_joins(data,paste0("Population_",datasets))
#' corrected_data <- tongfen_join_regions(data,joins,meta)
#' }
tongfen_join_regions <- function(data,joins,meta=NULL,id="TongfenID",na.rm=TRUE) {
  joined_id <- paste0(id,"_joined")
  assert(id %in% names(data),paste0("Did not find ",id," in data."))
  assert(all(c(id,joined_id) %in% names(joins)),
         paste0("Need joins to have columns ",id," and ",joined_id,"."))
  data <- data %>% ungroup()
  ids <- as.character(data[[id]])
  assert(!anyNA(ids) && !anyDuplicated(ids),
         paste0("Need ",id," to uniquely identify the regions in data."))

  match_index <- match(ids,as.character(joins[[id]]))
  listed <- !is.na(match_index)
  if (!any(listed)) return(data)

  new_id_values <- data[[id]]
  new_id_values[listed] <- joins[[joined_id]][match_index[listed]]
  # regions that other regions get joined to are part of the join, even if not listed themselves
  affected <- listed | ids %in% as.character(new_id_values[listed])

  is_sf <- "sf" %in% class(data)
  geo_column <- if (is_sf) attr(data,"sf_column") else NULL
  joined <- data[affected,]
  joined[[id]] <- new_id_values[affected]
  new_ids <- as.character(joined[[id]])

  value_columns <- setdiff(names(data),c(id,"TongfenUID",geo_column))
  join_meta <- tibble(variable=character(0),rule=character(0),parent=character(0),type=character(0))
  if (!is.null(meta)) join_meta <- meta_for_joining_regions(joined,meta)
  # results of tongfen calls can have count variables that are not part of the metadata,
  # like the population, dwelling and household counts for Canadian census data
  additive_columns <- setdiff(value_columns,join_meta$variable)
  additive_columns <- additive_columns[vapply(additive_columns,\(v) is.numeric(joined[[v]]),logical(1))]
  if (length(additive_columns)>0) {
    message(paste0(ifelse(is.null(meta),"No metadata given, treating all numeric variables as additive: ",
                          "Treating numeric variables that are not part of the metadata as additive: "),
                   paste0(additive_columns,collapse=", ")))
    join_meta <- bind_rows(join_meta,
                           tibble(variable=additive_columns,rule="Additive",
                                  parent=NA_character_,type="Manual"))
  }
  dropped_columns <- setdiff(value_columns,join_meta$variable)
  if (length(dropped_columns)>0)
    message(paste0("Don't know how to aggregate ",paste0(dropped_columns,collapse=", "),
                   ", setting to NA for joined regions."))

  aggregated <- joined %>%
    select(all_of(c(id,intersect(join_meta$variable,names(joined)),geo_column))) %>%
    group_by(!!as.name(id)) %>%
    aggregate_data_with_meta(join_meta,na.rm=na.rm) %>%
    ungroup()

  if ("TongfenUID" %in% names(data)) {
    uids <- vapply(split(as.character(joined$TongfenUID),new_ids),merge_tongfen_uids,character(1))
    aggregated$TongfenUID <- unname(uids[as.character(aggregated[[id]])])
  }

  result <- bind_rows(data[!affected,],aggregated)
  # joined regions take the place of the region they got their identifier from
  result <- result[order(match(as.character(result[[id]]),ids)),names(data)]
  if (is_sf) {
    geometry_types <- unique(as.character(sf::st_geometry_type(result)))
    if (length(geometry_types)>1 && all(geometry_types %in% c("POLYGON","MULTIPOLYGON")))
      result <- sf::st_cast(result,"MULTIPOLYGON")
  }
  result
}


#' Join regions in a correspondence
#'
#' @description
#' \lifecycle{experimental}
#'
#' Updates a correspondence so that the given regions are joined, for example to correct for likely geocoding
#' anomalies as determined by `tongfen_anomaly_joins`. The updated correspondence can be used in `tongfen_aggregate`
#' to aggregate data on the coarser common geography, which works for all variables `tongfen_aggregate` can
#' deal with and for data that was not part of detecting the anomalies.
#'
#' @param correspondence correspondence table with columns the unique geographic identifiers for each of the
#' geographies and the TongfenID and TongfenUID, as for example returned by `estimate_tongfen_correspondence`
#' or `get_tongfen_correspondence_ca_census`
#' @param joins table with the regions to join as returned by `tongfen_anomaly_joins`, with columns
#' `TongfenID` and `TongfenID_joined`
#' @return The correspondence with updated TongfenID and TongfenUID for the regions that got joined. If the
#' correspondence has a TongfenMethod column "anomaly" gets added to the method of the regions that got joined.
#' @export
#'
#' @examples
#' # Correct for likely geocoding problems in dissemination area level population timelines
#' # and use the updated correspondence to aggregate data on the corrected common geography
#' \dontrun{
#' regions <- list(CSD="5915022")
#' datasets <- c("CA01","CA06","CA11","CA16","CA21")
#' meta <- meta_for_additive_variables(datasets,"Population")
#' data <- get_tongfen_ca_census(regions=regions,meta=meta,level="DA",base_geo="CA21")
#' joins <- tongfen_anomaly_joins(data,paste0("Population_",datasets))
#'
#' correspondence <- get_tongfen_correspondence_ca_census(geo_datasets=datasets,
#'                                                        regions=regions,level="DA") %>%
#'   tongfen_join_correspondence(joins)
#' }
tongfen_join_correspondence <- function(correspondence,joins) {
  assert("TongfenID" %in% names(correspondence),"Did not find TongfenID in correspondence.")
  assert(all(c("TongfenID","TongfenID_joined") %in% names(joins)),
         "Need joins to have columns TongfenID and TongfenID_joined.")
  ids <- as.character(correspondence$TongfenID)
  missing_regions <- setdiff(as.character(joins$TongfenID),ids)
  if (length(missing_regions)>0)
    warning(paste0("Did not find ",length(missing_regions)," of the regions to join in the correspondence."))

  match_index <- match(ids,as.character(joins$TongfenID))
  listed <- !is.na(match_index)
  if (!any(listed)) return(correspondence)

  ids[listed] <- as.character(joins$TongfenID_joined)[match_index[listed]]
  # regions that other regions get joined to are part of the join, even if not listed themselves
  affected <- listed | ids %in% ids[listed]
  new_ids <- ids[affected]
  correspondence$TongfenID[affected] <- new_ids
  if ("TongfenUID" %in% names(correspondence)) {
    uids <- vapply(split(as.character(correspondence$TongfenUID[affected]),new_ids),
                   merge_tongfen_uids,character(1))
    correspondence$TongfenUID[affected] <- unname(uids[new_ids])
  }
  if ("TongfenMethod" %in% names(correspondence)) {
    method <- correspondence$TongfenMethod[affected]
    tagged <- grepl("anomaly",method,fixed=TRUE)
    method[!tagged] <- paste0(method[!tagged],", anomaly")
    correspondence$TongfenMethod[affected] <- method
  }
  correspondence
}

#' @import dplyr
#' @importFrom rlang .data
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
