% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tongfen_ca_deprecated.R
\name{get_tongfen_census_da}
\alias{get_tongfen_census_da}
\title{Canadian Census DA level tongfen}
\usage{
get_tongfen_census_da(
  regions,
  vectors,
  geo_format = NA,
  use_cache = TRUE,
  na.rm = TRUE,
  quiet = TRUE
)
}
\arguments{
\item{regions}{census region list, should be inclusive list of GeoUIDs across censuses}

\item{vectors}{List of cancensus vectors, can come from different census years}

\item{geo_format}{`NA` to only get the variables or 'sf' to also get geographic data}

\item{use_cache}{logical, passed to `cancensus::get_census` to regulate caching}

\item{na.rm}{logical, determines how NA values should be treated when aggregating variables}

\item{quiet}{suppress download progress output, default is `TRUE`}
}
\value{
dataframe with variables on common geography
}
\description{
\lifecycle{deprecated}

Grab variables from several censuses on a common geography. Requires sf package to be available
Will return CT level data
}
