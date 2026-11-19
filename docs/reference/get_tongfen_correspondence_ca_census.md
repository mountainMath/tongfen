# Get StatCan correspondence data

**\[maturing\]**

Get correspondence file for several Canadian censuses on a common
geography. Requires sf and cancensus package to be available

## Usage

``` r
get_tongfen_correspondence_ca_census(
  geo_datasets,
  regions,
  level = "CT",
  method = "statcan",
  links = c("statcan", "adjusted"),
  tolerance = 50,
  quiet = FALSE,
  refresh = FALSE,
  crs = 3347
)
```

## Arguments

- geo_datasets:

  vector of census geography dataset identifiers

- regions:

  census region list, should be inclusive list of GeoUIDs across
  censuses

- level:

  aggregation level to return data on (default is "CT"), "EA" works as
  an alias for "DA"

- method:

  tongfen method, options are "statcan" (the default), "estimate",
  "identifier". \* "statcan" method builds up the common geography using
  Statistics Canada correspondence files, at this point this method only
  works for "DB", "DA" and "CT" levels, and for the censuses from 1981
  through 2021. Up to 1996 the smallest census geography were
  enumeration areas (EA), at the "DA" level these take the place of
  dissemination areas for those censuses. The "DB" level only works for
  the censuses from 2001 on. \* "estimate" uses
  \`estimate_tongfen_correspondence\` to build up the common geography
  from scratch based on geographies. \* "identifier" assumes regions
  with identical geographic identifier are identical, and builds up the
  the correspondence for regions with unmatched geographic identifiers.

  The geographies of the 1981 and 1986 censuses are incomplete, these
  censuses only work with the "statcan" method.

- links:

  which version of the StatCan links to use with the "statcan" method,
  "statcan" (the default) for the links as supplied by Statistics
  Canada, or "adjusted" for the audited links. The audit checked the
  StatCan links against the geography of the regions, flagged links the
  geography contradicts and added links for regions left without one. At
  this point audited links are available for the 1991 to 1986 and 1986
  to 1981 steps, the other steps use the StatCan links either way. A
  message lists the steps with audited links, and with "adjusted" there
  is a warning if none of the steps have them. Ignored for the other
  methods.

- tolerance:

  tolerance for \`estimate_tongfen_correspondence\` in metres, default
  value is 50 metres, only used when method is 'estimate' or
  'identifier'

- quiet:

  suppress download progress output, default is \`FALSE\`

- refresh:

  optional character, refresh data cache for this call, (default
  \`FALSE\`)

- crs:

  CRS to use for the spatial intersections if method is 'identifier' or
  'estimate', default is \`3347\` (Statistics Canada Lambert)

## Value

dataframe with the multi-census correspondence file

## Examples

``` r
# Get correspondance files between CTs in 2006 and 2016 censuses in Vancouver CMA
if (FALSE) { # \dontrun{
correspondence <- get_tongfen_correspondence_ca_census(geo_datasets=c('CA06','CA16'),
                                                       regions=list(CMA="59933"),level='CT')
} # }
```
