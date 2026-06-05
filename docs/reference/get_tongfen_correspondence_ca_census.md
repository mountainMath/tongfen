# Get StatCan correspondence data

**\[maturing\]**

Get correspondence file for several Candian censuses on a common
geography. Requires sf and cancensus package to be available

## Usage

``` r
get_tongfen_correspondence_ca_census(
  geo_datasets,
  regions,
  level = "CT",
  method = "statcan",
  tolerance = 50,
  area_mismatch_cutoff = 0.1,
  quiet = FALSE,
  refresh = FALSE
)
```

## Arguments

- geo_datasets:

  vector of census geography dataset identifiers

- regions:

  census region list, should be inclusive list of GeoUIDs across
  censuses

- level:

  aggregation level to return data on (default is "CT")

- method:

  tongfen method, options are "statcan" (the default), "estimate",
  "identifier". \* "statcan" method builds up the common geography using
  Statistics Canada correspondence files, at this point this method only
  works for "DB", "DA" and "CT" levels. \* "estimate" uses
  \`estimate_tongfen_correspondence\` to build up the common geography
  from scratch based on geographies. \* "identifier" assumes regions
  with identical geographic identifier are identical, and builds up the
  the correspondence for regions with unmatched geographic identifiers.

- tolerance:

  tolerance for \`estimate_tongen_correspondence\` in metres, default
  value is 50 metres.

- area_mismatch_cutoff:

  discard areas returned by \`estimate_tongfen_correspondence\` with
  area mismatch (log ratio) greater than cutoff.

- quiet:

  suppress download progress output, default is \`FALSE\`

- refresh:

  optional character, refresh data cache for this call, (default
  \`FALSE\`)

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
