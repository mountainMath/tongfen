# Togfen data from several Canadian censuses

**\[maturing\]**

Get data from several Candian censuses on a common geography. Requires
sf and cancensus package to be available

## Usage

``` r
get_tongfen_ca_census(
  regions,
  meta,
  level = "CT",
  method = "statcan",
  base_geo = NULL,
  na.rm = FALSE,
  tolerance = 50,
  area_mismatch_cutoff = 0.1,
  quiet = FALSE,
  refresh = FALSE,
  crs = NULL,
  data_transform = function(d) d
)
```

## Arguments

- regions:

  census region list, should be inclusive list of GeoUIDs across
  censuses

- meta:

  metadata for the census veraiables to aggregate, for example as
  returned by `meta_for_ca_census_vectors`.

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

- base_geo:

  base census year to build up common geography from, \`NULL\` (the
  default) to not return any geographi data

- na.rm:

  logical, determines how NA values should be treated when aggregating
  variables

- tolerance:

  tolerance for \`estimate_tongen_correspondence\` in metres, default
  value is 50 metres, only used when method is 'estimate' or
  'identifier'

- area_mismatch_cutoff:

  discard areas returned by \`estimate_tongfen_correspondence\` with
  area mismatch (log ratio) greater than cutoff, only used when method
  is 'estimate' or 'identifier'

- quiet:

  suppress download progress output, default is \`FALSE\`

- refresh:

  optional character, refresh data cache for this call, (default
  \`FALSE\`)

- crs:

  optional CRS to transform data to, and use for spatial intersections
  if method is 'identifier' or 'estimate'

- data_transform:

  optional transform function to be applied to census data after being
  returned from cancensus

## Value

dataframe with variables on common geography

## Examples

``` r
# Get rent data for census years 2001 through 2016
if (FALSE) { # \dontrun{
rent_variables <- c(rent_2001="v_CA01_1667",rent_2016="v_CA16_4901",
                    rent_2011="v_CA11N_2292",rent_2006="v_CA06_2050")
meta <- meta_for_ca_census_vectors(rent_variables)

regions=list(CMA="59933")
rent_data <- get_tongfen_ca_census(regions=regions, meta=meta, quiet=TRUE,
                                   method="estimate", level="CT", base_geo = "CA16")

} # }
```
