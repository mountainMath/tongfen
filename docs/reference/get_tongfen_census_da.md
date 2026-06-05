# Canadian Census DA level tongfen

**\[deprecated\]**

Grab variables from several censuses on a common geography. Requires sf
package to be available Will return CT level data

## Usage

``` r
get_tongfen_census_da(
  regions,
  vectors,
  geo_format = NA,
  use_cache = TRUE,
  na.rm = TRUE,
  quiet = TRUE
)
```

## Arguments

- regions:

  census region list, should be inclusive list of GeoUIDs across
  censuses

- vectors:

  List of cancensus vectors, can come from different census years

- geo_format:

  \`NA\` to only get the variables or 'sf' to also get geographic data

- use_cache:

  logical, passed to \`cancensus::get_census\` to regulate caching

- na.rm:

  logical, determines how NA values should be treated when aggregating
  variables

- quiet:

  suppress download progress output, default is \`TRUE\`

## Value

dataframe with variables on common geography
