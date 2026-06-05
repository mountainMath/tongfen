# Canadian census CT level tongfen

**\[deprecated\]**

Grab variables from several censuses on a common geography. Requires sf
package to be available Will return CT level data

## Usage

``` r
get_tongfen_census_ct(
  regions,
  vectors,
  geo_format = NA,
  na.rm = TRUE,
  quiet = TRUE,
  refresh = FALSE
)
```

## Arguments

- regions:

  census region list, should be inclusive list of GeoUIDs across
  censuses

- vectors:

  List of cancensus vectors, can come from different census years

- geo_format:

  geographic format for returned data, 'sf' for sf format and \`NA“

- na.rm:

  remove NA values when aggregating up values, default is \`TRUE\`

- quiet:

  suppress download progress output, default is \`FALSE\`

- refresh:

  optional character, refresh data cache for this call

## Value

dataframe with census variables on common geography
