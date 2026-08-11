# Get US census data for 2000 and 2010 census on common census tract based geography

**\[maturing\]**

This wraps data acquisition via the tidycensus package and tongfen on a
common geography into a single convenience function.

Data is only available for the 2000, 2010 and 2020 censuses, the Census
Bureau has retired the 1990 API endpoint. To tongfen 1990 data, obtain
it elsewhere and combine it with a correspondence table from
[`get_tongfen_correspondence_us_census`](https://mountainmath.github.io/tongfen/reference/get_tongfen_correspondence_us_census.md)
via
[`tongfen_aggregate`](https://mountainmath.github.io/tongfen/reference/tongfen_aggregate.md).

## Usage

``` r
get_tongfen_us_census(
  regions,
  meta,
  level = "tract",
  survey = "census",
  base_geo = NULL,
  min_area_share = 0.01,
  sumfile = NULL
)
```

## Arguments

- regions:

  list with regions to query the data for. At this stage, the only valid
  list is a vector of states, i.e. \`regions = list(state=c("CA","OR"))“

- meta:

  metadata for variables to retrieve

- level:

  aggregation level to return the data on. At this stage, the only valid
  levels are 'tract' and 'county subdivision'.

- survey:

  survey to get data for, supported options is "census"

- base_geo:

  census year to use as base geography, default is \`2010\`.

- min_area_share:

  minimum share of area two geographies have to have in common to count
  as related, default is \`0.01\`, see
  [`get_tongfen_correspondence_us_census`](https://mountainmath.github.io/tongfen/reference/get_tongfen_correspondence_us_census.md).

- sumfile:

  summary file to read the variables from, either a single value used
  for all censuses or a vector named by dataset, for example
  \`c(dec2010="sf1", dec2020="dhc")\`. Default is \`NULL\`, which leaves
  the choice to tidycensus. Note that tidycensus defaults the 2020
  census to the PL 94-171 redistricting file, most 2020 variables need
  \`sumfile="dhc"\`.

## Value

sf object with (wide form) census variables with census year as suffix
(separated by underdcore "\_").

## Examples

``` r
# Get US census data on population and households for 2000 and 2010 censuses on a uniform geography
# based on census tracts.
if (FALSE) { # \dontrun{
variables=c(population="H011001",households="H013001")

meta <- c(2000,2010) %>%
  lapply(function(year){
    v <- variables %>% setNames(paste0(names(.),"_",year))
    meta_for_additive_variables(paste0("dec",year),v)
  }) %>%
  bind_rows()
census_data <- get_tongfen_us_census(regions = list(state="CA"), meta=meta, level="tract") %>%
  mutate(change=population_2010/households_2010-population_2000/households_2000)

} # }
```
