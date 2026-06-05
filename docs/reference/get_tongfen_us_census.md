# Get US census data for 2000 and 2010 census on common census tract based geography

**\[maturing\]**

This wraps data acquisition via the tidycensus package and tongfen on a
common geography into a single convenience function.

## Usage

``` r
get_tongfen_us_census(
  regions,
  meta,
  level = "tract",
  survey = "census",
  base_geo = NULL
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
