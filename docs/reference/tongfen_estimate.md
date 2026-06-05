# Estimate variable values for custom geography

**\[maturing\]**

Estimates data from source geometry onto target geometry using
area-weighted interpolation. The metadata specifies how data should be
aggregated, "additive" data like population counts are summed up
proportionally to the area of the intersection, "averages" need further
additive "parent" count variables to estimate weighted averages.

## Usage

``` r
tongfen_estimate(target, source, meta, na.rm = FALSE)
```

## Arguments

- target:

  custom geography to estimate values for

- source:

  input geography with values

- meta:

  metadata for variable aggregation, see \`meta_for_additive_variables\`
  and \`meta_for_ca_census_vectors\` for more information on how to
  construct metadata.

- na.rm:

  remove NA values when aggregating, default is FALSE

## Value

\`target\` with estimated quantities from \`source\` as specified by
\`meta\`

## Examples

``` r
# Estimate 2006 Population in the City of Vancouver dissemination ares on 2016 census geographies
if (FALSE) { # \dontrun{
geo1 <- cancensus::get_census("CA06",regions=list(CSD="5915022"),geo_format='sf',level='DA')
geo2 <- cancensus::get_census("CA16",regions=list(CSD="5915022"),geo_format='sf',level='DA')
meta <- meta_for_additive_variables("CA06","Population")
result <- tongfen_estimate(geo2 %>% rename(Population_2016=Population),geo1,meta)
} # }
```
