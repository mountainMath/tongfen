# Tag regions by largest overlap

**\[maturing\]**

tags regions in \`source\` by \`target_id\` of region in \`target\` with
the largest overlap

## Usage

``` r
tongfen_tag_largest_overlap(source, target, target_id)
```

## Arguments

- source:

  input geography

- target:

  custom geography

- target_id:

  name of the column in \`target\` table with unique id (character)

## Value

\`source\` with extra column with name \`"target_id"\` and column
\`...overlap_fraction\` with the proportion of overlap of the target
geometry with the respective \`target_id\`

## Examples

``` r
# Estimate 2006 Populatino in the City of Vancouver dissemination ares on 2016 census geoographies
if (FALSE) { # \dontrun{
geo1 <- cancensus::get_census("CA06",regions=list(CSD="5915022"),geo_format='sf',level='DA')
geo2 <- cancensus::get_census("CA16",regions=list(CSD="5915022"),geo_format='sf',level='DA')
meta <- meta_for_additive_variables("CA06","Population")
result <- tongfen_estimate(geo2 %>% rename(Population_2016=Population),geo1,meta)
} # }
```
