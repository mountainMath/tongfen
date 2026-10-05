# Join regions in a correspondence

**\[experimental\]**

Updates a correspondence so that the given regions are joined, for
example to correct for likely geocoding anomalies as determined by
\`tongfen_anomaly_joins\`. The updated correspondence can be used in
\`tongfen_aggregate\` to aggregate data on the coarser common geography,
which works for all variables \`tongfen_aggregate\` can deal with and
for data that was not part of detecting the anomalies.

## Usage

``` r
tongfen_join_correspondence(correspondence, joins)
```

## Arguments

- correspondence:

  correspondence table with columns the unique geographic identifiers
  for each of the geographies and the TongfenID and TongfenUID, as for
  example returned by \`estimate_tongfen_correspondence\` or
  \`get_tongfen_correspondence_ca_census\`

- joins:

  table with the regions to join as returned by
  \`tongfen_anomaly_joins\`, with columns \`TongfenID\` and
  \`TongfenID_joined\`

## Value

The correspondence with updated TongfenID and TongfenUID for the regions
that got joined. If the correspondence has a TongfenMethod column
"anomaly" gets added to the method of the regions that got joined.

## Examples

``` r
# Correct for likely geocoding problems in dissemination area level population timelines
# and use the updated correspondence to aggregate data on the corrected common geography
if (FALSE) { # \dontrun{
regions <- list(CSD="5915022")
datasets <- c("CA01","CA06","CA11","CA16","CA21")
meta <- meta_for_additive_variables(datasets,"Population")
data <- get_tongfen_ca_census(regions=regions,meta=meta,level="DA",base_geo="CA21")
joins <- tongfen_anomaly_joins(data,paste0("Population_",datasets))

correspondence <- get_tongfen_correspondence_ca_census(geo_datasets=datasets,
                                                       regions=regions,level="DA") %>%
  tongfen_join_correspondence(joins)
} # }
```
