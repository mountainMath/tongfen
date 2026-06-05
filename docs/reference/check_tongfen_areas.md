# Check geographic integrety

**\[maturing\]**

Sanity check for areas of estimated tongfen correspondence. This is
useful if for example the total extent of geo1 and geo2 differ and there
are regions at the edges with large difference in overlap.

## Usage

``` r
check_tongfen_areas(data, correspondence)
```

## Arguments

- data:

  alist of geogrpahic data of class sf

- correspondence:

  Correspondence table with columns the unique geographic identifiers
  for each of the geographies and the TongfenID (and optionally
  TongfenUID and TongfenMethod) returned by
  \`estimate_tongfen_correspondence\`.

## Value

A table with columns \`TongfenID\`, geo_identifiers, the areas of the
aggregated regions corresponding to each geographic identifier column,
the tongfen estimation method and the maximum log ratio of the areas.

## Examples

``` r
# Estimate a common geography for 2006 and 2016 dissemination areas in the City of Vancouver
# based on the geographic data and check estimation errors
if (FALSE) { # \dontrun{
regions <- list(CSD="5915022")

data_06 <- cancensus::get_census("CA06",regions=regions,geo_format='sf',level="DA") %>%
  rename(GeoUID_06=GeoUID)
data_16 <- cancensus::get_census("CA16",regions=regions,geo_format="sf",level="DA") %>%
  rename(GeoUID_16=GeoUID)

correspondence <- estimate_tongfen_correspondence(list(data_06, data_16),
                                                  c("GeoUID_06","GeoUID_16"))

area_check <- check_tongfen_areas(list(data_06, data_16),correspondence)
} # }
```
