# Generate togfen correspondence for list of geographies

**\[maturing\]**

Get correspondence data for arbitrary congruent geometries. Congruent
means that one can obtain a common tiling by aggregating several
sub-geometries in each of the two input geo data. Worst case scenario
the only common tiling is given by unioning all sub-geometries and there
is no finer common tiling.

## Usage

``` r
estimate_tongfen_correspondence(
  data,
  geo_identifiers,
  method = "estimate",
  tolerance = 50,
  computation_crs = NULL
)
```

## Arguments

- data:

  list of geometries of class sf

- geo_identifiers:

  vector of unique geographic identifiers for each list entry in data.

- method:

  aggregation method. Possible values are "estimate" or "identifier".
  "estimate" estimates the correspondence purely from the geographic
  data. "identifier" assumes that regions with identical geo_identifiers
  are the same, and uses the "estimate" method for the remaining
  regions. Default is "estimate".

- tolerance:

  tolerance (in projected coordinate units of \`computation_crs\`) for
  feature matching

- computation_crs:

  optional crs in which the computation should be carried out, defaults
  to crs of the first entry in the data parameter.

## Value

A correspondence table linking geo1_uid and geo2_uid with unique
TongfenID and TongfenUID columns that enumerate the common geometry.

## Examples

``` r
# Estimate a common geography for 2006 and 2016 dissemination areas in the City of Vancouver
# based on the geographic data.
if (FALSE) { # \dontrun{
regions <- list(CSD="5915022")

data_06 <- cancensus::get_census("CA06",regions=regions,geo_format='sf',level="DA") %>%
 rename(GeoUID_06=GeoUID)
data_16 <- cancensus::get_census("CA16",regions=regions,geo_format="sf",level="DA") %>%
  rename(GeoUID_16=GeoUID)

correspondence <- estimate_tongfen_correspondence(list(data_06, data_16),
                                                  c("GeoUID_06","GeoUID_16"))

} # }
```
