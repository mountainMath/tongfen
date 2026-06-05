# Generate togfen correspondence for two geographies

**\[maturing\]**

Get correspondence data for arbitrary congruent geometries. Congruent
means that one can obtain a common tiling by aggregating several
sub-geometries in each of the two input geo data. Worst case scenario
the only common tiling is given by unioning all sub-geometries and there
is no finer common tiling.

## Usage

``` r
estimate_tongfen_single_correspondence(
  geo1,
  geo2,
  geo1_uid,
  geo2_uid,
  tolerance = 1,
  computation_crs = NULL,
  robust = FALSE
)
```

## Arguments

- geo1:

  input geometry 1 of class sf

- geo2:

  input geometry 2 of class sf

- geo1_uid:

  (unique) identifier column for geo1

- geo2_uid:

  (unique) identifier column for geo2

- tolerance:

  tolerance (in projected coordinate units) for feature matching

- computation_crs:

  optional crs in which the computation should be carried out, defaults
  to crs of geo1

- robust:

  boolean parameter, will ensure geometries are valid if set to TRUE

## Value

A correspondence table linking geo1_uid and geo2_uid with unique
TongfenID and TongfenUID columns that enumerate the common geometry.
