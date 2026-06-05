# Check geographic integrety

**\[deprecated\]**

Sanity check for areas of estimated tongfen correspondence. This is
useful if for example the total extent of geo1 and geo2 differ and there
are regions at the edges with large difference in overlap.

## Usage

``` r
check_tongfen_single_areas(geo1, geo2, correspondence)
```

## Arguments

- geo1:

  input geometry 1 of class sf

- geo2:

  input geometry 2 of class sf

- correspondence:

  Correspondence table between \`geo1\` and \`geo2\` as e.g. returned by
  \`estimate_tongfen_correspondence\`.

## Value

A table with columns \`TongfenID\`, \`area1\` and \`area2\`, where each
row corresponds to a unique \`TongfenID\` from them \`correspondence\`
table and the other columns hold the areas of the regions aggregated
from \`geo1\` and \`geo2\`.\`
