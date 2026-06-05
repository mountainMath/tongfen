# Canadian census CT level tongfen via identifier matching

**\[deprecated\]**

Aggregate variables to common CTs, returns data2 on new tiling matching
data1 geography

## Usage

``` r
tongfen_ca_census_ct(
  data1,
  data2,
  data2_sum_vars,
  data2_group_vars = c(),
  na.rm = TRUE
)
```

## Arguments

- data1:

  cancensus CT level datatset for year1 \< year2 to serve as base for
  common geography

- data2:

  cancensus CT level datatset for year2 to be aggregated to common
  geography

- data2_sum_vars:

  vector of variable names to by summed up when aggregating geographies

- data2_group_vars:

  optional vector of grouping variables

- na.rm:

  optional parameter to remove NA values when summing, default =
  \`TRUE\`
