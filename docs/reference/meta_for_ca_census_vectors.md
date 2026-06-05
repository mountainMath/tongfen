# Generate metadata from Candian census vectors

**\[maturing\]**

Build tibble with information on how to aggregate variables given
vectors Queries list_census_variables to obtain needed information and
add in vectors needed for aggregation

## Usage

``` r
meta_for_ca_census_vectors(vectors)
```

## Arguments

- vectors:

  list of variables to query

## Value

tidy dataframe with metadata information for requested variables and
additional variables needed for tongfen operations

## Examples

``` r
# Build metadata for vectors
if (FALSE) { # \dontrun{
meta <- meta_for_ca_census_vectors("v_CA16_4836","v_CA16_4838","v_CA16_4899")
} # }
```
