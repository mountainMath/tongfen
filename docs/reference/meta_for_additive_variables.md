# Generate tongfen metadata for additive variables

**\[maturing\]**

Generates metadata to be used in tongfen_aggregate. Variables need to be
additive like counts.

## Usage

``` r
meta_for_additive_variables(dataset, variables)
```

## Arguments

- dataset:

  identifier for the dataset contianing the variable

- variables:

  (named) vecotor with additive variables

## Value

a tibble to be used in tongfen_aggregate

## Examples

``` r
# Get metadata for additive variable Population for the CA16 and CA06 datasets
if (FALSE) { # \dontrun{
meta <- meta_for_additive_variables(c("CA06","CA16"),"Population")
} # }
```
