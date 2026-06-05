# Aggregate variables in grouped data

**\[maturing\]**

Aggregate census data up, assumes data is grouped for aggregation Uses
data from meta to determine how to aggregate up

## Usage

``` r
aggregate_data_with_meta(data, meta, geo = FALSE, na.rm = TRUE, quiet = FALSE)
```

## Arguments

- data:

  census data as obtained from get_census call, grouped by TongfenID

- meta:

  list with variables and aggregation information as obtained from
  meta_for_vectors

- geo:

  logical, should also aggregate geographic data

- na.rm:

  logical, should NA values be ignored or carried through.

- quiet:

  logical, don't emit messages if set to \`TRUE\`

## Value

data frame with variables aggregated to new common geography

## Examples

``` r
# Aggregate population from DA level to grouped by CT_UID
if (FALSE) { # \dontrun{
geo <- cancensus::get_census("CA06",regions=list(CSD="5915022"),level='DA')
meta <- meta_for_additive_variables("CA06","Population")
result <- aggregate_data_with_meta(geo %>% group_by(CT_UID),meta)
} # }
```
