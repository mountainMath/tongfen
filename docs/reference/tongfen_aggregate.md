# Perform tongfen according to correspondence

**\[maturing\]**

Aggregate variables secified in meta for several datasets according to
correspondence.

## Usage

``` r
tongfen_aggregate(data, correspondence, meta = NULL, base_geo = NULL)
```

## Arguments

- data:

  list of datasets to be aggregated

- correspondence:

  correspondence data for gluing up the datasets

- meta:

  metadata containing aggregation rules as for example returned by
  \`meta_for_ca_census_vectors\`

- base_geo:

  identifier for which data element to base the final geography on, uses
  the first data element if \`NULL\` (default), expects that
  \`base_geo\` is an element of \`names(data)\`.

## Value

aggregated dataset of class sf if base_geo is not NULL and data is of
type sf or tibble otherwise.

## Examples

``` r
# aggregate census tract level 2006 population data on common gepgraphy build through
# correspondence from 2006 and 2016 census tracts in the City of Vancouver.
if (FALSE) { # \dontrun{
regions <- list(CSD="5915022")
geo1 <- cancensus::get_census("CA06",regions=regions,geo_format='sf',level='CT')
geo2 <- cancensus::get_census("CA16",regions=regions,geo_format='sf',level='CT')
meta <- meta_for_additive_variables("CA06","Population")
correspondence <- get_tongfen_correspondence_ca_census(geo_datasets=c('CA06','CA16'),
                                                       regions=regions,level='CT')
result <- tongfen_aggregate(list(geo1 %>% rename(GeoUIDCA06=GeoUID),
                                 geo2 %>% rename(GeoUIDCA16=GeoUID)),correspondence,meta)
} # }
```
