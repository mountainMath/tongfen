# tongfen

[![Build Status](https://travis-ci.org/mountainMath/tongfen.svg?branch=master)](https://travis-ci.org/mountainMath/tongfen)

<a href="https://mountainmath.github.io/tongfen/index.html"><img src="https://raw.githubusercontent.com/mountainMath/tongfen/master/images/tongfen-sticker.png" alt="tongfen logo" align="right" width = "25%" height = "25%"/></a>
TongFen (通分) means to convert two fractions to the least common denominator, typically in preparation for further manipulation like addition or subtraction. In English, that's a mouthful and sounds complicated. But in Chinese there is a word for this, TongFen, which makes this process appear very simple.

When working with geospatial datasets we often want to compare data that is given on different regions. For example census data and election data. Or data from two different censuses. To properly compare this data we first need to convert it to a common geography. The process to do this is quite analogous to the process of TongFen for fractions, so we appropriate this term to give it a simple name. Using the **tongfen** package, preparing data on disparate geographies for comparison by converting them to a common geography is as easy as typing `tongfen`.

### Reference

[**TongFen home page and reference guide**](https://mountainmath.github.io/tongfen/index.html)

### Installing the package

The package can be installed from GitHub.
```
devtools::install_github("mountainmath/tongfen")
library(tongfen)
```

### Caching correspondence files
The `get_tongfen_census_ct` and `get_tongfen_census_ct_from_da` methods make use of the StatCan correspondence
files. To speed up this process it is useful to permanently cache these files instead of having to download them repeatedly. If caching is desired, set either 

* `options("tongfen.cache_path"="<your local cache path>")` 
* `Sys.setenv("tongfen.cache_path"="<your local cache path>")`
* `options("custom_data_path"="<your local cache path>")` 

in your `.Rprofile` or `.Renviron` file. 

### Introduction

The package offers several functions

1. `tongfen_estimate` makes no assumption on the underlying geographies and returns estimates of the data on the geography of the first dataset.
2. `estimate_tongfen_correspondence` creates a correspondence table for least common denomicator geography given two different but congruent input geograpies.

#### Canadian census data
The package is well-integrated to work with Canadian census data.
* `tongfen_ca_censusct` works on Canadian census tracts from different censuses and returns the data on a common geography. This takes a loose approach to tongfen by generating the correspondence from geographic data and optionally assuming regions with the same GeoUIDs are the same and boundary changes of these only affect greenfield sites.
* `get_tongfen_ca_censusct` get Canadian census variables from any of the 2001, 2006, 2011 and 2016 censuses on a common geography based on CTs
* `get_tongfen_ca_censusct_from_da` get Canadian census variables from any of the 2001, 2006, 2011 and 2016 censuses on a common geography based on CTs, build up from a DA level correspondence. This gives a more accurate, but coarser, match compared to using `get_tongfen_ca_censusct` and will use the official DA correspondence files.

#### US census data
* `get_tongfen_us_census_ct`

Not all data can be aggregated in this form, and some data requires different aggregation functions than others. For example, in census data we encounter variables representing simple counts, for example population, that must be added up when joining geographic regions. Averages or percentages require a weighted sum, medians cannot be aggregated but may be approximated. 

In the case of the data originating from the **cancensus** package the aggregation will be done automatically whenever possible, in other cases the user needs to specify how data should be aggregated.
