# tongfen

<a href="https://mountainmath.github.io/tongfen/index.html"><img src="https://raw.githubusercontent.com/mountainMath/tongfen/master/images/tongfen-sticker.png" alt="tongfen logo" align="right" width = "25%" height = "25%"/></a>

TongFen (通分) means to convert two fractions to the least common denominator, typically in preparation for further manipulation like addition or subtraction. In English, that's a mouthful and sounds complicated. But in Chinese there is a word for this, TongFen, which makes this process appear very simple.

When working with geospatial datasets we often want to compare data that is given on different regions. For example census data and election data. Or data from two different censuses. To properly compare this data we first need to convert it to a common geography. The process to do this is quite analogous to the process of TongFen for fractions, so we appropriate this term to give it a simple name. Using the **tongfen** package, preparing data on disparate geographies for comparison by converting them to a common geography is as easy as typing `tongfen`.

### Reference

[**TongFen home page and reference guide**](https://mountainmath.github.io/tongfen/index.html)

### Installing the package

The package can be installed from GitHub.
```
devtools::install_github("mountainmath/tongfen")
library(cancensus)
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
2. `tongfen_ct` works on Canadian census tracts from different censuses and returns the data on a common geography.
3. `tongfen_cancensus` works on any census geography obtained through the **cancensus** package. It utilizes the Statistics Canada correspondence files and gives the most accurate estimate of data across censuses.
4. `get_tongfen_census_ct` get Canadian census variables from any of the 2001, 2006, 2011 and 2016 censuses on a common geography based on CTs
5. `get_tongfen_census_ct_from_da` get Canadian census variables from any of the 2001, 2006, 2011 and 2016 censuses on a common geography based on CTs, build up from a DA level correspondence. This gives a more accurate, but coarser, match compared to using `get_tongfen_census_ct` and will use the official DA correspondence files.
6. `get_tongfen_census_ct` get Canadian census variables from any of the 2001, 2006, 2011 and 2016 censuses on a common geography based on DAs using the official DA correspondence files.

Not all data can be aggregated in this form, and some data requires different aggregation functions than others. For example, in census data we encounter variables representing simple counts, for example population, that must be added up when joining geographic regions. Averages or percentages require a weighted sum, medians cannot be aggregated but may be approximated. 

In the case of the data originating from the **cancensus** package the aggregation will be done automatically whenever possible, in other cases the user needs to specify how data should be aggregated.
