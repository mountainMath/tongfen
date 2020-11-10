# tongfen

[![Build Status](https://travis-ci.org/mountainMath/tongfen.svg?branch=master)](https://travis-ci.org/mountainMath/tongfen)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tongfen)](https://cran.r-project.org/package=tongfen)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/tongfen)](https://cranlogs.r-pkg.org/badges/tongfen)

<a href="https://mountainmath.github.io/tongfen/index.html"><img src="https://raw.githubusercontent.com/mountainMath/tongfen/master/images/tongfen-sticker.png" alt="tongfen logo" align="right" width = "25%" height = "25%"/></a>
TongFen (通分) means to convert two fractions to the least common denominator, typically in preparation for further manipulation like addition or subtraction. In English, that's a mouthful and sounds complicated. But in Chinese there is a word for this, TongFen, which makes this process appear very simple.

When working with geospatial datasets we often want to compare data that is given on different regions. For example census data and election data. Or data from two different censuses. To properly compare this data we first need to convert it to a common geography. The process to do this is quite analogous to the process of TongFen for fractions, so we appropriate this term to give it a simple name. Using the **tongfen** package, preparing data on disparate geographies for comparison by converting them to a common geography is as easy as typing `tongfen`.

### Reference

[**TongFen home page and reference guide**](https://mountainmath.github.io/tongfen/index.html)

### Installing the package

```
install.packages("tongfen")
```

The latest development version can be installed from GitHub.
```
remotes::install_github("mountainmath/tongfen")
library(tongfen)
```

### Caching correspondence files
The `get_tongfen_census_ct` and `get_tongfen_census_ct_from_da` methods make use of the StatCan correspondence
files. To speed up this process it is useful to permanently cache these files instead of having to download them repeatedly. If caching is desired, set either 

* `options("tongfen.cache_path"="<your local cache path>")` 
* `Sys.setenv("tongfen.cache_path"="<your local cache path>")`
* `options("custom_data_path"="<your local cache path>")` 

in your `.Rprofile` or `.Renviron` file. 

## General TongFen

The `tongfen` package is build around the following basic **TongFen workflow**:

1. Given a list of datasets on diverse geographies, **generate a correspondence table** that links the geographies and specifies how to aggregate them up to a (least) common geography via `estimate_tongfen_correspondence`.
2. **generate metadata** that specifies how variables can be aggregated up, the `meta_for_additive_variables` function does this for additive variables.
3. Use the correspondence table and metadata to generate a dataset with variables from the original datasets aggregated up on a common geography via `tongfen_aggregate`.

A convenience function to validate geographic TongFen fit via area comparison is available via `check_tongfen_areas`, it allows to explore and deal with spatial mismatches during TongFen.


### Aggregation of variables
Finding a common tiling of several different yet congruent geographies is only one part of the problem TongFen addresses, aggregating up the variables is the other part. The `tongfen` package deals with this using a *metadata* table that specifies how variables should be aggregated. In it's simplest form values are simply added up. The `meta_for_additive_variables` convenience function builds the metadata for additive variables. Metadata for non-additive variables like averages, ratios or percentages needs more care to build, it requires additional information on the **parent variable** that specifies the denominator of the average, ratio or percentage. Other data, like medians, can't be aggregated up, although `tongfen` can provide estimates of medians on aggregated geographies by treating them as averages.

### Packaged data
The package ships with a subset of [voting data from Elections Canada](https://www.elections.ca/content.aspx?section=ele&dir=pas&document=index&lang=e) for the 42nd and 43rd federal elections as well as the polling district geographies for the [42nd](https://open.canada.ca/data/en/dataset/6a78ccfd-6bba-4109-b040-87cb8c71ec35) and [43rd](https://open.canada.ca/data/en/dataset/e70e3263-8584-4f22-94cb-8c15b616cbfc). This facilitates running the example vignette on polling districts without having to download external data. Both are available as open data covered under the [Open Government Licence - Canda](https://open.canada.ca/en/open-government-licence-canada).

## Data-specific implementations
The need for TongFen comes up frequently with certain types of geographies. Census geographies is one such example. In some cases these data sources come with their own correspondence files that go beyond geographic matchup but also join regions to alleviate data integrity problems like geocoding issues. 

In such cases it can be worthwhile to wrap data acquisition and TongFen into one convenience function, and also extend the TongFen *method* parameter to allow for external correspondence files to be used.

### Canadian census data
The package is well-integrated to work with Canadian census data in two essential ways.
* `meta_for_ca_census_vectors` builds rich metadata for a given list of Canadian census variables by utilizing the metadata available via [CensusMapper](https://censusmapper.ca). In particular, this automates the proper aggregation of non-count variables like averages, ratios and percentages.
* `get_tongfen_ca_census` wraps the process of data acquisition (via CensusMapper and the [**cancensus** package](https://mountainmath.github.io/cancensus/index.html) and tongfen into one convenience function. At the same time it adds the TongFen `method = "statcan"` option that uses the Statistics Canada correspondence files to build the common geography. 
* The `get_tongfen_correspondence_ca_census` function breaks out the correspondence generation to aid the process of accessing the Statistics Canada correspondence files (and better integration of generating correspondences for Canadian census geographies in general) to facilitate mixing in non-census data coming on census geographies, like for example [CMHC data](https://www03.cmhc-schl.gc.ca/hmip-pimh).


### US census data
* `get_tongfen_us_census` integrates the data acquisition (via the [**tidycensus** package](https://walker-data.com/tidycensus/index.html)) with TongFen, and adds the tongfen `method = "census.gov"` to use the US Census Bureau correspondence files for matching.

## Other implementations
The `tongfen` package is open to add extensions for other specialized data sources, as well as extensions of existing ones. 


## Fixed target geography estimation
When geographies aren't sufficiently congruent or the target geography is fixed, we won't be able to use the `tongfen` methods to compute the data on a common geography but have to instead rely on estimates. The  `tongfen_estimate` makes no assumption on the underlying geographies and returns estimates of the data on the target geography. It uses area-weighted interpolation to achieve this, and can be refined to dasymmetric estimates using the `proportional_reaggregate` function.

This method has the example that it works independent of the nature of the underlying geographies, but comes at the heavy price of only being an estimate. To be useful for research purposes we also need methods to estimate the errors this introduces and the effects this has on subsequent analysis results.

Methods to facilitate this are still under active development. 

