## Submission - v.0.3

## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes


## Vignettes
Some vignettes require API authentification and code chunks are not evaluated when system variables are not enabled. Vignettes are prebuilt with pdfs included. Vignettes are evaluated locally and on Travis with [log results](https://travis-ci.org/mountainMath/tongfen).


## Updates
* Removed used of global environment (e.g. by using <<-) in vignette.
* added examples for all exported functions (wrapped by \dontrun{} when they rely web API calls)
* added a section in the package README to link to Elections Canada data and open data licence.

We checked through several CRAN packages that ship with example data and found that this was a common pattern to acknowledge pre-downloaded data, for example [nycflights13](https://cran.r-project.org/web/packages/nycflights13/index.html) or [babynames](https://cran.r-project.org/web/packages/babynames/index.html). The DATASET.R file in the data-raw directory can be used to reproduce the datasets the package ships with.


