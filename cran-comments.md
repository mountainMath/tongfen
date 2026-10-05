# tongfen v.0.3.9
## Major changes
- new experimental functions `tongfen_detect_anomalies`, `tongfen_anomaly_joins`, `tongfen_join_regions`
  and `tongfen_join_correspondence` to detect and correct for likely geocoding anomalies in timelines
  on a common geography, together with a new vignette
- StatCan correspondence files are now downloaded as parquet files from a mirror, Statistics Canada
  put the original files behind a browser check that blocks programmatic downloads, which broke
  `method = "statcan"`
## Minor changes
- combining correspondences across three or more datasets no longer runs into a cross join
- fix `proportional_reaggregate` giving wrong results when the finer level data already has values
- fix `tongfen_estimate` mishandling intersections that are geometry collections, regions that only
  touch the target, and targets without overlap with the source
- fix averages getting scaled more than once in `tongfen_aggregate` when the metadata lists the same
  variable name for several datasets
- fix averages with missing values being pulled toward zero when aggregating with `na.rm = TRUE`,
  and "Average to" variables sharing a parent variable overwriting each other's base
- `estimate_tongfen_correspondence` no longer requires the geometry column to be named `geometry`
- `refresh = TRUE` now also refreshes the cached StatCan correspondence files
- US Census Bureau relationship files are downloaded to a temporary file before being moved to the cache
- added missing `\value` documentation for `tongfen_estimate_ca_census` and `tongfen_ca_census_ct`
- fixed typos in the documentation

# Test environments
* local macOS installation, R 4.6.0
* GitHub actions: macOS-latest (release), windows-latest (release), ubuntu-latest (devel, release, oldrel-1)

# R CMD check results
0 errors | 0 warnings | 0 notes

There are no reverse dependencies.


