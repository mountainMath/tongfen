# tongfen v.0.3.8
## Breaking changes
- `get_tongfen_ca_census` now honours its `base_geo`, `na.rm`, `tolerance`, `crs` and
  `data_transform` arguments, all of which were silently ignored
- removed the `area_mismatch_cutoff` argument from `get_tongfen_ca_census` and
  `get_tongfen_correspondence_ca_census`, it never had any effect
## Major changes
- correspondence tables are now built via a vectorised connected components pass instead of
  a row-by-row union-find, making tongfen on large geographies dramatically faster
- the "statcan" method no longer downloads census geometries it does not use
- dissolving geometries skips regions that don't need to be merged
- new `get_tongfen_correspondence_us_census`, US tract correspondence tables now reach back to
  the 1990 census and county subdivisions forward to the 2020 census
- US correspondence tables no longer chain regions together over slivers, and no longer strip
  leading zeros off 2020 census tract identifiers
## Minor changes
- `get_tongfen_correspondence_ca_census` gained a `crs` argument for the spatial intersections
- missing geographic identifiers no longer merge unrelated regions into one common geography
- fix crash when tongfen-ing census tracts across non-adjacent censuses
- several fixes to the deprecated `get_tongfen_census_*` functions

# tongfen v.0.3.7
## Major changes
- accommodate factors in proportional_reaggregate
- sizable performance increases
- squish several edge case bugs

# tongfen v.0.3.6
## Major changs
- better downsampling that can also accommodate averages
- performance improvements
## Minor changes
- better documentation
- allow for datasets vartiables by census year for canadian data
- fix issue where some metadata might get duplicated


# Update v.0.3.3
- Fix compatibility issue with changes in {sf} package
- More reliable GitHub action CRAN checks

# Update v.0.3.2
- Added `tongfen_estimate_ca_census` function for new CensusMapper endpoint, tying into new {cancensus} functionality.
- Custom impelementation of `tongfen_etimate` for finer control
- Fix compatibility issue with changes in {sf} package

# Submission - v.0.3

# Test environments
* local macOS installation, R 4.6.0
* GitHub actions (windows-latest, macOS-latest, ubuntu-latest) on release, devel and oldrel

# R CMD check results
0 errors | 0 warnings | 0 notes

There are no reverse dependencies.


