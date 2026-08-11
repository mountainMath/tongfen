# tongfen v.0.3.8
## Breaking changes
- `get_tongfen_ca_census` now honours its `base_geo`, `na.rm`, `tolerance`, `crs` and
  `data_transform` arguments, all of which were silently ignored. Most visibly, the
  documented default `base_geo = NULL` now returns data without geographic information,
  where previously the geography of the first dataset was returned. Pass `base_geo` to
  get an `sf` object back
- removed the `area_mismatch_cutoff` argument from `get_tongfen_ca_census` and
  `get_tongfen_correspondence_ca_census`, it never had any effect. Use `check_tongfen_areas`
  to inspect area mismatches, keeping in mind that geographies for different years are
  simplified independently and differ in how water features are cut out
## Major changes
- correspondence tables are now built via a vectorised connected components pass instead of
  a row-by-row union-find, which makes tongfen on large geographies dramatically faster
  (dissemination blocks for a large province: minutes down to seconds)
- the "statcan" method no longer downloads census geometries it does not use
- dissolving geometries skips regions that don't need to be merged
- new `get_tongfen_correspondence_us_census` to get correspondence tables for US census
  geographies without also fetching the data
- US census tract correspondence tables now reach back to the 1990 census (`dec1990`). The
  Census Bureau has retired the 1990 API endpoint, so 1990 data itself has to be brought in
  separately, for example from NHGIS, and combined via `tongfen_aggregate`
- US county subdivisions can now be matched across the 2010 and 2020 censuses, previously only
  the 2000 and 2010 censuses were available
- US correspondence tables no longer chain regions together over slivers. The Census Bureau
  relationship files list every geometric overlap, including boundaries that only shifted
  slightly, and matching those up merged unrelated regions into one common geography. The new
  `min_area_share` argument controls how much area two regions have to have in common to count
  as related, default is `0.01`, and no region is ever dropped. This gives substantially finer
  common geographies, for Rhode Island tracts across the 2010 and 2020 censuses 198 instead of
  60, for Vermont 151 instead of 26
## Minor changes
- `get_tongfen_correspondence_ca_census` gained a `crs` argument for the spatial
  intersections, default is `3347` (Statistics Canada Lambert)
- missing geographic identifiers no longer merge unrelated regions into one common geography
- fix crash when tongfen-ing census tracts across non-adjacent censuses
- fix `get_tongfen_census_ct`, `get_tongfen_census_da` and `get_tongfen_ca_census_ct_from_da`
  erroring out when called with `geo_format=NA`
- fix 2020 US census tract identifiers getting stripped of their leading zeros when read from
  the relationship file, which silently dropped most tracts out of the result. For Rhode Island
  246 of 250 tracts were affected
- US county subdivision data now errors out up front on censuses it can't be matched across,
  instead of failing with "Did not find matching geographic identifiers" after downloading
  the comparability file and the census data
- faster `check_tongfen_areas` and `aggregate_correspondences`

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

# tongfen 0.3.2
- Fix compatibility issue with changes in {sf} package
- More reliable GitHub action CRAN checks

# tongfen 0.3.2

## Major changes
- Added `tongfen_estimate_ca_census` function for new CensusMapper endpoint, tying into new {cancensus} functionality.
## Minor changes
- Custom impelementation of `tongfen_etimate` for finer control
- Fix compatibility issue with changes in {sf} package

# tongfen 0.3

## Major changes
- Initial release
