# tongfen v.0.3.9
## Major changes
- new experimental functions to detect and correct for likely geocoding anomalies in timelines on a
  common geography, where the same dwellings got assigned to different neighbouring regions in
  different years. This shows up as a surprising drop in one region that is offset by a jump in a
  neighbouring region. `tongfen_detect_anomalies` lists the candidate regions,
  `tongfen_anomaly_joins` determines the regions to join, `tongfen_join_regions` joins them in
  data that already is on a common geography and `tongfen_join_correspondence` joins them in a
  correspondence for use with `tongfen_aggregate`. See the new "Geocoding anomalies in TongFen
  timelines" vignette for details
- StatCan correspondence files are now downloaded as parquet files from a mirror, Statistics Canada
  put the original files behind a browser check that blocks programmatic downloads, which broke
  `method = "statcan"`. Cached files are checked against the mirror once per session and downloaded
  again if they changed. The mirror location can be changed via the `tongfen.statcan_correspondence_url`
  option. Previously cached `statcan_correspondence_*.csv` files in the tongfen cache directory are
  no longer used and can be removed
## Minor changes
- combining correspondences across three or more datasets no longer runs into a cross join when the
  correspondences happen to be ordered so that consecutive ones don't share a geographic identifier.
  This gave a dplyr deprecation warning and needlessly large intermediate tables, results are unchanged
- fix `proportional_reaggregate` giving wrong results when the finer level data already has values
  for the categories to reaggregate. The values were compared to the parent total across all
  categories instead of per category, and a missing value in one child region discarded the
  existing values of all its siblings
- fix `tongfen_estimate` underestimating values when the intersection of a source and a target
  region is a geometry collection, i.e. several polygons joined by a shared boundary line
- `tongfen_estimate` with `na.rm = FALSE` no longer returns `NA` for target regions that only
  share a boundary with a source region with missing values
- `tongfen_estimate` now returns `NA` for target regions that don't overlap the source instead of
  erroring out when none of them do, and gives a clear error when `target` already has a column
  named like one of the variables to estimate
- fix `tongfen_aggregate` and `aggregate_data_with_meta` scaling averages by their parent variable
  more than once when the metadata lists the same variable name for several datasets, as is common
  with US census data. `tongfen_aggregate` now only uses the metadata of the dataset being
  aggregated, conflicting aggregation rules for the same variable are an error
- averages aggregated with `na.rm = TRUE` are now taken over the regions that have a value. The
  parent variable of regions with a missing average still counted toward the total the average
  was divided by, pulling the result toward zero. This affects `aggregate_data_with_meta`,
  `tongfen_aggregate`, `tongfen_estimate` and the functions built on them
- fix "Average to" variables like percentage changes overwriting each other's base when several of
  them share a parent variable. In that case the base columns in the result are named after the
  variable (`base_<variable>`) instead of the parent
- `estimate_tongfen_correspondence` no longer requires the geometry column to be named `geometry`
- `refresh = TRUE` in `get_tongfen_correspondence_ca_census` and `get_tongfen_ca_census` now also
  refreshes the cached StatCan correspondence files
- US Census Bureau relationship files are downloaded to a temporary file first, an interrupted
  download no longer leaves a broken file in the cache. They are now cached in the same place as
  the StatCan correspondence files, also honouring the `tongfen.cache_path` environment variable
  and the `custom_data_path` option
- `tongfen_estimate_ca_census` returns its result visibly
- documentation fixes, among others the `tongfen_aggregate` example now passes a named list of
  datasets matching the metadata
- requires dplyr 1.1.0 or newer, which the package already relied on

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
- `get_tongfen_us_census` gained a `sumfile` argument, passed through to tidycensus, either a
  single value for all censuses or a vector named by dataset. Without it 2020 data is read from
  the PL 94-171 redistricting file, which carries almost no variables
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
- fix `proportional_reaggregate` ignoring all but the first base variable when `base` names a
  different variable per category, which silently weighted every category by the same variable
- fix `estimate_tongfen_correspondence` with `method="identifier"` erroring out when every
  geographic identifier matches and there is nothing left to estimate geometrically
- fix `tongfen_tag_largest_overlap` emitting a tibble name repair deprecation warning
- `estimate_tongfen_correspondence` and `get_tongfen_correspondence_ca_census` now error out with
  a clear message when handed fewer than two geographies
- no longer trip the tidyselect deprecation warning for `.data` in `select()` and `rename()`
- functions relying on the suggested `cancensus`, `tidycensus` and `readxl` packages now check
  that they are installed and give an actionable message instead of failing deep in the call
- fix the duplicate check in `tongfen_aggregate` only looking at the first geographic identifier
  when matching over several
- cache directories for US data are now created recursively, so a nested
  `options(tongfen.cache_path=...)` works
- faster `check_tongfen_areas` and `aggregate_correspondences`

# tongfen v.0.3.7
## Major changes
- accommodate factors in proportional_reaggregate
- sizable performance increases
- squish several edge case bugs

# tongfen v.0.3.6
## Major changes
- better downsampling that can also accommodate averages
- performance improvements
## Minor changes
- better documentation
- allow for datasets variables by census year for Canadian data
- fix issue where some metadata might get duplicated

# tongfen 0.3.2
- Fix compatibility issue with changes in {sf} package
- More reliable GitHub action CRAN checks

# tongfen 0.3.2

## Major changes
- Added `tongfen_estimate_ca_census` function for new CensusMapper endpoint, tying into new {cancensus} functionality.
## Minor changes
- Custom implementation of `tongfen_estimate` for finer control
- Fix compatibility issue with changes in {sf} package

# tongfen 0.3

## Major changes
- Initial release
