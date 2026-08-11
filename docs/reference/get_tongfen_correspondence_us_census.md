# Get correspondence table for US census geographies

**\[maturing\]**

Builds a correspondence table matching US census geographies across
censuses, based on the relationship files published by the US Census
Bureau. Censuses that aren't requested but sit in between two that are
get traversed on the way, the Census Bureau only publishes relationship
files between consecutive censuses.

The relationship files are geometric overlays that list every sliver
along boundaries that only shifted slightly. Those get cut via
\`min_area_share\`, keeping them would chain unrelated regions into one
common geography.

The correspondence layer reaches back one census further than
[`get_tongfen_us_census`](https://mountainmath.github.io/tongfen/reference/get_tongfen_us_census.md).
The 1990 census is available as \`dec1990\` here, but the Census Bureau
has retired the 1990 API endpoint, so 1990 data has to be brought in by
other means, for example from NHGIS via the ipumsr package, and handed
to
[`tongfen_aggregate`](https://mountainmath.github.io/tongfen/reference/tongfen_aggregate.md)
together with this correspondence table.

## Usage

``` r
get_tongfen_correspondence_us_census(
  datasets,
  regions,
  level = "tract",
  min_area_share = 0.01,
  cache_path = getOption("tongfen.cache_path")
)
```

## Arguments

- datasets:

  vector of censuses to match up, valid values are \`dec1990\`,
  \`dec2000\`, \`dec2010\` and \`dec2020\` for census tracts,
  \`dec2000\` through \`dec2020\` for county subdivisions. At least two
  censuses are needed.

- regions:

  list with regions to query the correspondence for. At this stage, the
  only valid list is a vector of states, i.e. \`regions =
  list(state=c("CA","OR"))\`

- level:

  aggregation level, at this stage the only valid levels are 'tract' and
  'county subdivision'.

- min_area_share:

  minimum share of area two geographies have to have in common to count
  as related, default is \`0.01\`. The Census Bureau relationship files
  list every geometric overlap, lowering this pulls in slivers along
  boundaries that only shifted slightly and chains unrelated regions
  into one common geography. Raising it gives finer common geographies
  at the risk of separating regions that did change. No region is ever
  dropped, if all of its parts are slivers its largest part is kept.

- cache_path:

  optional path to cache the relationship files in, defaults to the
  \`tongfen.cache_path\` option and falls back to a temporary directory

## Value

tibble with one row per census geography, a GEOID column for each
requested census, and the common geography identified by \`TongfenID\`
and \`TongfenUID\`.

## Examples

``` r
# Match up census tracts for the 1990 and 2000 censuses in Rhode Island
if (FALSE) { # \dontrun{
correspondence <- get_tongfen_correspondence_us_census(datasets = c("dec1990","dec2000"),
                                                       regions = list(state="RI"))
} # }
```
