# Tongfen estimate data for given geometry

**\[maturing\]**

Estimates values for the given census vectors for the given geometry
using data from the specified level range. This is a wrapper around
\`cancensus::get_intersecting_geometries\` and \`tongfen_estimate\`,
optionally with downsampling via \`proportional_reaggregate\`, to
streamline estimating Canadian census data on custom geographies.

## Usage

``` r
tongfen_estimate_ca_census(
  geometry,
  meta,
  level,
  intersection_level = level,
  downsample_level = NULL,
  na.rm = FALSE,
  quiet = FALSE
)
```

## Arguments

- geometry:

  geometry

- meta:

  metadata for the census variables to aggregate, for example as
  returned by \`meta_for_ca_census_vectors\`. At this point this
  function only accepts variables from the same census geography year.
  We will expand this to also allow estimates across multiple census
  geography years, but this requires further attention to detail. It is
  recommended to apply due caution when running this function separately
  across several census geography years with the purpose of comparing
  data across time as a naive application can lead to systematic biases.

- level:

  level to use for tongfen

- intersection_level:

  level to use for geometry intersection, if different from tongfen
  level by `meta_for_ca_census_vectors`. This can be set at a higher
  aggregation level to conserve API points for the
  \`get_intersecting_geometries\` call.

- downsample_level:

  default \`NULL\`, can be a geographic level lower than \`level\`, in
  which case the data is downsamples to that geography level
  proportionally using the value of the \`downsample\` column (must be
  supplied) in the \`meta\` argument before intersecting the geometries.
  This can lead to more accurate results. At this point the only allowed
  variables for the \`downsample\` column in \`meta\` are "Population",
  "Households" or "Dwellings", and it can only be one of these for all
  variables.

- na.rm:

  how to deal with NA values, default is `FALSE`.

- quiet:

  suppress progress messages

## Value

\`geometry\` with the estimated values for the census variables
specified by \`meta\`

## Examples

``` r
# Estimate the 2016 population within 1 km of Toronto City Hall from dissemination area level
# census data
if (FALSE) { # \dontrun{
toronto_city_hall <- sf::st_point(c(-79.3839,43.6534)) %>%
  sf::st_sfc(crs=4326) %>%
  sf::st_transform(3348) %>%
  sf::st_buffer(1000) %>%
  sf::st_sf()

meta <- meta_for_additive_variables("CA16","Population")

data <- tongfen_estimate_ca_census(toronto_city_hall,meta,level="DA",intersection_level="CT")

print(paste0("Approximately ",scales::comma(data$Population,accuracy=100),
             " people live within a 1 km radius of Toronto City Hall."))

} # }
```
