# Get StatCan DA or DB level correspondence file

**\[maturing\]**

## Usage

``` r
get_single_correspondence_ca_census_for(
  year,
  level = c("DA", "DB"),
  refresh = FALSE
)
```

## Arguments

- year:

  census year, only 2006 through 2021 are supported

- level:

  geographic level, DA or DB

- refresh:

  reload the correspondence files, default is \`FALSE\`

## Value

tibble with correspondence table\`
