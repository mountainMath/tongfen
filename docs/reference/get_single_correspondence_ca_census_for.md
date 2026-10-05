# Get StatCan DA or DB level correspondence file

**\[maturing\]**

The correspondence files are downloaded from a mirror of the Statistics
Canada correspondence files and cached in the tongfen cache directory.
The cached files are checked against the mirror once per session and get
downloaded again if they changed. The location of the mirror can be
changed via the \`tongfen.statcan_correspondence_url\` option.

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
