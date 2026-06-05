# Get StatCan DA or DB level correspondence file

**\[deprecated\]** Joins the StatCan correspodence files for several
census years

## Usage

``` r
get_correspondence_ca_census_for(years, level, refresh = FALSE)
```

## Arguments

- years:

  list of census years

- level:

  geographic level, DA or DB

- refresh:

  reload the correspondence files, default is \`FALSE\`

## Value

tibble with correspondence table\`spanning all years
