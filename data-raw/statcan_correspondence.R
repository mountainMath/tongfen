## code to prepare the StatCan DA and DB correspondence files hosted on S3
##
## StatCan put the correspondence files behind a browser check, so they can't be downloaded
## programmatically any more. Download and extract the zip files manually from
## https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DB_ID_txt.zip
## https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2006_92-156_DA_AD_txt.zip
## https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DB_ID_txt.zip
## https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2011_92-156_DA_AD_txt.zip
## https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DB_ID_csv.zip
## https://www12.statcan.gc.ca/census-recensement/2011/geo/ref/files-fichiers/2016/2016_92-156_DA_AD_csv.zip
## https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/correspondence-correspondance/files-fichiers/2021_92-156-X_DB_ID.zip
## https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/correspondence-correspondance/files-fichiers/2021_92-156-X_DA_AD.zip
## into `input_dir`, then run this script and upload the parquet files in `output_dir` to S3.

library(dplyr)

input_dir <- "~/Downloads"
output_dir <- "~/Downloads/tongfen_statcan_correspondence"

level_codes <- c(DA="DA_AD",DB="DB_ID")

prepare_statcan_correspondence <- function(year,level){
  new_field <- paste0(level,"UID",year)
  old_field <- paste0(level,"UID",year-5)
  dirs <- dir(input_dir,paste0("^",year,"_92-156.*",level_codes[[level]]),full.names=TRUE)
  dirs <- dirs[dir.exists(dirs)]
  file <- dir(dirs,"\\.(txt|csv)$",full.names=TRUE,recursive=TRUE)
  stopifnot(length(file)==1)
  # DA files are at DB granularity and carry the DBUID in the third column, 2021 files have extra DGUID columns
  headers <- if (level=="DB") c(new_field,old_field,"flag") else c(new_field,old_field,paste0("DBUID",year),"flag")
  d <- readr::read_csv(file,col_types=readr::cols(.default="c"),col_names=FALSE) %>%
    select(all_of(seq_along(headers))) %>%
    setNames(headers) %>%
    filter(grepl("^\\d+$",.data[[new_field]])) %>% # header row in the 2016 and 2021 files
    select(all_of(c(new_field,old_field,"flag"))) %>%
    unique() %>%
    arrange(.data[[new_field]],.data[[old_field]])
  stopifnot(all(grepl("^\\d+$",d[[old_field]])),all(d$flag %in% as.character(1:4)))
  d
}

dir.create(output_dir,showWarnings=FALSE)
for (year in c(2006,2011,2016,2021)) {
  for (level in c("DA","DB")) {
    prepare_statcan_correspondence(year,level) %>%
      nanoparquet::write_parquet(file.path(output_dir,paste0("statcan_correspondence_",year,"_",level,".parquet")),
                                 compression="zstd",
                                 # nanoparquet does not compress at its default zstd level
                                 options=nanoparquet::parquet_options(compression_level=19))
  }
}
