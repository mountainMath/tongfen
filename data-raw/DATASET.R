## code to prepare `DATASET` dataset goes here


get_canada_federal_election_polling_division_geos <- function(year=2019){
  base_path=file.path(tempdir(),paste0("polling_division_boundaries_",year))
  if (year==2015) {
    url= "http://ftp.maps.canada.ca/pub/elections_elections/Electoral-districts_Circonscription-electorale/polling_divisions_boundaries_2015/polling_divisions_boundaries_2015_shp.zip"
    layer="PD_A"
  } else if (year==2019) {
    url="http://ftp.maps.canada.ca/pub/elections_elections/Electoral-districts_Circonscription-electorale/Elections_Canada_2019/polling_divisions_boundaries_2019.shp.zip"
    layer="PD_CA_2019_EN"
  } else {
    stop("Year needs to be 2015 or 2019")
  }
  if (!dir.exists(base_path)) {
    dir.create(base_path)
    tmp=tempfile()
    utils::download.file(url,tmp)
    fs <- utils::unzip(tmp,exdir = base_path)
  }
  path <- file.path(base_path,dir(base_path,pattern="\\.shp$"))
  #rgdal::ogrListLayers(path)
  d<-sf::read_sf(path, layer = layer) %>%
    mutate_at(vars(-any_of("geometry")), as.character)
}


get_canada_federal_election_polling_division_votes <- function(year=2019){
  base_path=file.path(tempdir(),paste0("polling_division_data_",year))
  if (year==2019) {
    url="https://www.elections.ca/res/rep/off/ovr2019app/51/data_donnees/pollresults_resultatsbureauCanada.zip"
  } else if (year==2015) {
    url="https://www.elections.ca/res/rep/off/ovr2015app/41/data_donnees/pollresults_resultatsbureauCanada.zip"
  }
  if (!dir.exists(base_path)) {
    tmp=tempfile()
    utils::download.file(url,tmp)
    utils::unzip(tmp,exdir = base_path)
  }
  dir(base_path,pattern="*.csv") %>%
    file.path(base_path,.) %>%
    lapply(readr::read_csv,col_type=readr::cols(.default="c")) %>%
    bind_rows
}

clean_poll_data <- function(data){
  data %>%
    select(FED_NUM=`Electoral District Number/Numéro de circonscription`,
           Merge=`Merge With/Fusionné avec`,
           no_poll=`No Poll Held Indicator/Indicateur de bureau sans scrutin`,
           void_poll=`Void Poll Indicator/Indicateur de bureau supprimé`,
           PD_NUM=`Polling Station Number/Numéro du bureau de scrutin`,
           Party=`Political Affiliation Name_English/Appartenance politique_Anglais`,
           Incumbent=`Incumbent Indicator/Indicateur_Candidat sortant`,
           Elected=`Elected Candidate Indicator/Indicateur du candidat élu`,
           Votes=`Candidate Poll Votes Count/Votes du candidat pour le bureau`) %>%
    filter(!is.na(PD_NUM)) %>%
    mutate(PDNUM=PD_NUM,PD_NUM=gsub('[^0-9.]$','',PD_NUM)) %>%
    mutate(Votes=as.integer(Votes))
}



data_2015 <- get_canada_federal_election_polling_division_votes(2015) %>%
  clean_poll_data() %>%
  mutate(PD_2015=paste0(FED_NUM,"_",coalesce(Merge,PD_NUM)))
data_2019 <- get_canada_federal_election_polling_division_votes(2019) %>%
  clean_poll_data() %>%
  mutate(PD_2019=paste0(FED_NUM,"_",coalesce(Merge,PD_NUM)))

geo_2015 <- get_canada_federal_election_polling_division_geos(2015) %>%
  sf::st_make_valid() %>%
  select(FED_NUM,PD_NUM,POLL_NAME,ADV_POLL) %>%
  mutate(AP_2015=paste0(FED_NUM,"_",ADV_POLL)) %>%
  left_join(data_2015 %>%
              select(FED_NUM,PD_NUM,PD_2015) %>%
              unique(),
            by=c("FED_NUM","PD_NUM"))

geo_2019 <- get_canada_federal_election_polling_division_geos(2019) %>%
  sf::st_make_valid() %>%
  select(FED_NUM=FEDNUM,PD_NUM=PDNUM,POLL_NAME=POLLNAME,ADV_POLL=ADVPOLLNUM) %>%
  mutate(AP_2019=paste0(FED_NUM,"_",ADV_POLL)) %>%
  left_join(data_2019 %>%
              select(FED_NUM,PD_NUM,PD_2019) %>%
              unique(),
            by=c("FED_NUM","PD_NUM"))

fed_num <- c("59034","59035","59036","59038","59039","59040")


cutout_geometry <- cancensus::get_census("CA16",regions=list(CSD=c("5915022","5915803"),
                                                             CT=c("9330069.01","9330069.02")),
                                         geo_format="sf") %>%
  summarise() %>%
  st_transform(st_crs(geo_2019))


vancouver_elections_data_2015 <- data_2015 %>% filter(FED_NUM %in% fed_num)
vancouver_elections_geos_2015 <- geo_2015 %>%
  filter(FED_NUM %in% fed_num) %>%
  st_intersection(cutout_geometry) %>%
  st_transform(mountainmathHelpers::lambert_conformal_conic_at(cutout_geometry))
vancouver_elections_data_2019 <- data_2019 %>% filter(FED_NUM %in% fed_num)
vancouver_elections_geos_2019 <- geo_2019 %>% filter(FED_NUM %in% fed_num) %>%
  st_intersection(cutout_geometry) %>%
  st_transform(mountainmathHelpers::lambert_conformal_conic_at(cutout_geometry))

usethis::use_data(vancouver_elections_data_2015, overwrite = TRUE)
usethis::use_data(vancouver_elections_geos_2015, overwrite = TRUE)
usethis::use_data(vancouver_elections_data_2019, overwrite = TRUE)
usethis::use_data(vancouver_elections_geos_2019, overwrite = TRUE)


