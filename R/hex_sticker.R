#' Internal function to generate hex sticker
#' @keywords internal
generate_cancensus_sticker <- function() {
  tongfen_data <- tongfen::get_tongfen_census_da(regions=list(CSD="5915022"),
                                                 vectors=c(lico_2006="v_CA06_1981",lico_2016="v_CA16_2555"),
                                                 geo_format='sf')
  map_data <- tongfen_data %>%
    mutate(c06=map(.data$TongfenUID,function(d)strsplit(d," ") %>%
                     map(first) %>% unlist %>% strsplit(.,",") %>%
                     map(length) %>% unlist) %>% unlist,
           c16=map(.data$TongfenUID,function(d)strsplit(d," ") %>%
                     map(last) %>% unlist %>% strsplit(.,",") %>%
                     map(length) %>% unlist) %>% unlist) %>%
    mutate(t=c06+c16) %>%
    filter(c06>1,c16>1) %>%
    filter(t==max(t)-2)

  tongfen_core <- c("59153821","59153825","59153826","59151429","59151430","59151436","59151428","59151437")
  tongfen_core <- c("59153821","59153825","59153826","59151429","59151430","59151436","59151428","59151437","59151432","59151420","59151419","59150336","59150337","59151433","59151434","59151435")

  in_core <- function(TongfenUID) {
    grepl(paste0(tongfen_core,collapse = "|"),TongfenUID)
  }

  map_data <- tongfen_data %>%
    filter(in_core(TongfenUID)) %>%
    mutate(lico_change=lico_2016/lico_2006)

  id06 <- map_data$TongfenUID %>% strsplit(" ") %>% map(first) %>% unlist  %>% strsplit(":") %>% map(last) %>% unlist %>% strsplit(",") %>% unlist
  id16 <- map_data$TongfenUID %>% strsplit(" ") %>% map(last) %>% unlist  %>% strsplit(":") %>% map(last) %>% unlist %>% strsplit(",") %>% unlist

  map_data_06 <- cancensus::get_census("CA06",regions=list(DA=id06),vectors=c(lico_2006="v_CA06_1981"),geo_format="sf")
  map_data_16 <- cancensus::get_census("CA16",regions=list(DA=id16),vectors=c(lico_2016="v_CA16_2555"),geo_format="sf")


  g06 <- ggplot(map_data_06) + geom_sf(aes(fill=lico_2006),color="grey80") + scale_fill_viridis_c(guide=FALSE) + coord_sf(datum=NA)+ theme_void()
  g16 <- ggplot(map_data_16) + geom_sf(aes(fill=lico_2016),color="grey80") + scale_fill_viridis_c(guide=FALSE) + coord_sf(datum=NA)+ theme_void()
  gtf <- ggplot(map_data) + geom_sf(aes(fill=lico_change),color="grey80") + scale_fill_viridis_c(guide=FALSE,option="magma") + coord_sf(datum=NA)+ theme_void()

  p <- ggplot() +
    theme_void() +
    annotation_custom(ggplotGrob(g06),xmin=0.5,xmax=1,ymin=0.55,ymax=1) +
    annotation_custom(ggplotGrob(g16),xmin=0,xmax=0.5,ymin=0.55,ymax=1) +
    annotation_custom(ggplotGrob(gtf),xmin=0.25,xmax=0.75,ymin=0,ymax=0.5) +
    geom_curve(data=tibble(d=1),x=0.15,xend=0.3,y=0.58,yend=0.3,size=1,lineend="round",
               arrow = arrow(length = unit(0.1,"inches")), curvature=0.1,color="white") +
    geom_curve(data=tibble(d=1),x=0.8,xend=0.65,y=0.58,yend=0.3,size=1,lineend="round",
               arrow = arrow(length = unit(0.1,"inches")), curvature=-0.1,color="white") +
  geom_curve(data=tibble(d=1),x=0.15,xend=0.3,y=0.58,yend=0.3,size=0.7,lineend="round",
             arrow = arrow(length = unit(0.1,"inches")), curvature=0.1) +
    geom_curve(data=tibble(d=1),x=0.8,xend=0.65,y=0.58,yend=0.3,size=0.7,lineend="round",
               arrow = arrow(length = unit(0.1,"inches")), curvature=-0.1)




  hexSticker::sticker(p, package="tongfen", p_size=8, p_y = 1.6,p_x=1.03,
                      s_x=1.07, s_y=0.78,
                      s_width=1.5, s_height=1.5,
                      #s_x=1, s_y=0.75,
                      #s_width=2.5, s_height=2.5,
                      #h_color="#FF0000",
                      h_color="#4B0082",
                      h_fill="grey30",
                      p_color="white",
                      #white_around_sticker = TRUE,
                      filename=here::here("images/tongfen-sticker.png"),
                      dpi=600)
}

#' @importFrom dplyr %>%
