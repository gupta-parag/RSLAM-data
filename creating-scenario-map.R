library(sf)
library(leaflet)
library(stringr)
library(foreign)
library(cartography)
library(htmlwidgets)

alt_name <- "alt_secondary"

########################## reading all the files #######################################
#geographyFile <- read.csv('C:\\Projects_2023\\land-use-model\\scenario\\TazTag.csv')
baseYear <- read.dbf('C:\\Projects_2023\\land-use-model\\scenario\\landUseData\\RTC_LANDUSE_2017.dbf')
horYear <- read.dbf('C:\\Projects_2023\\land-use-model\\scenario\\landUseData\\RTC_LANDUSE_2045.dbf')
taz_sf <- st_read(str_c("C:\\R_SLAM_2\\BaseLU\\",alt_name, "\\Output"),
                        layer = "tazx")
tdm_file <- read.dbf(str_c("C:\\R_SLAM_2\\BaseLU\\",alt_name,
                           "\\Output\\RSlam_output_",alt_name,".dbf"))
###############################################################################
outerLoopTAZs <- geographyFile$Model_TAZ[geographyFile$SecondaryZones == 1 ]



x <- tdm_file[tdm_file$N %in% outerLoopTAZs,c(1,4,8,10,14)] # tazs of outerloop

y <- merge(x,baseYear[,c(1,8,10,14)], by = "N", suffixes = c("_MODEL", "_17"))
z <- merge(y,horYear[,c(1,8,10,14)], by = "N")




z_sf <- st_as_sf(left_join(z, taz_sf[,1], by = c("N" = "MODEL_TAZ")),
                 sf_column_name = "geometry")
                 

leaflet() %>%
  addTiles() %>%
  addPolygons(data = st_transform(z_sf,4326))

                  

#baseline and scenario - pop_hh, hh difference
labels <- sprintf(
  "<strong>MODEL TAZ</strong> : %s<br/><strong>County </strong>: %s<br/>
  <strong>Scenario Pop </strong>: %s<br/>
  <strong>2050 Pop</strong> : %s<br/>
   <strong>2017 Pop</strong> : %s<br/>
  <strong>Scenario HH </strong>: %s<br/>
  <strong>2050 HH</strong> : %s<br/>
   <strong>2017 HH</strong> : %s<br/>",
  z_sf$N, z_sf$JUR,z_sf$POP_HH_MODEL, z_sf$POP_HH,z_sf$POP_HH_17,
  z_sf$HH_MODEL,z_sf$HH,z_sf$HH_17) %>%
  lapply(htmltools::HTML)

# getting breaks and coloring

bks <- getBreaks(v = z_sf$POP_HH_MODEL ,
          nclass = 5,
          method = "jenks")



 pal <- colorBin("YlOrRd", domain = z_sf$POP_HH_MODEL, bins = bks)

map <-   leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = st_transform(z_sf,4326),
              weight = 2 , 
              label = labels,
              fillColor =  ~pal(z_sf$POP_HH_MODEL),
              fillOpacity = 0.7,
              color = "black",
              dashArray = "3",
              highlightOptions = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.4,
                bringToFront = TRUE)) %>%
  addLegend(pal = pal, values = z_sf$POP_HH_MODEL, 
            opacity = 0.7, title = NULL, 
            position = "topright") %>%
  addScaleBar()

saveWidget(map, "secondary.html")

