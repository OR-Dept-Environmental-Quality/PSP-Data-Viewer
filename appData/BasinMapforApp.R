library(leaflet)
library(leaflet.extras)
# library(jpeg)
# library(sp)
# library(rgdal)
library(sf)
library(leafpop)
library(ggplot2)


#Function to convert latlong to UTM
LongLatToUTM<-function(x,y){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")  ## for example
  res <- spTransform(xy, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  return(as.data.frame(res))
}

# Plot a default web map (brackets display the result) --------------------

# setwd('//deqhq1/psp/Rscripts/Repositories/PSP-Data-Viewer/')

# bsns <- readOGR(dsn = 'appData/PSP_Locations',
#                 layer = 'All_PSPBasins_01242023',
#                 verbose = FALSE)
# stns <- readOGR(dsn = 'appData/PSP_Locations',
#                 layer = 'XYPSP_stations_AWQMS',
#                 verbose = FALSE)
bsns <- st_read(dsn = 'appData/PSP_Locations',
                layer = 'All_PSPBasins_01242023')
stns <- st_read(dsn = 'appData/PSP_Locations',
                layer = 'XYPSP_stations_AWQMS')
# stn_ws <- readOGR(dsn = 'appData/PSP_Locations',
#                 layer = 'Station_Watersheds_nhd',
#                 verbose = FALSE,
#                 stringsAsFactors = FALSE)

stn_ws <- sf::st_read(dsn = 'appData/PSP_Locations',
                      layer = 'Station_Watersheds')

stn_ws <- dplyr::rename(stn_ws, Station = Name)

stn_ws <- sf::st_transform(stn_ws, crs = 4326)
bsns <- sf::st_transform(bsns, crs = 4326)

stn_landuse <- read.csv("appData/station_ws_landuse.csv", stringsAsFactors=FALSE)

stn_landuse <- dplyr::rename(stn_landuse, 
                      Station_ID = Name, 
                      Station_Description = StationDes, 
                      Project = psp_name, 
                      Agricultural = Ag, 
                      Forested = Forest)
# stn_landuse <- stn_landuse %>% mutate(Agricultural = Cultivated.Crops + Hay.Pasture,
#                                       Forested = Deciduous.Forest+ Evergreen.Forest+ Mixed.Forest,
#                                       Urban = Developed..High.Intensity+ Developed..Low.Intensity+ 
#                                                   Developed..Medium.Intensity+ Developed..Open.Space,
#                                       "Ag or Forest" = Herbaceous+ Shrub.Scrub,
#                                       Other = Barren.Land+ Emergent.Herbaceous.Wetlands+ Open.Water+ Perennial.Snow.Ice+ 
#                                                   Unclassified+ Woody.Wetlands,
#                                       total = Agricultural + Forested + Urban + `Ag or Forest` + Other)
stn_landuse <- stn_landuse[,c("Station_ID", "Station_Description", "Project", "Agricultural", "Forested", "Urban",
                              # "Ag.or.Forest", 
                              "Other")]
colnames(stn_landuse) <- c("Station_ID", "Station_Description", "Project", "Agricultural", "Forested", "Urban",
                           # "Ag or Forested", 
                           "Other")
stn_landuse <- stn_landuse %>% gather(key = "Land Use", value = "Percent", 4:7)
stn_landuse$`Land Use` <- ordered(stn_landuse$`Land Use`, 
                                    levels = c("Agricultural", "Forested", "Urban", 
                                               # "Ag or Forested", 
                                               "Other"))
# stn_ws <- sp::merge(x=stn_ws, y=stn_landuse, by.x = "Station", by.y = "Name", all.x = TRUE)
landuse_plot <- function(stn){
  # p <- plot_ly(filter(stn_landuse, Station_ID == stn)) %>% 
  #   plotly::add_pie(values = ~Percent, labels = ~`Land Use`, textinfo = 'label+percent', showlegend = FALSE)
  p <- ggplot(data = dplyr::filter(stn_landuse, Station_ID == stn), aes(x = `Land Use`, y = Percent, fill = `Land Use`))+
                geom_bar(stat = "identity")+
    ggplot2::ylim(c(0,100))+
    ggplot2::theme_classic()+
    ggplot2::ggtitle(paste(stn, "Land Use"))+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))


  return(p)
}
# plot_ly(data = filter(stn_landuse, Station_ID == "10434-ORDEQ")) %>% 
#      plotly::add_pie(values = ~Percent, labels = ~`Land Use`, textinfo = 'label+percent', showlegend = FALSE)
# stn_landuse_plots <- lapply(stn_ws@data$Station, landuse_plot)

bsns$PSP_Name <- as.character(bsns$PSP_Name)
bsns[grep("Hood", bsns$PSP_Name),]$PSP_Name <- "Hood River"
bsns[grep("Walla Walla", bsns$PSP_Name),]$PSP_Name <- "Walla Walla"
bsns[grep("Middle Rogue", bsns$PSP_Name),]$PSP_Name <- "Middle Rogue"
bsns[grep("Clackamas", bsns$PSP_Name),]$PSP_Name <- "Clackamas"
bsns[grep("Pudding", bsns$PSP_Name),]$PSP_Name <- "Pudding"
bsns[grep("Amazon", bsns$PSP_Name),]$PSP_Name <- "Amazon"
bsns[grep("South Yamhill", bsns$PSP_Name),]$PSP_Name <- "South Yamhill"
bsns[grep("Wasco", bsns$PSP_Name),]$PSP_Name <- "Wasco"
bsns[grep("South Umpqua", bsns$PSP_Name),]$PSP_Name <- "South Umpqua"
bsns[grep("Middle Deschutes", bsns$PSP_Name),]$PSP_Name <- "Middle Deschutes"
bsns[grep("South Coast", bsns$PSP_Name),]$PSP_Name <- "South Coast"
unique(bsns$PSP_Name)


detectColor <- colorBin(palette = c('#a0fff8', '#44a1ff', '#ffb744', '#ff311e'),
                        domain = NULL, bins = c(0,0.1,0.5,1,Inf))
BenchColor <- colorBin(palette = c('black', 'red'), domain = NULL, na.color = 'grey', bins = c(0,1,Inf))
espg4326 <- leafletCRS(crsClass = "L.Proj.CRS", code = 'EPSG:4326',
                       proj4def = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0',
                       resolutions = c(8192, 4096, 2048))

detectMap <- cmpfun(function(inputData, bsnData, stnData, ws) {
  # stn_ws@data <- filter(stn_ws@data, Station %in% stnData@data$MLocID)
  map <- leaflet() %>%
    addProviderTiles(providers$OpenStreetMap, group = "Street") %>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
    # addWMSTiles('https://www.mrlc.gov/arcgis/services/LandCover/USGS_EROS_LandCover_NLCD/MapServer/WMSServer?',
    #             group = "Land Use (NLCD 2011) *Colors do not <br>
    #                                    represent detections (see legend above map)",
    #             layers = '33',
    #             options = WMSTileOptions(format = 'image/png',
    #                                      version = '1.3.0',
    #                                      transparent = TRUE)
    # ) %>%
    addWMSTiles(baseUrl = 'https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/wms?',
                group = "Land Use (NLCD 2019) *Colors do not <br>
                                       represent detections (see legend above map)",
                layers = "NLCD_2019_Land_Cover_L48",
                options = WMSTileOptions(version = '1.3.0',
                                         format = 'image/png',
                                         transparent = TRUE)) %>% 
    # addWMSTiles("https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi?",
    #             group = "Crops (CDL 2020)",
    #             layers = "cdl_2020",
    #             options = WMSTileOptions(crs = espg4326,
    #                                      format = 'image/png',
    #                                      version = '1.1.1',
    #                                      request = 'GetMap',
    #                                      transparent = TRUE)
    # ) %>%
    addWMSTiles('https://hydro.nationalmap.gov/arcgis/services/nhd/MapServer/WMSServer?',
                group = "Streams (NHD)",
                layers = c('4','5'),
                options = WMSTileOptions(format = 'image/png',
                                         version = '1.3.0',
                                         transparent = TRUE)
    ) %>% 
    addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", 
                      group = "Hydrography", 
                      options = WMSTileOptions(format = "image/png", 
                                               transparent = TRUE),
                      layers = "0"
    ) %>%
    addCircleMarkers(data = inputData,
                     group = 'Detections',
                     lng = inputData$DECIMAL_LONG,
                     lat = inputData$DECIMAL_LAT,
                     fillColor = detectColor(inputData$AQL_Ratio),
                     color = detectColor(inputData$AQL_Ratio),
                     opacity = 1,
                     fillOpacity = 1,
                     stroke = TRUE,
                     clusterOptions = markerClusterOptions(showCoverageOnHover = TRUE, spiderfyOnMaxZoom = FALSE),
                     layerId = inputData$Station_Description,
                     label = ~paste(sep = "  ", paste0("Date = ", inputData$Sampling_Date),
                                    paste0("ALR = ", inputData$AQL_Ratio)),
                     labelOptions = labelOptions(textsize = "12pt",
                                                 style = list(
                                                   # "font-weight" = "bold",
                                                   padding = "3px 8px"))
    ) %>% 
    addMarkers(data = stnData,
               group = 'Stations',
               # lng = stnData@data$Long_DD,
               # lat = stnData@data$Lat_DD,
               # clusterOptions = markerClusterOptions(),
               options = markerOptions(riseOnHover = TRUE,
                                       keyboard = TRUE),
               label = stnData$StationDes,
               layerId = stnData$StationDes,
               popup = paste0("<b>",stnData$MLocID, " ", stnData$StationDes, "</b><br>")
    ) %>%
    addPolygons(data = bsnData,
                group = 'Basins',
                stroke = TRUE,
                weight = 1,
                opacity = 0.4,
                fillOpacity = 0.1,
                smoothFactor = 0.5,
                fillColor = topo.colors(13, alpha = NULL),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  # dashArray = "",
                  fillOpacity = 0.3,
                  bringToFront = FALSE),
                layerId = bsnData$PSP_Name,
                label = bsnData$PSP_Name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
    ) %>%
    addPolygons(data = ws,
                group = 'Station upstream watersheds',
                stroke = TRUE,
                weight = 1,
                color = "#666",
                opacity = 1,
                fillOpacity = 0.1,
                smoothFactor = 0.5,
                label = ws$Station,
                fillColor = topo.colors(13, alpha = NULL),
                highlight = highlightOptions(
                  weight = 3,
                  color = "#000000",
                  # dashArray = "",
                  # fillOpacity = 0.4,
                  bringToFront = FALSE, sendToBack = TRUE)
    ) %>%
    # addPopupGraphs(stn_landuse_plots, group = 'Stations') %>%
    # addLegend("bottomright",
    #           title = if(length(unique(inputData$Analyte)) > 1){'All Analytes ALR'
    #           } else {paste0(unique(inputData$Analyte), " ALR")},
    #           values = inputData$AQL_Ratio,
    #           pal = colorBin(palette = c('#a0fff8', '#44a1ff', '#ffb744', '#ff311e'),
    #                          bins = c(0,0.1,0.5,1,Inf)),
    #           labels = c('0','10','50','100'), opacity = 1.0
    # ) %>% 
    addFullscreenControl(position='topright') %>% 
    # addSearchFeatures(targetGroups = 'Stations') %>% 
    addLayersControl(overlayGroups = c('Stations','Station upstream watersheds', 'Detections', 'Basins', "Land Use (NLCD 2019) *Colors do not <br>
                                       represent detections (see legend above map)", 
                                       # "Crops (CDL 2020)", 
                                       "Streams (NHD)", "Hydrography"),
                     baseGroups = c("Street", "World Imagery"),
                     position = 'bottomleft'
    ) %>%
    hideGroup(group = c('Land Use (NLCD 2019) *Colors do not <br>
                                       represent detections (see legend above map)', 
                        # 'Crops (CDL 2020)', 
                        'Streams (NHD)', "Hydrography", "Basins"))
  map
  
})
