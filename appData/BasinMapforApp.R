library(leaflet)
library(leaflet.extras)
library(jpeg)
library(sp)
library(rgdal)


#Function to convert latlong to UTM
LongLatToUTM<-function(x,y){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")  ## for example
  res <- spTransform(xy, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  return(as.data.frame(res))
}

# Plot a default web map (brackets display the result) --------------------


bsns <- readOGR(dsn = 'appData/PSP_Locations',
                layer = 'All_PSPBasins_08042016',
                verbose = FALSE)
stns <- readOGR(dsn = 'appData/PSP_Locations',
                layer = 'XYPSPStations_2017_09_28',
                verbose = FALSE)

bsns@data$PSP_Name <- as.character(bsns@data$PSP_Name)
bsns@data[grep("Hood", bsns@data$PSP_Name),]$PSP_Name <- "Hood River"
bsns@data[grep("Walla Walla", bsns@data$PSP_Name),]$PSP_Name <- "Walla Walla"
bsns@data[grep("Middle Rogue", bsns@data$PSP_Name),]$PSP_Name <- "Middle Rogue"
bsns@data[grep("Clackamas", bsns@data$PSP_Name),]$PSP_Name <- "Clackamas"
bsns@data[grep("Pudding", bsns@data$PSP_Name),]$PSP_Name <- "Pudding"
bsns@data[grep("Amazon", bsns@data$PSP_Name),]$PSP_Name <- "Amazon"
bsns@data[grep("South Yamhill", bsns@data$PSP_Name),]$PSP_Name <- "South Yamhill"
bsns@data[grep("Wasco", bsns@data$PSP_Name),]$PSP_Name <- "Wasco"
bsns@data[grep("South Umpqua", bsns@data$PSP_Name),]$PSP_Name <- "South Umpqua"
bsns@data[grep("Middle Deschutes", bsns@data$PSP_Name),]$PSP_Name <- "Middle Deschutes"
bsns@data[grep("South Coast", bsns@data$PSP_Name),]$PSP_Name <- "South Coast"
unique(bsns@data$PSP_Name)


detectColor <- colorBin(palette = c('#a0fff8', '#44a1ff', '#ffb744', '#ff311e'),
                        domain = NULL, bins = c(0,0.1,0.5,1,Inf))
BenchColor <- colorBin(palette = c('black', 'red'), domain = NULL, na.color = 'grey', bins = c(0,1,Inf))
espg4326 <- leafletCRS(crsClass = "L.Proj.CRS", code = 'EPSG:4326',
                       proj4def = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0',
                       resolutions = c(8192, 4096, 2048))

detectMap <- cmpfun(function(inputData, bsnData, stnData) {
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
    addWMSTiles(baseUrl = 'https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Land_Cover_L48/wms?',
                group = "Land Use (NLCD 2016) *Colors do not <br>
                                       represent detections (see legend above map)",
                layers = "NLCD_2016_Land_Cover_L48",
                options = WMSTileOptions(version = '1.3.0',
                                         format = 'image/png',
                                         transparent = TRUE)) %>% 
    addWMSTiles("https://nassgeodata.gmu.edu/CropScapeService/wms_cdlall.cgi?",
                group = "Crops (CDL 2017)",
                layers = "cdl_2017",
                options = WMSTileOptions(crs = espg4326,
                                         format = 'image/png',
                                         version = '1.1.1',
                                         request = 'GetMap',
                                         transparent = TRUE)
    ) %>%
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
                     clusterOptions = markerClusterOptions(showCoverageOnHover = TRUE, spiderfyOnMaxZoom = TRUE),
                     # layerId = inputData$Station_Description,
                     label = ~paste(sep = "  ", paste0("Date = ", inputData$Sampling_Date),
                                    paste0("ALR = ", inputData$AQL_Ratio)),
                     labelOptions = labelOptions(textsize = "12pt",
                                                 style = list(
                                                   # "font-weight" = "bold",
                                                   padding = "3px 8px"))
    ) %>% 
    addMarkers(data = stnData,
               group = 'Stations',
               lng = stnData@data$LONG,
               lat = stnData@data$LAT,
               # clusterOptions = markerClusterOptions(),
               options = markerOptions(riseOnHover = TRUE,
                                       keyboard = TRUE),
               label = stnData@data$Station_De,
               layerId = stnData@data$Station_De,
               popup = paste0(stnData@data$Station_Id, " ", stnData@data$Station_De)
    ) %>%
    addPolygons(data = bsnData,
                group = 'Basins',
                stroke = TRUE,
                weight = 1,
                opacity = 0.4,
                smoothFactor = 0.5,
                fillColor = topo.colors(13, alpha = NULL),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  # dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = FALSE),
                layerId = bsnData@data$PSP_Name,
                label = bsnData@data$PSP_Name,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
    ) %>%
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
    addLayersControl(overlayGroups = c('Stations', 'Detections', 'Basins', "Land Use (NLCD 2016) *Colors do not <br>
                                       represent detections (see legend above map)", "Crops (CDL 2017)", "Streams (NHD)", "Hydrography"),
                     baseGroups = c("Street", "World Imagery"),
                     position = 'bottomleft'
    ) %>%
    hideGroup(group = c('Stations', 'Land Use (NLCD 2016) *Colors do not <br>
                                       represent detections (see legend above map)', 'Crops (CDL 2017)', 'Streams (NHD)', "Hydrography"))
  map
  
})
