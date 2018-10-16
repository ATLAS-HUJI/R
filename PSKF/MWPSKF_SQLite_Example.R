# Simple implementation of the Median Weighted Paige Saunders Kalman filter for ATLAS SQLite data
# data is assumed to be a single track of one individual
# Original idea by Sivan Toledo 2018, R implementation by Ingo Schiffner 2018

source('../SQLITE/ATLAS_SQLite_GetLoc.R')
source('PSKF.R')
source('PSKFForATLAS.R')
source('MWPSKFForATLAS.R')
source('RunMedFilt.R')

library(plotly)
library(sp)
library(leaflet) # required for interactive map display

#projection string for wgs84 latitude and longitude format
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#projection string for Israeli New Grid (ITM) format
itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"

#select database file
sqlfile <- choose.files(default = "", caption = "Select SQLite Database", multi = FALSE, filters = c(".sqlite", "*.*"))

atl_dat <- ATLAS_SQLite_GetLoc(sqlfile)
psk_dat <- PSKFForATLAS(atl_dat)
mkf_dat <- MWPSKFForATLAS(atl_dat)

atl_ll <- atl_dat[,c(4,5)] 
colnames(atl_ll)<- c('lon','lat')
coordinates(atl_ll)<-~lon+lat
proj4string(atl_ll)<-CRS(itm)
atl_ll <- spTransform(atl_ll, wgs84)

psk_ll <- psk_dat[,c(2,3)] 
colnames(psk_ll)<- c('lon','lat')
coordinates(psk_ll)<-~lon+lat
proj4string(psk_ll)<-CRS(itm)
psk_ll <- spTransform(psk_ll, wgs84)

mkf_ll <- mkf_dat[,c(2,3)] 
colnames(mkf_ll)<- c('lon','lat')
coordinates(mkf_ll)<-~lon+lat
proj4string(mkf_ll)<-CRS(itm)
mkf_ll <- spTransform(mkf_ll, wgs84)

ll<-leaflet() %>% addTiles() %>%
  #load options for background maps
  addProviderTiles("CartoDB.Positron", group="Positron") %>%
  addProviderTiles("Stamen.Toner", group = "Toner") %>%
  addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
  addPolylines(data=atl_ll@coords, group = 'atlas-raw' ,color='#f46b41', weight = 2, opacity = 0.5) %>%
  addPolylines(data=psk_ll@coords, group = 'atlas-kalman-filter' ,color='#419af4', weight = 2, opacity = 1) %>%
  addPolylines(data=mkf_ll@coords, group = 'median-weighted-pskf' ,color='#42f450', weight = 2, opacity = 1) %>%
  addLayersControl(baseGroups = c("Positron", "Toner", "Satellite"),overlayGroups = c("atlas-raw","atlas-kalman-filter","median-weighted-pskf"),
                       options = layersControlOptions(collapsed = FALSE))

ll