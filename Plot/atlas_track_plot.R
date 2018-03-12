#example for using leaflet for interactive display of atlas tracks from sqlite files

source('atlas_track_functions/ATLAS_SQLite_GetLoc.R')
source('atlas_track_functions/ATLAS_SQLite_GetTag.R')
source('atlas_track_functions/SpdFilt.R')
source('atlas_track_functions/MedFilt.R')

library(rgdal) # required for reading shape files
library(leaflet) # required for interactive map display

spd_lim = 105/3.6 # m/s estimate top speed for falco tinnunculus
med_win = 30000 #(milliseconds)

#select ATLAS database file
sqlfile = choose.files(default = "", caption = "Select SQLite Database", multi = FALSE, filters = c(".sqlite", "*.*"))
SQLDATA = ATLAS_SQLite_GetLoc(sqlfile)
TagList = unique(SQLDATA$TAG)

#projection string for conversion to wgs84 latitude and longitude format
wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#projection string for Israeli New Grid (ITM) format
itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"

#create color palette function
factpal <- colorFactor(topo.colors(length(TagList)), TagList)

ll<-leaflet() %>% addTiles() %>%
  #load options for background maps
  addProviderTiles("CartoDB.Positron", group="Positron") %>%
  addProviderTiles("Stamen.Toner", group = "Toner") %>%
  addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
  addProviderTiles('Esri.WorldShadedRelief', group = "Relief")

for(tn in 1:length(TagList))
{
  t_dat = ATLAS_SQLite_GetTag(sqlfile,TagList[tn])
      
  #apply speed filter data
  sf_dat <- SpdFilt(t_dat$X,t_dat$Y,t_dat$TIME,spd_lim)
      
  #apply median filter
  mf_dat <- MedFilt(sf_dat$x,sf_dat$y,sf_dat$t,med_win)
  
  pd <- mf_dat[,c(1,2)]
  pd <- pd[!is.na(pd$x),]
  colnames(pd) <- c("lon","lat")
  coordinates(pd)<-~lon+lat
  proj4string(pd)<-CRS(itm)
  llpd <- spTransform(pd, wgs84)
  ll<-addPolylines(ll,data=llpd@coords, group = paste(TagList[tn]) ,color=factpal(TagList[tn]), weight = 2, opacity = 1)

}
#layers control elements
ll<-addLayersControl(ll,baseGroups = c("Positron", "Toner", "Satellite", "Relief"), overlayGroups = c(TagList),
                 options = layersControlOptions(collapsed = FALSE))