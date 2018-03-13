#Example script to estimate Home/Utilization Range using Time Local Covex Hull approach and compare areas to 
#determine interactions between individuals using library tlocoh 
#tlocoh by Lyons, A., Turner, W.C., and WM Getz. 2013. Home range plus: A space-time characterization of movement over real landscapes. BMC Movement Ecology 1:2, doi:10.1186/2051-3933-1-2.
#example code by Ingo Schiffner

# Custom functions require package RSQLite
source('ATLAS_SQLite_GetLoc.R')
source('ATLAS_SQLite_GetTag.R')

library(sp) # spatial points data frame
library(raster) # geographic data analysis and modeling package
library(tlocoh) # time local convex hull library

# Select database file
sqlfile <- choose.files(default = "", caption = "Select SQLite Database File", multi = FALSE, filters = c(".sqlite", "*.*"))
# Read entire database
SQLDATA <- ATLAS_SQLite_GetLoc(sqlfile)

#Create list of tags 
TagList <- unique(SQLDATA$TAG) 

#determine start and end time
ST = min(SQLDATA$TIME)
ET = max(SQLDATA$TIME)

bt_scl = 30 #behavioural time scale in seconds
res_h <- vector("list", length(TagList)) #polygons of home ranges (95% isopleth) for all tags

#create home ranges
for(tn in 1:length(TagList))
{
  #get data for tags
  t_dat = ATLAS_SQLite_GetTag(sqlfile,TagList[tn])
  tmp = data.frame(t_dat$X,t_dat$Y)
  t_xy <- SpatialPointsDataFrame(tmp,tmp)
  t_t <- as.POSIXct(t_dat$TIME/1000, tz="UTC", origin = "1970-01-01")
  
  #create coordinate set
  lc <- xyt.lxy(xy=t_xy@coords, dt=t_t, id=TagList[tn])
  
  #auto select scaling
  scl_set = lxy.plot.sfinder(lc, delta.t=bt_scl)
  med_scl = median(scl_set[[1]]$svals[[1]])
  
  #add nearest neighbours
  lc <- lxy.nn.add(lc, s=med_scl, k=20)
  
  #create hullset
  hl <- lxy.lhs(lc, k=20, s=med_scl)

  #create isopleths
  hl <- lhs.iso.add(hl)
  iso = isopleths(hl)
  iso95 <- iso[[1]]
  
  #isolate home range isopleth
  res_h[[tn]] <- iso95[iso95$iso.level==0.95,]
  
}

#example to compare home range between individuals and estimate overlap
#NOTE: this is a rough outline nothing is stored yet !!!
for  (tn in 1:length(TagList))
{
  #get data for tag 1
  t1_dat = ATLAS_SQLite_GetTag(sqlfile,TagList[tn])
  tmp = data.frame(t1_dat$X,t1_dat$Y)
  t1_xy <- SpatialPointsDataFrame(tmp,tmp)
  t1_t <- as.POSIXct(t1_dat$TIME/1000, tz="UTC", origin = "1970-01-01")
  
  for  (nx in tn+1:length(TagList))
  {
    #get data for tag 2
    t2_dat = ATLAS_SQLite_GetTag(sqlfile,TagList[tn])
    tmp = data.frame(t2_dat$X,t2_dat$Y)
    t2_xy <- SpatialPointsDataFrame(tmp,tmp)
    t2_t <- as.POSIXct(t2_dat$TIME/1000, tz="UTC", origin = "1970-01-01")
    
    #get joint area
    joint_area <- intersect(res_h[[tn]], res_h[[nx]])
    
    #joint area size
    aj = joint_area@polygons[[1]]@area
    
    #time and position of tag 1 in joint area
    j1 <- over(t1_xy, joint_area) # index of points in joint area
    vj1 <- t1_xy[!is.na(j1$area.1),] # localizations in joint area
    jt1 <- t1_t[!is.na(j1$area.1)] # time in joint area
    
    #time and position of tag 2 in joint area
    j2 <- over(t2_xy, joint_area)
    vj2 <- t2_xy[!is.na(j2$area.1),]
    jt2 <- t2_t[!is.na(j2$area.1)]
    
    #shared time in joint area 
    shr_t =intersect(jt1,jt2)
  }
}

