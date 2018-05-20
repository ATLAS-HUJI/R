# Running Window implementation of the Paige Saunders Kalman filter for ATLAS SQLite data
# data is assumed to be a single track of one individual
# Original code by Sivan Toledo 2017
# Converted to R by Ingo Schiffner 2017

source('../SQLITE/ATLAS_SQLite_GetLoc.R')
source('PSKF.R')
source('PSKFForATLAS.R')
library(plotly)

behav_win <- 1*60*1000 #behavioural time window in ms (1 minute in this example; pick a window that suits your data)
ini_win <- behav_win/4 # initialization window for the Kalman filter

#select database file
sqlfile <- choose.files(default = "", caption = "Select SQLite Database", multi = FALSE, filters = c(".sqlite", "*.*"))
t_dat <- ATLAS_SQLite_GetLoc(sqlfile)
o_dat <- t_dat
#initialise min and max time 
tmin <- min(t_dat$TIME)
tmax <- max(t_dat$TIME)

#initialise results
kf_dat <- NULL
psq <- NULL

#get time sequence
tsq <- seq(tmin, tmax-behav_win, by=behav_win)

for (h in 1:length(tsq))
{
  #apply behavioural time window
  i <- as.double(tsq[h])
  if (i == tmin) {
    tdx <- which(t_dat$TIME >= i & t_dat$TIME < (i + behav_win))
  } else {
    tdx <- which(t_dat$TIME >= (i - ini_win) & t_dat$TIME < (i + behav_win))
  }
  if (length(tdx) > 0)
  {
    #filter data
    kf_tmp <- PSKFForATLAS(t_dat[tdx,])
    
    #reset time index 
    tdx <- which(kf_tmp$TIME >= i & kf_tmp$TIME < (i + behav_win))
    kf_tmp <- kf_tmp[tdx,]
    kf_dat <- rbind(kf_dat,kf_tmp)
    #transitions for plot
    psq <- c(psq,nrow(kf_dat))
  }
}
#rerun filter with offset 

#initialise min and max time 
tmin <- min(kf_dat$TIME) - behav_win/2
tmax <- max(kf_dat$TIME) + behav_win/2

#initialise results
okf_dat <- kf_dat
t_dat<- kf_dat#[kf_dat$TYPE==1,]
kf_dat <- NULL
nsq <- NULL
#get time sequence
tsq <- seq(tmin, tmax-behav_win, by=behav_win)

for (h in 1:length(tsq))
{
  #apply behavioural time window
  i <- as.double(tsq[h])
  if (i == tmin) {
    tdx <- which(t_dat$TIME >= i & t_dat$TIME < (i + behav_win))
  } else {
    tdx <- which(t_dat$TIME >= (i - ini_win) & t_dat$TIME < (i + behav_win))
  }
  if (length(tdx) > 0)
  {
    #filter data
    kf_tmp <- PSKFForATLAS(t_dat[tdx,])
    
    #reset time index 
    tdx <- which(kf_tmp$TIME >= i & kf_tmp$TIME < (i + behav_win))
    kf_tmp <- kf_tmp[tdx,]
    kf_dat <- rbind(kf_dat,kf_tmp)
    #transitions for plot
    nsq <- c(nsq,nrow(kf_dat))
  }
}

#plot
kf_dat_full <- PSKFForATLAS(o_dat)
p<-plot_ly() %>%
  add_trace(data=kf_dat_full, x=~X, y=~Y,type = 'scatter', mode = 'lines', line=list(color='black'), name='Kalman filter Complete') %>%
  add_trace(data=okf_dat, x=~X, y=~Y,type = 'scatter', mode = 'markers', marker=list(symbol="x",color='red'), name='Kalman filter RW Stage 1') %>%
  add_trace(data=kf_dat, x=~X, y=~Y,type = 'scatter', mode = 'markers', marker=list(symbol="x",color='blue'), name='Kalman filter RW Stage 2') %>%
  add_markers(data=okf_dat[psq,], x=~X, y=~Y,type = 'scatter', mode = 'markers', marker=list(symbol="circle-open", color='red', size=15), name='Transition Stage 1') %>%
  add_markers(data=kf_dat[nsq,], x=~X, y=~Y,type = 'scatter', mode = 'markers', marker=list(symbol="circle-open", color='blue', size=15), name='Transition Stage 2') %>%
  layout(xaxis = list(title="X"),
         yaxis = list(title="Y"))
p