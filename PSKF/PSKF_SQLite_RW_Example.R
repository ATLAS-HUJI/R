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

#initialise min and max time 
tmin <- min(t_dat$TIME)
tmax <- max(t_dat$TIME)

#initialise results
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
kf_dat_full <- PSKFForATLAS(t_dat)
psq <- nsq+1
pal <- c("green", "red")
p<-plot_ly(colors=pal) %>%
  #add_trace(data=t_dat, x=~X, y=~Y,type = 'scatter', mode = 'lines+markers', line=list(color='black'), marker=list(symbol="x"), name='Unfiltered localizations') %>%
  add_trace(data=kf_dat_full, x=~X, y=~Y,type = 'scatter', mode = 'lines', line=list(color='red'), name='Kalman filter Complete') %>%
  add_markers(data=kf_dat, x=~X, y=~Y,type = 'scatter', mode = 'markers', color=~Std, marker=list(symbol="circle"), text = ~paste("STD: ", Std), name='Kalman filter RW') %>%
  add_markers(data=kf_dat[nsq,], x=~X, y=~Y,type = 'scatter', mode = 'markers', marker=list(symbol="circle-open", color='blue', size=15), name='Transition') %>%
  add_markers(data=kf_dat[psq,], x=~X, y=~Y,type = 'scatter', mode = 'markers', marker=list(symbol="circle-open", color='red', size=15), name='Transition') %>%
  layout(xaxis = list(title="X"),
         yaxis = list(title="Y"))
p