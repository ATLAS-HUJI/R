# Double Filter implementation of the Paige Saunders Kalman filter for ATLAS SQLite data
# data is assumed to be a single track of one individual
# Original code by Sivan Toledo 2017
# Converted to R by Ingo Schiffner 2017

source('../SQLITE/ATLAS_SQLite_GetLoc.R')
source('PSKF.R')
source('PSKFForATLAS.R')
library(plotly)

#select database file
sqlfile <- choose.files(default = "", caption = "Select SQLite Database", multi = FALSE, filters = c(".sqlite", "*.*"))
t_dat <- ATLAS_SQLite_GetLoc(sqlfile)

kf_p1 <- PSKFForATLAS(t_dat)

#remove predicted positions
kf_tmp<-kf_p1[kf_p1$TYPE==1,]

#filter data with high standard deviation
#kf_tmp<-kf_tmp[kf_tmp$Std<5,]
kf_p2 <- PSKFForATLAS(kf_tmp)

mean(kf_p1$Std)
mean(kf_p2$Std)

p<-plot_ly() %>%
  add_trace(data=t_dat, x=~X, y=~Y,type = 'scatter', mode = 'markers', marker=list(symbol="x", color='black'), name='Unfiltered localizations') %>%
  add_trace(data=kf_p1, x=~X, y=~Y,type = 'scatter', mode = 'lines', line=list(color='red'), name='Kalman filter stage 1') %>%
  add_trace(data=kf_p2, x=~X, y=~Y,type = 'scatter', mode = 'lines', line=list(color="blue"), name='Kalman filter stage 2') %>%
layout(xaxis = list(title="X"),
         yaxis = list(title="Y"))
p