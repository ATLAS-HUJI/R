# Simple implementation of the Paige Saunders Kalman filter for ATLAS SQLite data
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

kf_dat <- PSKFForATLAS(t_dat)
pal <- c("green", "red")
p<-plot_ly(colors=pal) %>%
  add_trace(data=t_dat, x=~X, y=~Y,type = 'scatter', mode = 'lines+markers', line=list(color='black'), marker=list(symbol="x"), name='Unfiltered localizations') %>%
  add_markers(data=kf_dat, x=~X, y=~Y,type = 'scatter', mode = 'markers', color=~Std, marker=list(symbol="circle"), text = ~paste("STD: ", Std), name='Kalman filtered data') %>%
  layout(xaxis = list(title="X"),
         yaxis = list(title="Y"))
p