#simple data filter example including a basic speed filter and a median filter

source('../SQLITE/ATLAS_SQLite_GetLoc.R')
source('RunMedFilt.R')
source('MedFilt.R')
source('SpdFilt.R')

#parameters
max_spd <- 50 #maximum speed in m/s (should be adjusted to the maximum speed of your animal)
med_win <- 30000 #median window in ms (should be adjusted to your sampling rate and the behavioral timeframe)
med_stp <- 10000 #median step width in ms for running median filter

#select database file
sqlfile = choose.files(default = "", caption = "Select SQLite Database", multi = FALSE, filters = c(".sqlite", "*.*"))
SQLDATA = ATLAS_SQLite_GetLoc(sqlfile)
TagList = unique(SQLDATA$TAG)

for(tn in 1:length(TagList))
{
  #get data
  t_dat <- subset(SQLDATA,SQLDATA$TAG==TagList[tn])
  plot(t_dat$X,t_dat$Y, col='red')
  
  #speed filter
  s_dat = SpdFilt(t_dat$X, t_dat$Y, t_dat$TIME, max_spd)
  points(s_dat$x,s_dat$y, col='green')
  
  #basic median filter
  m_dat = MedFilt(s_dat$x, s_dat$y, s_dat$t, med_win)
  lines(m_dat$x,m_dat$y)
  
  #runing median filter
  rm_dat = RunMedFilt(s_dat$x, s_dat$y, s_dat$t, med_win, med_stp)
  lines(rm_dat$x,rm_dat$y, col='blue')
}