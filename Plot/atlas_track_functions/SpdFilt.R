#speed based filter by Ingo Schiffner 2017
#calculates speed between consecutive localizations and filters out segments exceeding spd_lim
#assumes x and y coordinates are given in a projected format in meters and time(t) given as ATLASTimeStamp(ms)
#returns a data.frame containing filtered x,y and time(t)
SpdFilt <- function(x,y,t,spd_lim) 
{
  dx = diff(x,1)
  dy = diff(y,1)
  dt = diff(t,1)/1000
  de = (dx^2 + dy^2) ^ 0.5
  spd = de/dt
  spd = c(0,spd)
  xr= x[spd<=spd_lim]
  yr= y[spd<=spd_lim]
  tr= t[spd<=spd_lim]
  sdf <- data.frame(x=as.numeric(xr),y=as.numeric(yr),t=as.numeric(tr)) 
  return (sdf)
}