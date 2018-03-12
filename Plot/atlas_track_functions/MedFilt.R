#basic median filter by Ingo Schiffner 2017
#calculates median x y localizations and time (t) over a predefined window (win) in ms
#assumes x and y coordinates are given in a projected format in meters and time(t) given as ATLASTimeStamp(ms)
#returns a data.frame containing filtered x,y and time(t)
MedFilt <- function(x,y,t,win) 
{
  #set bins
  tb = trunc((t - t[1])/win)+1
  mx <- aggregate(x ~ tb, FUN = median)
  my <- aggregate(y ~ tb, FUN = median)
  mt <- aggregate(t ~ tb, FUN = median)
  mtf <- data.frame(x=as.numeric(mx$x),y=as.numeric(my$y),t=as.numeric(mt$t)) 
  return (mtf)
}