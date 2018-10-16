#running median filter by Ingo Schiffner 2017
#calculates median x y localizations and time (t) over a predefined window (win) in ms in predefined time steps (stp)
#assumes x and y coordinates are given in a projected format in meters and time(t) given as ATLASTimeStamp(ms)
#returns a data.frame containing filtered x,y and time(t) and n number of samples used to calculate for each time step
RunMedFilt <- function(x,y,t,med_win,med_stp) 
{
  rmt <- NULL
  rmx <- NULL
  rmy <- NULL
  rmn <- NULL
  i <- 1
  while (i < length(t))
  {
    #get window
    nt <- which(t >= (t[i]+med_win))
    if (!is.na(nt[1])) {
      nt <- nt[1]
      
      #calculate median
      rmt<-c(rmt,median(t[i:nt]))
      rmx<-c(rmx,median(x[i:nt]))
      rmy<-c(rmy,median(y[i:nt]))
      rmn<-c(rmn,nt-i)
      
      #get next step
      ni <- which(t >= (t[i]+med_stp))
      i <- ni[1]
    } else {
      break
    }
  }
  rmdf <- data.frame(x=as.numeric(rmx),y=as.numeric(rmy),t=as.numeric(rmt),n=as.numeric(rmn)) 
}