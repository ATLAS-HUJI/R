#function to determine track efficency over a set of x,y coordinates
#code by Ingo Schiffner
CalEff <- function (x,y)
{
  #remove na values
  x_n<-x[!is.na(x)]
  y_n<-y[!is.na(y)]
  
  #get track length
  xd<-diff(x_n)
  yd<-diff(y_n)
  TrackLength<-sum((xd^2 + yd^2)^0.5)
  
  #get beeline distance
  xb<-x_n[1]-x_n[length(x_n)]
  yb<-y_n[1]-y_n[length(y_n)]
  Beeline<-(xb^2 + yb^2)^0.5
  Efficiency<-round(Beeline/TrackLength *100)/100
  
  list(TrackLength,Beeline,Efficiency)
}