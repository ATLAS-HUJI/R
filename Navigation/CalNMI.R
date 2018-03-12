# function to calculate normalized mutual information between two variables
# input x and y have to be of equal length
# code by Ingo Schiffner
CalNMI <- function(x,y)
{
  library(infotheo)
  dp <- length(x)
  PX<-tabulate(x)
  PY<-tabulate(y)
  bin<-max(c(length(PX),length(PY)))
  PXY = matrix(data=0, nrow = bin , ncol = bin)
  
  for (i in 1:dp)
  {
    PXY[x[i], y[i]] <- PXY[x[i], y[i]] + 1
  }
  
  HX <- -sum(PX*log(PX/dp))/dp
  HY <- -sum(PY*log(PY/dp))/dp
  mi <- mutinformation(x,y)   # function for Mutual Information from package infotheo
  nmi <- mi/max(HX,HY)        # normalized mutual information
  
  return(nmi)
}