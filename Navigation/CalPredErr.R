# function to predict timeseries t2 using time series t1

# INPUT:
# input are 2 N*M matrices containting M dimensional positional information (presumed to be ordered in time)
# and 2 (N-1)*(M-1) matrices containing (M-1) dimensional angular data (derived from the positional information)
# t1 and t2 have to be sampled at the same rate
# the prediction algorithm uses the positional information to find nearest neighbours between the two time series, 
# but uses directional information for prediction, to avoid issues with data not beeing bounded

# OUTPUT:
# output is a data frame containing the prediction error for every time step. i.e. the divergence of the two time 
# series from each nearest neighbour and an estimate base error by predicting error from a randomly chosen point 
# instead from the nearest neighbour.

# INTERPRETATION:
# If t1 and t2 are based on deterministic principles, e.g. 2 trajectories of an animal following the same sequence 
# of landmarks from a common starting location to a common goal location, commonly referred to as piloting. Then the 
# prediction error(predicting t2 based on nearest neighbours in t1) should never exceed the base error (predicting t2 
# by picking random points). If the base error exceeds the prediction error it can be concluded that the strategy 
# applied in t1 is not the same as in t2. On top of that the prediction error itself serves as a much more robust
# estimate of similarity because it does not only consider the closeness (like simple nearest neighbour approaches), 
# but also takes into account the dynamics observed in both trajectories

# code by Ingo Schiffner (2018)

CalPredErr <- function(t1,t2,h1,h2) 
{ 
  t1<-as.matrix(t1)
  t2<-as.matrix(t2)
  
  h1<-as.matrix(h1)
  h2<-as.matrix(h2)
  
  p_err_mat <- matrix(data=NA, nrow = nrow(t1) , ncol = nrow(t2))
  b_err_mat <- matrix(data=NA, nrow = nrow(t1) , ncol = nrow(t2))
  
  #loop through each point
  for (i in 1:nrow(t1))
  {
    #find nearest neighbour based on positional data
    dt <- matrix(data=NA, nrow = nrow(t2) , ncol = ncol(t1))
    for (j in 1:ncol(t1))
    {
      dt[,j]=(t2[,j]-t1[i,j])^2
    }
    dt <- rowSums(dt)^0.5 
    hp <- which(dt==min(dt))
    hp <- hp[1]
    
    #predict based on heading
    ii <- 0
    for (h in hp:nrow(h2))
    {
      if (i+ii<nrow(h1))
      {
        dpe <- matrix(data=NA, nrow = 1, ncol = ncol(h2))
        for (j in 1:ncol(h1))
        {
          dpe[j]<-(h1[i+ii,j]-h2[h,j])^2
        }
        p_err_mat[i,h] = sum(dpe)^0.5
        ii <- ii + 1
      }
    }
    
    ii <- 0
    rp <- round(runif(1)*(nrow(h2)-1))+1
    for (h in rp:nrow(h2))
    {
      if (i+ii<nrow(h1))
      {
        bpe <- matrix(data=NA, nrow = 1 , ncol = ncol(h2))
        for (j in 1:ncol(h1))
        {
          bpe[j]<-(h1[i+ii,j]-h2[h,j])^2
        }
      }
      b_err_mat[i,h] = sum(bpe)^0.5
      ii <- ii + 1 
    }
  }
  base_err <- rowMeans(b_err_mat, na.rm=T)
  pred_err <- rowMeans(p_err_mat, na.rm=T)
  base_err <- base_err[1:length(pred_err)]
  prdf <- data.frame(base_err=as.numeric(base_err),pred_err=as.numeric(pred_err)) 
  return (prdf)
}