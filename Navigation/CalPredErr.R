# function to predict timeseries t2 using time series t1
# input are 2 N*M matrices containting M dimensional timeseries (without the time component)
# t1 and t2 have to be sampled at the same rate 
# code by Ingo Schiffner
CalPredErr <- function(t1,t2) 
{ 
  t1<-as.matrix(t1)
  t2<-as.matrix(t2)
  p_err_mat <- matrix(data=NA, nrow = nrow(t1) , ncol = nrow(t2))
  b_err_mat <- matrix(data=NA, nrow = nrow(t1) , ncol = nrow(t2))
  
  #loop through each point
  for (i in 1:nrow(t1))
  {
    #find nearest neighbour
    dt <- matrix(data=NA, nrow = nrow(t2) , ncol = ncol(t1))
    for (j in 1:ncol(t1))
    {
      dt[,j]=(t2[,j]-t1[i,j])^2
    }
    dt <- rowSums(dt)^0.5 
    hp <- which(dt==min(dt))
    hp <- hp[1]
    
    #predict based on nearest neighbour
    ii <- 0
    for (h in hp:nrow(t2))
    {
      if (i+ii<nrow(t1))
      {
        dpe <- matrix(data=NA, nrow = 1, ncol = ncol(t2))
        for (j in 1:ncol(t1))
        {
          dpe[j]<-(t1[i+ii,j]-t2[h,j])^2
        }
        p_err_mat[i,h] = sum(dpe)^0.5
        ii <- ii + 1
      }
    }
    
    ii <- 0
    rp <- round(runif(1)*(nrow(t2)-1))+1
    for (h in rp:nrow(t2))
    {
      if (i+ii<nrow(t1))
      {
        bpe <- matrix(data=NA, nrow = 1 , ncol = ncol(t2))
        for (j in 1:ncol(t1))
        {
          bpe[j]<-(t1[i+ii,j]-t2[h,j])^2
        }
      }
      b_err_mat[i,h] = sum(bpe)^0.5
      ii <- ii + 1 
    }
  }
  base_err <- rowMeans(b_err_mat, na.rm=T)
  pred_err <- rowMeans(p_err_mat, na.rm=T)
  prdf <- data.frame(base_err=as.numeric(base_err),pred_err=as.numeric(pred_err)) 
  return (prdf)
}