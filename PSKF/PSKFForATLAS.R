#helper/wrapper function for easy integration of PSKF for ATLAS data
# INPUT:
# ATLAS data with covariance matices as retrieved from the online database or an offline sqlite file
# OUTPUT:
# kalman filtered xy coordinates with standard deviation
PSKFForATLAS <- function (t_dat)
{
  # make sure data is ordered in time
  t_dat <- t_dat[order(t_dat$TIME),]
  
  # get first timestamp
  t0 <- t_dat$TIME[1]
  
  # estimate sampling rate from data
  dt <- round(min(diff(t_dat$TIME))/1000) *1000
  
  # kalman filter base settings
  S <- matrix(c( 1,0,1,0,0,1,0,1,0,0,1,0,0,0,0,1),nrow=4,ncol=4,byrow=T) # state transition matrix
  O <- matrix(c( 1,0,0,0,0,1,0,0),nrow=2,ncol=4,byrow=T)                 # observation matrix
  SDIM <- 4                                                              # state dimensions (x,y,.x,and.y)
  ODIM <- 2                                                              # observation dimensions (x and y)
  
  #build dataset
  n <- ceiling((t_dat$TIME[nrow(t_dat)]-t0)/dt)+1
  type <- numeric(n)
  j <- 1
  i <- 1
  t <- t0 - dt
  tmp_cov <- matrix(NA,2,2)
  tmp_y <- matrix(NA,2,1)
  tmp_t <- matrix(NA,1,1)
  
  observations <- rep(list(list(TIME=tmp_t,y=tmp_y,cov=tmp_cov)),n)
  while (j<=n)
  {
    t <- t + dt
    observations[[j]]$TIME <- t
    if (abs(t-t_dat$TIME[i])< 500)
    {
      t <- t_dat$TIME[i]
      type[j] <- 1
      observations[[j]]$y   <- matrix(c(t_dat$X[i],t_dat$Y[i]),nrow=1,ncol=2,byrow=T)
      observations[[j]]$cov <- matrix(c(t_dat$VARX[i],t_dat$COVXY[i],t_dat$COVXY[i],t_dat$VARY[i]),nrow=2,ncol=2,byrow=T)
      i=i+1
    }
    j=j+1
  }
  
  # apply Kalman filter
  s_cov <- diag(SDIM)
  estimates = PSKF(S, s_cov, O, observations)
  # store results to data frame 
  kx <- matrix(NA,n,1)
  ky <- matrix(NA,n,1)
  kstd <- matrix(NA,n,1)
  for (i in 1:n)
  {
    kx[i] = estimates[[i]]$estimate[1];
    ky[i] = estimates[[i]]$estimate[2];
    kstd[i] = sqrt(norm(estimates[[i]]$estimateCov[1:2,1:2]))
  }
  kf_dat<-data.frame(X=kx,Y=ky,Std=kstd)
  return(kf_dat)
}