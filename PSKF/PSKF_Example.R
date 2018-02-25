# Simple implementation of the Paige Saunders Kalman filter for ATLAS data
# Original code by Sivan Toledo 2017
# Converted to R by Ingo Schiffner 2017
source('PSKF.R')

dat=read.csv('example_locs.txt',header=F,sep=" ") 

#column nbmrs in ascii file
TIME = 2
X = 7
Y = 8
VARX = 15
VARY = 19
COVXY = 16

stdThreshold = 16 #threshold for quality estimation (m)

#sort data
dat<- dat[order(dat[,TIME]),]
t0 = dat[1,TIME];
dt = round(min(diff(dat[,TIME]))) # determine sample rate

#Kalman filter settings
S = matrix(c( 1,0,1,0,0,1,0,1,0,0,1,0,0,0,0,1),nrow=4,ncol=4,byrow=T) #state transition matrix
O = matrix(c( 1,0,0,0,0,1,0,0),nrow=2,ncol=4,byrow=T) # observation matrix
SDIM = 4 # state dimensions (x,y,.x,and.y)
ODIM = 2 # observation dimensions (x and y)

#build dataset
n = ceiling((dat[nrow(dat),TIME]-t0)/dt)+1
type <- numeric(n)
j = 1
i = 1
t = t0 - dt
tmp_cov <- matrix(NA,2,2)
tmp_y <- matrix(NA,2,1)
tmp_t <- matrix(NA,1,1)

observations = rep(list(list(TIME=tmp_t,y=tmp_y,cov=tmp_cov)),n)
while (j<=n)
{
  t = t + dt
  observations[[j]]$TIME = t
  if (abs(t-dat[i,TIME])< 0.5)
  {
    t = dat[i,TIME]
    type[j] = 1; 
    observations[[j]]$y   = matrix(c(dat[i,X],dat[i,Y]),nrow=1,ncol=2,byrow=T)
    observations[[j]]$cov = matrix(c(dat[i,VARX],dat[i,COVXY],dat[i,COVXY],dat[i,VARY]),nrow=2,ncol=2,byrow=T)
    i=i+1
  }
  j=j+1
}

#Apply Kalman Filter
S_cov = diag(SDIM)
estimates = PSKF(S, S_cov, O, observations);
kx <- matrix(NA,n,1)
ky <- matrix(NA,n,1)
kstd <- matrix(NA,n,1)
for (i in 1:n)
{
   kx[i] = estimates[[i]]$estimate[1];
   ky[i] = estimates[[i]]$estimate[2];
   kstd[i] = sqrt(norm(estimates[[i]]$estimateCov[1:2,1:2]))
}
 
presentGood = intersect(which(type==1), which(kstd<=stdThreshold))
missingGood = intersect(which(type==0), which(kstd<=stdThreshold))
good = which(kstd<=10)

plot(dat[,X],dat[,Y],type="p", pch=4)
lines(kx[good],ky[good],col="blue")
points(kx[presentGood],ky[presentGood],col="green", pch=16, cex=0.5)
points(kx[missingGood],ky[missingGood],col="red", pch=16, cex=0.5)
