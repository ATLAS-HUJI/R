# Paige Saunders Kalman filter for data with known covariances
# For use/implementation see PSKF_Example.R
# Based on Paige CC, Saunders MA. Least squares estimation of discrete linear dynamic systems using orthogonal transformations. SIAM Journal on Numerical Analysis. 1977 Apr;14(2):180-93.
# Original code by Sivan Toledo 2017
# Converted to R by Ingo Schiffner 2017
source('InvChol.R')
PSKF <- function (S, S_cov, O, observations)
{
  SDIM = nrow(S)          #state dimensions
  ODIM = nrow(O)          #observed dimensions
  L = InvChol(S_cov)      #doesnt do anything here
  K = -t(L) %*% S         #cross product
  n = length(observations)
  
  #estimate list array
  tmp <- matrix(NA,1,1)
  estimates = rep(list(list(Riipo=tmp,Rii=tmp,Rhs=tmp)),n)
  r = 1;
  c = 1;
  Bimo = matrix(0,SDIM,SDIM)
  bimo = matrix(0,SDIM,1)
  
  for (i in 1:n)
  {
    observation_cov = observations[[i]]$cov
    if (is.na(observation_cov[1,1])==F)
    {
      Ltilde = InvChol(observation_cov)
      C = t(Ltilde) %*% O
      r = r + ODIM
      
      # recursive/sparse formulation
      if (i < n)
      {
        B1 = cbind(Bimo, matrix(0,SDIM,SDIM), bimo)
        B2 = cbind(C, matrix(0,ODIM,SDIM), t(Ltilde) %*% t(observations[[i]]$y))
        B3 = cbind(K, t(L), matrix(0,SDIM,1))
        B = rbind(B1,B2,B3)
        rhs_col = SDIM+SDIM+1
      }
      else
      {
        B1 = cbind(Bimo,bimo)
        B2 = cbind(C, t(Ltilde) %*% t(observations[[i]]$y))
        B = rbind(B1,B2)
        rhs_col = SDIM+1
      }
    }
    else
    {
      if (i<n)
      {
        B1 = cbind(Bimo, matrix(0,SDIM,SDIM), bimo)
        B2 = cbind(K, t(L), matrix(0,SDIM,1))
        B = rbind(B1,B2)
        rhs_col = SDIM + SDIM+1
      }
      else
      {
        B = cbind(Bimo,bimo)
        rhs_col = SDIM+1
      }
    }
    
    c = c + SDIM
    r = r + SDIM
    
    # recursive/sparse formulation
    R = qr.R(qr(B), complete = T)
    estimates[[i]]$Rhs = R[1:SDIM, rhs_col]
    estimates[[i]]$Rii = R[1:SDIM, 1:SDIM]
    if (i<n)
    {
      #RESET after certain time?
      bimo = R[(1+SDIM):(SDIM+SDIM), rhs_col]
      Bimo = R[(1+SDIM):(SDIM+SDIM), (1+SDIM):(SDIM+SDIM)]
      estimates[[i]]$Riipo = R[1:SDIM, (1+SDIM):(SDIM+SDIM)]
    }
  }
  
  n = i
  Rtildeii = estimates[[n]]$Rii
  for (i in n:1)
  {
    # compute the smoothed estimate
    rhs = estimates[[i]]$Rhs;
    if (i<n)
    {
      rhs = rhs - estimates[[i]]$Riipo %*% estimates[[i+1]]$estimate;
    }
    
    #works perfectly
    s = solve(estimates[[i]]$Rii,rhs)
    estimates[[i]]$estimate = s
    
    #Compute the covariance of the smoothed estimate
    estimates[[i]]$estimateCov = solve(t(Rtildeii) %*% Rtildeii);
    if (i>1)
    {
      RBCS1 = cbind(estimates[[i-1]]$Riipo,estimates[[i-1]]$Rii)
      RBCS2 = cbind(Rtildeii, matrix(0,SDIM,SDIM))
      RBlockColSwap = rbind(RBCS1, RBCS2)
      # now what?
      R = qr.R(qr(RBlockColSwap), complete = T)
      # for next iteration
      Rtildeii = R[(SDIM+1):(SDIM+SDIM), (SDIM+1):(SDIM+SDIM)];
    }
  }
  return(estimates)
}