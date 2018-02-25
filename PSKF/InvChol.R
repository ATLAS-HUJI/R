# Inverse Cholesky decomposition
# Original code by Sivan Toledo 2017
# Converted to R by Ingo Schiffner 2017
InvChol <- function (C)
{
  IC<-tryCatch({
  invL = chol(C)
  IC = solve(invL)
  return(IC)
  },error = function(err) {
  print('indefinite covariance matrix? using only diagonal')
  tmp=diag(C)^-0.5
  IC = diag(tmp)
  return(IC)
  })
}