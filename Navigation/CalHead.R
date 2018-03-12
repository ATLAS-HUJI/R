#function to calculate heading over a set of x,y coordinates
#code by Ingo Schiffner
CalHead <- function (x,y)
{
  #remove na values
  x_n = x[!is.na(x)]
  y_n = y[!is.na(y)]
  
  #get euclidean distance
  x_dif = diff(x_n)
  y_dif = diff(y_n)
  e_dif = (x_dif^2 + y_dif^2)^0.5
  ang_cos = x_dif / e_dif
  ang_sin = y_dif / e_dif
  head = cart2pol(ang_sin , ang_cos, degrees = T)
  return(head$theta)
}