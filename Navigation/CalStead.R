#function to determine steadiness over a set of x,y coordinates
#code by Ingo Schiffner
CalStead <- function (x,y)
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
  sum_cos = sum(ang_cos[!is.na(ang_cos)])
  sum_sin = sum(ang_sin[!is.na(ang_sin)])
  head = cart2pol(sum_sin, sum_cos,  degrees = T)
  Heading= head$theta
  if (Heading < 0)
  {
    Heading= Heading + 360
  }
  Steadiness = round(((sum_cos ^ 2) + (sum_sin ^ 2)) ^ 0.5 / sum(!is.na(x_dif)) * 100) / 100;
  list(Heading,Steadiness)
}