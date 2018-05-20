#AdpFixedPoint is a function to perform track segmentation utilizing a 
#first-passage algorithm to determine fixed points where the agent/animal 
#has spent a minimum number of observations (obs_min) within a a certain 
#range (adp_rng) of a fixed point that is continuously reavaluated.
#
#Input:
#time: absolute time e.g. in ms (ATLAS timestamp) 
#x,y: projected latitude and longitude in meters
#adp_rng: adaptive range defining fixed points
#smp_rte: sampling rate e.g. in ms (ATLAS)
#obs_min: minimum nuber of observations defining a fixed position
#p_lim: point limit for leaving current fixed point
#
#Output:
#The function returns an array containing information about each fixed 
#point including (in order) start time, end time, duration, number of locations, 
#position quality, median x, median-y, lower x quantile, upper x quantile, lower y quantile, upper y quantile
#
#Original Code by Ingo Schiffner 2017 (Matlab version)
#Converted to R by Emanuel Lourie 2018
AdpFixedPoint <- function(time,x,y,adp_rng,smp_rte,obs_min,p_lim)
{
  time <- as.double(time) # fix potential problems with integer overlow
  n_x <- which(!is.na(x))
  max_pos <- trunc(length(n_x)/(obs_min+p_lim))
  AFPList<-data.frame(start=rep(NA,max_pos),end=rep(NA,max_pos),duration=rep(NA,max_pos), num_loc=rep(NA,max_pos), position_qlt=rep(NA,max_pos),medX=rep(NA,max_pos), medY=rep(NA,max_pos))
  
  si <- n_x[1]                    # start index
  ei <- n_x[length(n_x)]          # end index
  cfp <- si                       # set initial fixed point
  
  cfp_i <- 1                      # counter to check if we have a valid fixed point (i.e. exceeds obs_min)
  l_cnt <- 0                      # counter to check if agent is leaving current fixed point (i.e. exceeds p_lim)
  fp_cnt <- 1                      # fixed point counter
  
  #set startpoint as first point (we do this for those cases were the animal is imidiatley moving)
  AFPList[fp_cnt,1] <- time[si]  # start time 
  AFPList[fp_cnt,2] <- time[si+1]# end time 
  AFPList[fp_cnt,3] <- 1         # duration (ms) 
  AFPList[fp_cnt,4] <- 2         # number of localizations defining fixed point
  AFPList[fp_cnt,5] <- 0         # estimate of position quality
  AFPList[fp_cnt,6] <- x[si]     # median x position
  AFPList[fp_cnt,7] <- y[si]     # median-y position
  
  cfp_x <- NULL # current fixed point x 
  cfp_y <- NULL # current fixed point y
  cfp_t <- NULL # current fixed point time
  
  #move through the data
  for (i in (si+1):ei)
  {  
    #make sure its a valid position
    if (!is.na(x[i]) & !is.na(y[i]))
    {
      #get distance from current fixed point
      dx <- x[i]-x[cfp]
      dy <- y[i]-y[cfp]
      e_dst <- (dx^2 + dy^2)^0.5

      if (e_dst > adp_rng) # check if agent is out of range
      {
        # increase leaving counter 
        l_cnt <- l_cnt + 1
        if (l_cnt >= p_lim) # check if agent has left current fixed point
        {
          if (cfp_i >= obs_min) # check if fixed point has sufficient observations
          {
            #evaluate fixed point
            fp_cnt <- fp_cnt + 1
            AFPList[fp_cnt,1] <- cfp_t[1]                                      # start time
            AFPList[fp_cnt,2] <- cfp_t[cfp_i-1]                                # end time
            AFPList[fp_cnt,3] <- (cfp_t[cfp_i-1]-cfp_t[1])                     # duration
            AFPList[fp_cnt,4] <- cfp_i-1;                                      # number of localizations
            AFPList[fp_cnt,5] <- AFPList[fp_cnt,4]/(AFPList[fp_cnt,3]/smp_rte) # position quality
            AFPList[fp_cnt,6] <- median(cfp_x)                                 # median x position
            AFPList[fp_cnt,7] <- median(cfp_y)                                 # median y position
          }
          
          # set new fixed point
          cfp <- i
          cfp_i <- 1
          
          # reset temp data
          cfp_x <- NULL
          cfp_y <- NULL
          cfp_t <- NULL
        }
      }
      else
      {
        
        #add data to tmp fixed point list
        cfp_x[cfp_i] <- x[i]
        cfp_y[cfp_i] <- y[i]
        cfp_t[cfp_i] <- time[i]
        cfp_i <- cfp_i +1
        
        #reset leaving counter
        l_cnt <- 0
      }
    }
  }
  #set end point to last position (in case track stops mid flight)
  fp_cnt <- fp_cnt + 1
  AFPList[fp_cnt,1] <- time[ei-1]  # start time
  AFPList[fp_cnt,2] <- time[ei]    # end time
  AFPList[fp_cnt,3] <- 1           # duration
  AFPList[fp_cnt,4] <- 2           # number of localization
  AFPList[fp_cnt,5] <- 0           # position quality
  AFPList[fp_cnt,6] <- x[ei]       # median x position
  AFPList[fp_cnt,7] <- y[ei]       # median y position
  
  #trunctate output matrix
  AFPList<-AFPList[1:fp_cnt,]
  return(AFPList)
}
