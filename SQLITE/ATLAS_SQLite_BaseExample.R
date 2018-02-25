# Example script for querying ATLAS SQLITE files

# Custom functions require package RSQLite !!!
source('ATLAS_SQLite_GetLoc.R')
source('ATLAS_SQLite_GetTag.R')
source('ATLAS_SQLite_GetTagTime.R')

##########################################################READ ENTIRE DATABASE################################################
# This code takes a while to execute as it has to read the entire database
# Select database file
sqlfile <- choose.files(default = "", caption = "Select SQLite Database File", multi = FALSE, filters = c(".sqlite", "*.*"))
# Read entire database
SQLDATA <- ATLAS_SQLite_GetLoc(sqlfile)


##########################################################READ SPECIFIC DATA################################################
# If you know which tags your looking for it is faster to read just the data for the tags your interested in or
# just the time interval for the tag your interested in
TagList <- unique(SQLDATA$TAG) #Create list of tags (this is a workaround for me to create a list of tags for this example)

# Go through a list of tags
for(tag_number in 1:length(TagList))
{
  # Read data only for a specific tag
  TAGDATA <- ATLAS_SQLite_GetTag(sqlfile,TagList[tag_number])
  
  # This part covers time conversion
  start_time <- min(TAGDATA$TIME) # Unix time in ms since 1970-01-01 (default ATLAS time format; time zone in ATLAS is UTC)
  start_time_seconds <- start_time/1000 # Prior to conversion the time has to be converted to seconds
  start_time_date <- as.POSIXct(start_time_seconds, tz="UTC", origin="1970-01-01") # Conversion to Date_Time format
  
  # Working in code it's never favorouble to use Date-Time formats which is why ATLAS provides time in absolute ms. 
  # instead of converting ATLAS time to match your metadata its recommended to convert your metadata from 
  # Date-Time format to ATLAS time format.
  
  # Conversion to ATLAS Time format
  YOURTIME <- as.character.Date(start_time_date) # Lets assume the start_time was the time you wanted to convert
  ATLASTIME<-as.numeric(as.POSIXct(YOURTIME, "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000
  #NOTE: the exact conversion parameters depend on the format you provide 
  
  # Here I am generating an enpoint in time for the final query example
  end_time <- max(TAGDATA$TIME) # Unix time in ms since 1970-01-01
  end_time_seconds <- end_time/1000 # Prior to conversion the time has to be converted to seconds
  end_time_date <- as.POSIXct(end_time_seconds, tz="UTC", origin="1970-01-01") # Conversion to Date-Time format
  
  # Read data for specific tag and time interval (from start_time to end_time)
  TIMEDATA = ATLAS_SQLite_GetTagTime(sqlfile,TagList[tag_number],start_time,end_time)
}