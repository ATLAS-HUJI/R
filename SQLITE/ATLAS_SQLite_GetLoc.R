ATLAS_SQLite_GetLoc <- function(dbn) 
{
  library("RSQLite")
  # connect to the sqlite file
  con = dbConnect(RSQLite::SQLite(),dbn)
  query = paste('select * from LOCALIZATIONS', sep = "")
  p1 = dbGetQuery(con,query)
  dbDisconnect(con)
  return(p1)
}