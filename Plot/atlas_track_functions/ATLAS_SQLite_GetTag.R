ATLAS_SQLite_GetTag<- function(dbn,seltag) 
  {
  library("RSQLite")
  # connect to the sqlite file
  con = dbConnect(RSQLite::SQLite(),dbn)
  query = paste('select * from LOCALIZATIONS WHERE TAG=', seltag , sep = "")
  tag_data = dbGetQuery(con,query)
  dbDisconnect(con)
  return(tag_data)
}