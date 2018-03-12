ATLAS_SQLite_GetTagTime <- function(dbn,seltag,st,et) 
{
  library("RSQLite")
  # connect to the sqlite file
  con = dbConnect(RSQLite::SQLite(),dbn)
  query = paste('select * from LOCALIZATIONS WHERE TAG=', seltag , 'AND TIME >=', st, 'AND TIME <=', et)
  tag_data = dbGetQuery(con,query)
  dbDisconnect(con)
  return(tag_data)
}