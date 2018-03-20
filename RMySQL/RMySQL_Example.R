#quick example script explaining basic DBI and RMySQL functionality
library(RMySQL)

#connect to database
con <- dbConnect(MySQL(),
                 user = 'YOURUSERNAME',      # username 
                 password = 'YOURPASSWORD',  # password
                 host = 'YOURIP',            # host ip address
                 dbname='YOUDBNAME')         # name of data base

TblNames <- dbListTables(con)                # returns the names of all tables in a database 
tdx <- 6                                     # selected table index
FldNames<- dbListFields(con, TblNames[tdx])  # returns name of all fields in a table 
fdx <- 1                                     # selected field index

# construct sQL statement
sql <- paste("SELECT * FROM ", TblNames[tdx], sep="") # in this example we query * (everything) in the selected table
#sql <- paste("SELECT ", FldNames[fdx] ," FROM ", TblNames[tdx], sep="") # in this example we query for a specific field in the selected table
rs <- dbSendQuery(con, sql) # send query to server
data = fetch(rs, n=-1)      # retrieve results from server (queries are executed on the server and fetch will return the result once the query has concluded)
dbDisconnect(con)           # disconnect from server
