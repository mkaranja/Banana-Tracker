# pool <- dbConnect(odbc::odbc(),
#                   Driver = "SQL Server",
#                   Server = "localhost\\SQLEXPRESS",
#                   Database = "***",
#                   Trusted_Connection = "True"
# )
pool <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "****\\SQLEXPRESS,41433",
                 Database = "***",
                 UID = "***",
                 PWD = "***"
                 )
# Load Data
loadData <- function(table) {
  db <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "****\\SQLEXPRESS,41433",
                 Database = "***",
                 UID = "***",
                 PWD = "***"
                 )
  
  query <- sprintf("SELECT * FROM %s", table)
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

# Save New Record
saveData <- function(data, table) {
  db <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "****\\SQLEXPRESS,41433",
                 Database = "***",
                 UID = "***",
                 PWD = "***"
                 )
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  dbGetQuery(db, query)
  dbDisconnect(db)
}


# Update Record
updateData <- function(data, table) {
  db <- dbConnect(odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "****\\SQLEXPRESS,41433",
                 Database = "***",
                 UID = "***",
                 PWD = "***"
                 )
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  dbGetQuery(db, query)
  dbDisconnect(db)
}
