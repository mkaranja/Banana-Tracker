is_local = Sys.getenv('SHINY_PORT') == ""

server = "SQL Server"
database = "IITABANANA"
uid = "example"
pwd = "HBBFSE"

dbConnector <- function(server, database, uid, pwd, 
                        local=TRUE, port=1433, tds_version=7.4){
  if(local){
    DBI::dbConnect(odbc::odbc(), 
                   driver = "ODBC Driver 13 for SQL Server",
                   server = server, 
                   database = database, 
                   Trusted_Connection = "True"
    )
    
  }else{
    DBI::dbConnect(odbc::odbc(),
                   Driver   = "FreeTDS",
                   Database = database,
                   Trusted_Connection = "True",
                   Server   = server,
                   Port     = port,
                   TDS_Version=tds_version
    )
  }
}

dbConn <- dbConnector(server, database, uid, pwd, is_local)

pool <- dbConnect(odbc::odbc(), 
                  Driver = "SQL Server", 
                  Server = "localhost\\SQLEXPRESS", 
                  Database = "IITABANANA", 
                  Trusted_Connection = "True"
)