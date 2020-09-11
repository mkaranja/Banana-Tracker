
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(rhandsontable)
library(dplyr)
library(dbplyr)
library(magrittr)
library(RODBC)
library(pool)
library(V8)
library(odbc)
library(scrypt) # https://cran.r-project.org/web/packages/argon2/README.html
library(qrencoder)
library(waiter) # notifications
library(lubridate)
library(janitor)
source("functions.R")
source("dbQueries.R")

pool <- dbConnect(odbc(),
                  Driver = "ODBC Driver 17 for SQL Server",
                  Server = "***,***",
                  Database = "****",
                  UID = "****",
                  PWD = "*****"
)
# Tabs
tab_files <- list.files(path = "tabs/ui", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

# Admin
isAdmin <- tbl(pool, "tblUserInformation") %>%
  dplyr::filter(trimws(PrivilegeLevel) == "Admin") %>%
  collect()
  
# Helpers
#source("helpers.R")
cores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")
jsrefresh <- "shinyjs.refresh = function() { history.go(0); }" # refresh app
jsprint <- 'shinyjs.winprint = function() { window.capture(); }' # print page
jsclose <- "shinyjs.closewindow = function() { window.close(); }"
jsResetCode <- "shinyjs.reset2 = function() {history.go(0)}" # Define the js method that resets the page
enableBookmarking(store = "url")


