# ipak <- function(pkg){
#  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#  if (length(new.pkg)) 
#    install.packages(new.pkg, dependencies = TRUE)
#  sapply(pkg, require, character.only = TRUE)
# }
# 
# # usage
# packages <- c("devtools","shiny", "shinyBS", "shinyWidgets", "shinyjs", "shinyalert", "shinydashboard","shinycssloaders", 
#              "rhandsontable", "dplyr", "dbplyr", "magrittr", "odbc", "RODBC", "pool", "bcrypt", "lubridate","sodium")
# ipak(packages)

library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
#library(shinyURL)# devtools::install_github("aoles/shinyURL")
library(shinydashboard)
#library(shinycssloaders) # devtools::install_github('andrewsali/shinycssloaders')
library(rhandsontable)
#library(excelR)
library(dplyr)
library(dbplyr)
library(magrittr)
library(odbc)
library(RODBC)
library(pool)

# library(webshot)
library(bcrypt) # passwords
# library(keyring)
# library(digest)
library(sodium)
library(waiter) # notifications
source("functions.R")

 pool <- dbConnect(odbc::odbc(), 
               Driver = "SQL Server", 
               Server = "localhost\\SQLEXPRESS", 
               Database = "IITABANANA", 
               Trusted_Connection = "True"
               )
cores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")


# Helpers

source("helpers.R")

# TABS

source("R/user_panel_ui.R", local = TRUE) # User panel
source("R/main_app.R", local = TRUE) # Main App



jsrefresh <- "shinyjs.refresh = function() { history.go(0); }" # refresh app
jsprint <- 'shinyjs.winprint = function() { window.capture(); }' # print page
jsclose <- "shinyjs.closeWindow = function() { window.close(); }" # close app
jsResetCode <- "shinyjs.reset2 = function() {history.go(0)}" # Define the js method that resets the page

enableBookmarking(store = "url")


# user loggedin

usersinfo <- pool %>% tbl("tblUserInformation") %>% collect()
admin_users <- usersinfo %>%
  dplyr::filter(grepl("Admin", PrivilegeLevel)==T) %>%
  dplyr::select(UserName) %>%
  as.vector()

isAdmin <- function(x){
  x %in% admin_users$UserName
}


# Load tables
mfc <- tbl(pool,"tblMFC") %>% collect()
cultured_by <- tbl(pool,"tblCulturedBy") %>% collect()
cultivar <- tbl(pool,"tblCultivar") %>% collect()
source <- tbl(pool,"tblSource") %>% collect()
permit_type <- tbl(pool,"tblPermitType") %>% collect()
media <- tbl(pool,"tblMedia") %>% collect()
additives <- tbl(pool,"tblAdditives")  %>% collect()
cultures <- tbl(pool,"tblCultures") %>% collect()
deletedMFC <- tbl(pool, "tblDeletedMFC") %>% collect()

