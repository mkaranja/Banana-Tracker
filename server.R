#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://github.com/yanirmor/shiny-user-management/blob/master/R/sign_in_server.R

library(shiny)

shinyServer(function(input, output, session) {

    # shinyURL.server()
    
    loggedIn <- reactiveVal(value = FALSE)
    user <- reactiveValues(name = NULL)
    
    # User Panel
    
    source("R/user_panel_server.R", local = TRUE)
    
    
    # Main App UI
    
    output$main_app_Output <- renderUI({
         username <- input$username
         
         if(isAdmin(username)){
             app1
       }else {
             app2
          }
    })
    
    
    ## 1. -- HOME  TAB----------------------------------------------------------------------------------------------
    source("tabs/server/home.R", local = TRUE)
    
    
    ## 2. MFC, SPC and CSC Control TAB------------------------------------------------------------------------------
    
    # Data Form
    
    source("tabs/server/control_form/data_form/culture_initiation_module.R", local = TRUE)
    source("tabs/server/control_form/data_form/cell_suspension_culture_module.R", local = TRUE)
    
    # Administration
    
    source("tabs/server/control_form/administration/identity_type.R", local = TRUE)
    source("tabs/server/control_form/administration/cultivar.R", local = TRUE)
    source("tabs/server/control_form/administration/cultured_by.R", local = TRUE)
    source("tabs/server/control_form/administration/additives.R", local = TRUE)
    source("tabs/server/control_form/administration/media.R", local = TRUE)
    source("tabs/server/control_form/administration/permit_type.R", local = TRUE)
    source("tabs/server/control_form/administration/source.R", local = TRUE)
    
    # Reports
    
    source("tabs/server/control_form/reports.R", local = TRUE)
    
    # User Details
    
    source("tabs/server/control_form/user_details.R", local = TRUE)
    
    ## 3 . PROJECT SELECTION TAB---------------------------------------------------------------------------------------
    
    source("tabs/server/project_specific.R", local = TRUE)
    
    ## 4. - ADMINISTRATION TAB ----------------------------------------------------------------------------------------
    
    source("tabs/server/administration.R", local = TRUE)
    
})
