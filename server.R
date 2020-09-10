
shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp)
  

  # Main UI
  output$mainApp <- renderUI({
    
    user <- tbl(pool,"tblUserInformation") %>% 
      dplyr::filter(UserName == "mkaranja") %>%
      collect()
    
        navbarPage(title = "",
                   
                   id = "navbar",
                   selected = "home",
                   theme = "style1.css", 
                   fluid = T,
                   position = "fixed-top",
                   
                   home,
                   control_form,
                   project_selection,
                   if(grepl("Admin",user$PrivilegeLevel) == TRUE){
                    administration
                   },
                   manual
        )
    # } else {
    #     navbarPage(title = "",
    #                
    #                id = "navbar",
    #                selected = "home",
    #                theme = "style1.css", 
    #                fluid = T,
    #                position = "fixed-top",
    #                
    #                home,
    #                control_form,
    #                project_selection,
    #                manual
    #     )
    # }
  })
  
  
  source("tabs/server/login_panel.R", local = TRUE)
  source("tabs/server/home.R", local = TRUE)
  source("tabs/server/control_form.R", local = TRUE)
  source("tabs/server/project_specific.R", local = TRUE)
  
})
