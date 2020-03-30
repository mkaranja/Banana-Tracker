# Other Tabs
tab_files <- list.files(path = "tabs/ui", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))


app1 <- navbarPage(title = "",
                  
                  id = "navbar",
                  selected = "home",
                  theme = "style1.css", 
                  fluid = T,
                  position = "fixed-top",
                  
                  home,
                  control_form,
                  project_selection,
                  administration,
                  manual
)


app2 <- navbarPage(title = "",
                   
                   id = "navbar",
                   selected = "home",
                   theme = "style1.css", 
                   fluid = T,
                   position = "fixed-top",
                   
                   home,
                   control_form,
                   project_selection,
                   manual
)