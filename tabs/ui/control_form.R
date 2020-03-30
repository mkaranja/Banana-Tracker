
tab_files <- list.files(path = "tabs/ui/control_form", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))


control_form <- tabPanel("MFC, SPC & CSC CONTROL FORM", value = "controlform",
                         br(), br(),
                         tabsetPanel(id = "controlform_Tabs", type = "pills",
                           dataform,
                           admin,
                           reports,
                           user_details
                         )
                      )
