
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}
tab_files <- list.files(path = "tabs/ui/control_form/reports", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

reports <- tabPanel("Reports", value = "controlform_reports",
                  
                  br(), br(),
                  fluidRow(
                    
                    column(8, 
                                       
                             div(style = "text-align: center;", 
                                 div(style = "text-align: center;", 
                                     column(4,
                                           actionBttn("culture_initiation_reports", label = "Culture Initiation Reports", size = "md", style = "unite", color = "primary", block=T)
                                           ),
                                     column(2),
                                     column(4,
                                           actionBttn("cell_suspension_culture_reports", label = "Cell Suspension Culture Reports", size = "md", style = "unite", color = "primary", block=T)
                                           )
                                     
                                 ) 
                                     
                           )
                    )
                  )
)
