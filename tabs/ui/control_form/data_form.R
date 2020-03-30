


panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


dataform <- tabPanel("Data Form", value = "controlform_dataform",
                     br(), br(), br(),
                     tags$head(tags$style(HTML('.modal-lg {width: 80%;}'))),
                      fluidRow(
                        
                        column(12, offset = 1,br(), br(), br(),br(), br(),
                               
                               column(width = 3, align = "center",
                                      actionBttn("culture_initiation_module", label = "Culture Initiation Module", size = "md", style = "jelly", color = "primary", block=T)
                               ),
                               column(1),
                               column(width = 3, align = "center",
                                      actionBttn("cell_suspension_culture_module", label = "Cell Suspension Culture Module", size = "md", style = "jelly", color = "primary", block=T)
                               )
                        )
                )
)
