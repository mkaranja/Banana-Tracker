

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

home <- tabPanel(title = "HOME", 
                 value = "home",
                 hr(),
                 br(), br(),br(),br(),
                 tags$p(style = "color: black; font-size: 72px; text-align: center;font-family: monospace;","BANANA TRACKER"),
                 HTML("<h3><center>Tracking the Banana Transgenics</center></h3>"),
                 br(), br(), br(), br(),
                 fluidRow(
                 column(2),
                 column(8,br(), br(), br(),br(), br(),
                        
                         column(width = 5, align = "center",
                                actionBttn("control_form", label = "MFC, SPC and CSC Control Form", size = "lg", style = "jelly", color = "primary")
                         ),
                         column(2),
                         column(width = 5, align = "center",
                                actionBttn("project_form", label = "To Banana Project Selection", size = "lg", style = "jelly", color = "warning")
                         )
                      )
                 )
)
