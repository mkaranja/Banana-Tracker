
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

user_details <- tabPanel("User Details", value = "controlform_userdetails",
                     
                    fluidRow(
                      
                      column(8, br(), br(),
                             panel_div(class_type = "default",
                                 content = tags$div(
                                   uiOutput("userdetails_Output")
                                 )
                                       
                             )
                      )
                    )
)
