
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

project_specific_admin_BacterialSelection_Modal <-  
  div(
              div(id = "project_specific_admin_BacterialSelection_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_BacterialSelection_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_BacterialSelection_AddNewBacterialSelection", "BacterialSelection", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_BacterialSelection_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_BacterialSelection_AddNew", "Add New Bacterial Selection", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_BacterialSelection_UpdateBacterialSelection", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_BacterialSelection_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_BacterialSelection_Update", "Update the Bacterial Selection", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_BacterialSelection_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_BacterialSelection_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_BacterialSelection_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_BacterialSelection_ControlForm", "ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              )
          )


