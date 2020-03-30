
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_Terminator_Modal <-  
  div(
              div(id = "project_specific_admin_Terminator_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_Terminator_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_Terminator_AddNewTerminator", "Terminator", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_Terminator_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Terminator_AddNew", "Add New Terminator", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_Terminator_UpdateTerminator", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_Terminator_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Terminator_Update", "Update the Terminator", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Terminator_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Terminator_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Terminator_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Terminator_ControlForm", "ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))

