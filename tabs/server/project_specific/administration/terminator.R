
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_Terminator_Modal <-  
  div(
              div(id = "project_specific_admin_Terminator_Form",
                  fluidRow(
                    column(12,
                           rHandsontableOutput("project_specific_admin_Terminator_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(1, textInput("project_specific_admin_Terminator_AddNewTerminator", "Terminator", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Terminator_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Terminator_GenBankAccessionNumber", "Gen Bank Accession", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Terminator_Origin", "Origin",value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Terminator_NBAPermitNumber", "NBA Permit", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Terminator_OGTRPermitNumber", "OGTR Permit",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Terminator_MTANumber", "MTA", value = "", width = "100%")),
                           
                           column(1, br(), actionBttn("project_specific_admin_Terminator_AddNew", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(12, 
                           column(1, disabled(textInput("project_specific_admin_Terminator_UpdateTerminator", "", value = "", width = "100%"))),
                           column(2, textInput("project_specific_admin_Terminator_UpdateDescription", "", value="", width = "100%")),
                           column(2, textInput("project_specific_admin_Terminator_UpdateGenBankAccessionNumber", "", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Terminator_UpdateOrigin", "",value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Terminator_UpdateNBAPermitNumber", "", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Terminator_UpdateOGTRPermitNumber", "",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Terminator_UpdateMTANumber", "", value = "", width = "100%")),
                           
                           column(2, br(), actionBttn("project_specific_admin_Terminator_Update", "Update", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Terminator_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Terminator_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
                       column(2, actionBttn("project_specific_admin_Terminator_Refresh", "Refresh", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Terminator_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))

