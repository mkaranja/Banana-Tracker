
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_Promoter_Modal <-  
  div(
              div(id = "project_specific_admin_Promoter_Form",
                  fluidRow(
                    column(12,
                           rHandsontableOutput("project_specific_admin_Promoter_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(1, textInput("project_specific_admin_Promoter_AddNewPromoter", "Promoter", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Promoter_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Promoter_GenBankAccessionNumber", "GenBank Accession", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Promoter_Origin", "Origin",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Promoter_NBAPermitNumber", "NBA Permit", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Promoter_OGTRPermitNumber", "OGTR Permit",value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Promoter_MTANumber", "MTA Number",value = "", width = "100%")),
                       
                           column(1, br(), actionBttn("project_specific_admin_Promoter_AddNew", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(12,
                           column(1, disabled(textInput("project_specific_admin_Promoter_UpdatePromoter", "", value = "", width = "100%"))),
                           column(2, textInput("project_specific_admin_Promoter_UpdateDescription", "", value="", width = "100%")),
                           column(2, textInput("project_specific_admin_Promoter_UpdateGenBankAccessionNumber", "", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Promoter_UpdateOrigin", "",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Promoter_UpdateNBAPermitNumber", "", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Promoter_UpdateOGTRPermitNumber", "",value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Promoter_UpdateMTANumber", "",value = "", width = "100%")),
                           
                           column(1, br(), actionBttn("project_specific_admin_Promoter_Update", "Update", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Promoter_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Promoter_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
                       column(2, actionBttn("project_specific_admin_Promoter_Refresh", "Refresh", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Promoter_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))

