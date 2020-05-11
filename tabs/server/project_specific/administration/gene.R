
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_Gene_Modal <-  
  div(
              div(id = "project_specific_admin_Gene_Form",
                  fluidRow(
                    column(12,
                           rHandsontableOutput("project_specific_admin_Gene_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(1, textInput("project_specific_admin_Gene_AddNewGene", "Gene", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Gene_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Gene_GenBankAccessionNumber", "Gen Bank Accession", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_Origin", "Origin",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_NBAPermitNumber", "NBA Permit", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Gene_OGTRPermitNumber", "OGTR Permit",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_MTANumber", "MTA", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_NLRDNumber", "NLRD",value = "", width = "100%")),
                          
                           column(1, br(), actionBttn("project_specific_admin_Gene_AddNew", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(12, 
                           column(1, disabled(textInput("project_specific_admin_Gene_UpdateGene", "", value = "", width = "100%"))),
                           column(2, textInput("project_specific_admin_Gene_UpdateDescription", "", value="", width = "100%")),
                           column(2, textInput("project_specific_admin_Gene_UpdateGenBankAccessionNumber", "", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_UpdateOrigin", "",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_UpdateNBAPermitNumber", "", value = "", width = "100%")),
                           column(2, textInput("project_specific_admin_Gene_UpdateOGTRPermitNumber", "",value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_UpdateMTANumber", "", value = "", width = "100%")),
                           column(1, textInput("project_specific_admin_Gene_UpdateNLRDNumber", "",value = "", width = "100%")),
                           
                           column(1, br(), actionBttn("project_specific_admin_Gene_Update", "Update", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Gene_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Gene_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
                       column(2, actionBttn("project_specific_admin_Gene_Refresh", "Refresh", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Gene_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))

