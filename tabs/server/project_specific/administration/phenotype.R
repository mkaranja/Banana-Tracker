
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_Phenotype_Modal <-  
  div(
              div(id = "project_specific_admin_Phenotype_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_Phenotype_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_Phenotype_AddNewPhenotype", "Phenotype", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_Phenotype_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Phenotype_AddNew", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_Phenotype_UpdatePhenotype", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_Phenotype_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Phenotype_Update", "Update", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Phenotype_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Phenotype_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
                       column(2, actionBttn("project_specific_admin_Phenotype_Refresh", "Refresh", style = "jelly", size = "xs", color = "success", block=T)),
                       column(2, actionBttn("project_specific_admin_Phenotype_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))

