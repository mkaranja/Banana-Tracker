
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_Gene_Modal <-  
  div(
              div(id = "project_specific_admin_Gene_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_Gene_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_Gene_AddNewGene", "Gene", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_Gene_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Gene_AddNew", "Add New Gene", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_Gene_UpdateGene", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_Gene_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Gene_Update", "Update the Gene", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Gene_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Gene_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Gene_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Gene_ControlForm", "ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))

