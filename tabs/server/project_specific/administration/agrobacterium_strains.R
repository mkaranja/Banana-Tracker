
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_AgrobacteriumStrains_Modal <-  
  div(
  div(id = "project_specific_admin_AgrobacteriumStrains_Form",
        fluidRow(
          column(10, offset = 1,
                 rHandsontableOutput("project_specific_admin_AgrobacteriumStrains_Table", height = "200px"), 
                 tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                 column(3, textInput("project_specific_admin_AgrobacteriumStrains_AddNewAgrobacteriumStrains", "Agrobacterium Strains", value = "", width = "100%")),
                 column(6, textInput("project_specific_admin_AgrobacteriumStrains_AddNewDescription", "Description",value = "", width = "100%")),
                 column(2, br(), actionBttn("project_specific_admin_AgrobacteriumStrains_AddNew", "Add New Agrobacterium Strains", style = "jelly", size = "xs", color = "primary", block=T))
          ),
          column(10, offset = 1, 
                 column(3, disabled(textInput("project_specific_admin_AgrobacteriumStrains_UpdateAgrobacteriumStrains", "", value = "", width = "100%"))),
                 column(6, textInput("project_specific_admin_AgrobacteriumStrains_UpdateDescription", "", value="", width = "100%")),
                 column(2, br(), actionBttn("project_specific_admin_AgrobacteriumStrains_Update", "Update the Agrobacterium Strains", style = "jelly", size = "xs", color = "primary", block=T))
          )
        )
    ), br(), br(),
    fluidRow(
      column(10, offset = 1,
             column(2, actionBttn("project_specific_admin_AgrobacteriumStrains_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
             column(2, actionBttn("project_specific_admin_AgrobacteriumStrains_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
             column(2, actionBttn("project_specific_admin_AgrobacteriumStrains_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
             column(2, actionBttn("project_specific_admin_AgrobacteriumStrains_ControlForm", "ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
      )
    )
  )

