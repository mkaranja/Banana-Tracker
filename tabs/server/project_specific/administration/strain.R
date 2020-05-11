
project_specific_admin_Strain_Modal <-  
  div(
      div(id = "project_specific_admin_Strain_Form",
          fluidRow(
            column(10, offset = 1,
                   rHandsontableOutput("project_specific_admin_Strain_Table", height = "200px"), 
                   tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                   column(3, textInput("project_specific_admin_Strain_AddNewStrain", "Strain", value = "", width = "100%")),
                   column(6, textInput("project_specific_admin_Strain_AddNewDescription", "Description",value = "", width = "100%")),
                   column(2, br(), actionBttn("project_specific_admin_Strain_AddNew", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
            ),
            column(10, offset = 1, 
                   column(3, disabled(textInput("project_specific_admin_Strain_UpdateStrain", "", value = "", width = "100%"))),
                   column(6, textInput("project_specific_admin_Strain_UpdateDescription", "", value="", width = "100%")),
                   column(2, br(), actionBttn("project_specific_admin_Strain_Update", "Update", style = "jelly", size = "xs", color = "primary", block=T))
            )
          )
      ), br(), br(),
      fluidRow(
        column(10, offset = 1,
               column(2, actionBttn("project_specific_admin_Strain_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
               column(2, actionBttn("project_specific_admin_Strain_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
               column(2, actionBttn("project_specific_admin_Strain_Refresh", "Refresh", style = "jelly", size = "xs", color = "success", block=T)),
               column(2, actionBttn("project_specific_admin_Strain_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T))
        )
      )
)

