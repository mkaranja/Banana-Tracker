
#jscode <- "shinyjs.refresh = function() { location.reload(); }"


#jscode <- "shinyjs.refresh = function() { location.reload(); }"


project_specific_admin_FieldTrialIdentity_Modal <-  
  div(
    div(id = "project_specific_admin_FieldTrialIdentity_Form",
        fluidRow(
          column(10, offset = 1,
                 rHandsontableOutput("project_specific_admin_FieldTrialIdentity_Table", height = "200px"), 
                 tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                 column(3, textInput("project_specific_admin_FieldTrialIdentity_AddNewFieldTrialIdentity", "Field Trial Identity", value = "", width = "100%")),
                 column(6, textInput("project_specific_admin_FieldTrialIdentity_AddNewDescription", "Description",value = "", width = "100%")),
                 column(2, br(), actionBttn("project_specific_admin_FieldTrialIdentity_AddNew", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
          ),
          column(10, offset = 1, 
                 column(3, disabled(textInput("project_specific_admin_FieldTrialIdentity_UpdateFieldTrialIdentity", "", value = "", width = "100%"))),
                 column(6, textInput("project_specific_admin_FieldTrialIdentity_UpdateDescription", "", value="", width = "100%")),
                 column(2, br(), actionBttn("project_specific_admin_FieldTrialIdentity_Update", "Update", style = "jelly", size = "xs", color = "primary", block=T))
          )
        )
    ), br(), br(),
    fluidRow(
      column(10, offset = 1,
             column(2, actionBttn("project_specific_admin_FieldTrialIdentity_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
             column(2, actionBttn("project_specific_admin_FieldTrialIdentity_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
             column(2, actionBttn("project_specific_admin_FieldTrialIdentity_Refresh", "Refresh", style = "jelly", size = "xs", color = "success", block=T)),
             column(2, actionBttn("project_specific_admin_FieldTrialIdentity_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T))
      )
    )
)

