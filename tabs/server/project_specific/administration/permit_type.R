
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

project_specific_admin_PermitType_Modal <-  function(text){
  modalDialog(size = "l",
              title = tags$h2(style="color:#800000;","Permit Type"),
              useShinyalert(), 
              shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
              div(id = "project_specific_admin_PermitType_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_PermitType_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_PermitType_AddNewPermitType", "Permit Type", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_PermitType_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_PermitType_AddNewIdentityType", "Add New Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_PermitType_UpdatePermitType", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_PermitType_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_PermitType_UpdateIdentityType", "Update the Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_PermitType_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_PermitType_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_PermitType_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_PermitType_MFC_SCP_CSC_ControlForm", "MFC SCP CSC ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))
}
observeEvent(input$project_specific_admin_PermitType, {
  showModal(
    project_specific_admin_PermitType_Modal("PermitType")
  )
})


# reactive values

loadPermitType <- reactive({
  pool %>% tbl("tblPermitType") %>% collect()
})

CV <- reactiveValues()

output$project_specific_admin_PermitType_Table <- renderRHandsontable({
  dt <- CV$Data <- loadPermitType()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$project_specific_admin_PermitType_Table_select$select$r,{
  
  temp <- isolate(input$project_specific_admin_PermitType_Table)
  df <- hot_to_r(temp)
  
  r <- input$project_specific_admin_PermitType_Table_select$select$r
  updateTextInput(session, "project_specific_admin_PermitType_UpdatePermitType", label = "", value = df[r, 1])
  updateTextInput(session, "project_specific_admin_PermitType_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$project_specific_admin_PermitType_UpdateIdentityType,{
  temp <- isolate(input$project_specific_admin_PermitType_Table)
  df <- hot_to_r(temp)
  
  id=input$project_specific_admin_PermitType_UpdatePermitType
  col="Description"
  value=input$project_specific_admin_PermitType_UpdateDescription
  
  sql <- "UPDATE tblPermitType SET Description = ?value, PermitType = ?id2 WHERE PermitType = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadPermitType()
  CV$Data
  shinyjs::js$refresh()
})


## add new PermitType

# define fields to add

observeEvent(input$project_specific_admin_PermitType_AddNewIdentityType,{
  req(input$project_specific_admin_PermitType_AddNewPermitType)
  dt <- CV$Data <- loadPermitType()
  df <- data.frame(PermitType = input$project_specific_admin_PermitType_AddNewPermitType, Description = input$project_specific_admin_PermitType_AddNewDescription)
  if((df$PermitType %in% dt$PermitType)==TRUE){
    shinyalert("Oops!", "PermitType Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblPermitType', df, append = T)
    shinyalert("Success!", "PermitType Added", type = "success")
  }
})


observeEvent(input$project_specific_admin_PermitType_Refresh,{
  CV$Data <- loadPermitType()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$project_specific_admin_PermitType_Clear, {
  reset(input$project_specific_admin_PermitType_Form)
  updateTextInput(session,  inputId = "project_specific_admin_PermitType_AddNewPermitType", "Permit Type", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_PermitType_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "project_specific_admin_PermitType_UpdatePermitType", "", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_PermitType_UpdateDescription", "", value = "")
})

observeEvent(input$project_specific_admin_PermitType_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$project_specific_admin_PermitType_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

