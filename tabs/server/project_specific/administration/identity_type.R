
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

project_specific_admin_IdentityType_Modal <-  function(text){
  modalDialog(size = "l",
              title = tags$h2(style="color:#800000;","Identity Type"),
              useShinyalert(), 
              shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
              div(id = "project_specific_admin_IdentityType_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_IdentityType_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_IdentityType_AddNewIdentityType", "Identity Type", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_IdentityType_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_IdentityType_AddNewIdentityType", "Add New Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_IdentityType_UpdateIdentityType", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_IdentityType_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_IdentityType_UpdateIdentityType", "Update the Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_IdentityType_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_IdentityType_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_IdentityType_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_IdentityType_MFC_SCP_CSC_ControlForm", "MFC SCP CSC ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))
}
observeEvent(input$project_specific_admin_IdentityType, {
  showModal(
    project_specific_admin_IdentityType_Modal("IdentityType")
  )
})


# reactive values

loadIdentityType <- reactive({
  pool %>% tbl("tblIdentityType") %>% collect()
})

CV <- reactiveValues()

output$project_specific_admin_IdentityType_Table <- renderRHandsontable({
  dt <- CV$Data <- loadIdentityType()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$project_specific_admin_IdentityType_Table_select$select$r,{
  
  temp <- isolate(input$project_specific_admin_IdentityType_Table)
  df <- hot_to_r(temp)
  
  r <- input$project_specific_admin_IdentityType_Table_select$select$r
  updateTextInput(session, "project_specific_admin_IdentityType_UpdateIdentityType", label = "", value = df[r, 1])
  updateTextInput(session, "project_specific_admin_IdentityType_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$project_specific_admin_IdentityType_UpdateIdentityType,{
  temp <- isolate(input$project_specific_admin_IdentityType_Table)
  df <- hot_to_r(temp)
  
  id=input$project_specific_admin_IdentityType_UpdateIdentityType
  col="Description"
  value=input$project_specific_admin_IdentityType_UpdateDescription
  
  sql <- "UPDATE tblIdentityType SET Description = ?value, IdentityType = ?id2 WHERE IdentityType = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadIdentityType()
  CV$Data
  shinyjs::js$refresh()
})


## add new IdentityType

# define fields to add

observeEvent(input$project_specific_admin_IdentityType_AddNewIdentityType,{
  req(input$project_specific_admin_IdentityType_AddNewIdentityType)
  dt <- CV$Data <- loadIdentityType()
  df <- data.frame(IdentityType = input$project_specific_admin_IdentityType_AddNewIdentityType, Description = input$project_specific_admin_IdentityType_AddNewDescription)
  if((df$IdentityType %in% dt$IdentityType)==TRUE){
    shinyalert("Oops!", "IdentityType Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblIdentityType', df, append = T)
    shinyalert("Success!", "IdentityType Added", type = "success")
  }
})


observeEvent(input$project_specific_admin_IdentityType_Refresh,{
  CV$Data <- loadIdentityType()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$project_specific_admin_IdentityType_Clear, {
  reset(input$project_specific_admin_IdentityType_Form)
  updateTextInput(session,  inputId = "project_specific_admin_IdentityType_AddNewIdentityType", "Identity Type", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_IdentityType_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "project_specific_admin_IdentityType_UpdateIdentityType", "", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_IdentityType_UpdateDescription", "", value = "")
})

observeEvent(input$project_specific_admin_IdentityType_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$project_specific_admin_IdentityType_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

