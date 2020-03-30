
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

project_specific_admin_Source_Modal <-  function(text){
  modalDialog(size = "l",
              title = tags$h2(style="color:#800000;","Source"),
              useShinyalert(), 
              shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
              div(id = "project_specific_admin_Source_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_Source_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_Source_AddNewSource", "Source", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_Source_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Source_AddNewIdentityType", "Add New Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_Source_UpdateSource", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_Source_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Source_UpdateIdentityType", "Update the Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Source_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Source_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Source_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Source_MFC_SCP_CSC_ControlForm", "MFC SCP CSC ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))
}
observeEvent(input$project_specific_admin_Source, {
  showModal(
    project_specific_admin_Source_Modal("Source")
  )
})


# reactive values

loadSource <- reactive({
  pool %>% tbl("tblSource") %>% collect()
})

CV <- reactiveValues()

output$project_specific_admin_Source_Table <- renderRHandsontable({
  dt <- CV$Data <- loadSource()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$project_specific_admin_Source_Table_select$select$r,{
  
  temp <- isolate(input$project_specific_admin_Source_Table)
  df <- hot_to_r(temp)
  
  r <- input$project_specific_admin_Source_Table_select$select$r
  updateTextInput(session, "project_specific_admin_Source_UpdateSource", label = "", value = df[r, 1])
  updateTextInput(session, "project_specific_admin_Source_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$project_specific_admin_Source_UpdateIdentityType,{
  temp <- isolate(input$project_specific_admin_Source_Table)
  df <- hot_to_r(temp)
  
  id=input$project_specific_admin_Source_UpdateSource
  col="Description"
  value=input$project_specific_admin_Source_UpdateDescription
  
  sql <- "UPDATE tblSource SET Description = ?value, Source = ?id2 WHERE Source = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadSource()
  CV$Data
  shinyjs::js$refresh()
})


## add new Source

# define fields to add

observeEvent(input$project_specific_admin_Source_AddNewIdentityType,{
  req(input$project_specific_admin_Source_AddNewSource)
  dt <- CV$Data <- loadSource()
  df <- data.frame(Source = input$project_specific_admin_Source_AddNewSource, Description = input$project_specific_admin_Source_AddNewDescription)
  if((df$Source %in% dt$Source)==TRUE){
    shinyalert("Oops!", "Source Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblSource', df, append = T)
    shinyalert("Success!", "Source Added", type = "success")
  }
})


observeEvent(input$project_specific_admin_Source_Refresh,{
  CV$Data <- loadSource()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$project_specific_admin_Source_Clear, {
  reset(input$project_specific_admin_Source_Form)
  updateTextInput(session,  inputId = "project_specific_admin_Source_AddNewSource", "Source", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_Source_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "project_specific_admin_Source_UpdateSource", "", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_Source_UpdateDescription", "", value = "")
})

observeEvent(input$project_specific_admin_Source_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$project_specific_admin_Source_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

