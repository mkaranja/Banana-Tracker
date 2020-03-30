
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

project_specific_admin_Additives_Modal <-  function(text){
  modalDialog(size = "l",
              title = tags$h2(style="color:#800000;","Additives"),
              useShinyalert(), 
              shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
              div(id = "project_specific_admin_Additives_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("project_specific_admin_Additives_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("project_specific_admin_Additives_AddNewAdditives", "Additives", value = "", width = "100%")),
                           column(6, textInput("project_specific_admin_Additives_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Additives_AddNewIdentityType", "Add New Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("project_specific_admin_Additives_UpdateAdditives", "", value = "", width = "100%"))),
                           column(6, textInput("project_specific_admin_Additives_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("project_specific_admin_Additives_UpdateIdentityType", "Update the Identity Type", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("project_specific_admin_Additives_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Additives_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Additives_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("project_specific_admin_Additives_MFC_SCP_CSC_ControlForm", "MFC SCP CSC ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))
}
observeEvent(input$project_specific_admin_Additives, {
  showModal(
    project_specific_admin_Additives_Modal("Additives")
  )
})


# reactive values

loadAdditives <- reactive({
  pool %>% tbl("tblAdditives") %>% collect()
})

CV <- reactiveValues()

output$project_specific_admin_Additives_Table <- renderRHandsontable({
  dt <- CV$Data <- loadAdditives()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$project_specific_admin_Additives_Table_select$select$r,{
  
  temp <- isolate(input$project_specific_admin_Additives_Table)
  df <- hot_to_r(temp)
  
  r <- input$project_specific_admin_Additives_Table_select$select$r
  updateTextInput(session, "project_specific_admin_Additives_UpdateAdditives", label = "", value = df[r, 1])
  updateTextInput(session, "project_specific_admin_Additives_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$project_specific_admin_Additives_UpdateIdentityType,{
  temp <- isolate(input$project_specific_admin_Additives_Table)
  df <- hot_to_r(temp)
  
  id=input$project_specific_admin_Additives_UpdateAdditives
  col="Description"
  value=input$project_specific_admin_Additives_UpdateDescription
  
  sql <- "UPDATE tblAdditives SET Description = ?value, Additives = ?id2 WHERE Additives = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadAdditives()
  CV$Data
  shinyjs::js$refresh()
})


## add new Additives

# define fields to add

observeEvent(input$project_specific_admin_Additives_AddNewIdentityType,{
  req(input$project_specific_admin_Additives_AddNewAdditives)
  dt <- CV$Data <- loadAdditives()
  df <- data.frame(Additives = input$project_specific_admin_Additives_AddNewAdditives, Description = input$project_specific_admin_Additives_AddNewDescription)
  if((df$Additives %in% dt$Additives)==TRUE){
    shinyalert("Oops!", "Additives Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblAdditives', df, append = T)
    shinyalert("Success!", "Additives Added", type = "success")
  }
})


observeEvent(input$project_specific_admin_Additives_Refresh,{
  CV$Data <- loadAdditives()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$project_specific_admin_Additives_Clear, {
  reset(input$project_specific_admin_Additives_Form)
  updateTextInput(session,  inputId = "project_specific_admin_Additives_AddNewAdditives", "Additives", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_Additives_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "project_specific_admin_Additives_UpdateAdditives", "", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_Additives_UpdateDescription", "", value = "")
})

observeEvent(input$project_specific_admin_Additives_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$project_specific_admin_Additives_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

