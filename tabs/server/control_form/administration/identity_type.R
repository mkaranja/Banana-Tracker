
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

admin_IdentityType_Modal <-  function(text){
  modalDialog(size = "l",
              title = tags$h2(style="color:#800000;","Identity Type"),
              useShinyalert(), 
              shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
              div(id = "admin_IdentityType_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("admin_IdentityType_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("admin_IdentityType_AddNewIdentityType", "Identity Type", width = "100%")),
                           column(6, textInput("admin_IdentityType_AddNewDescription", "Description", width = "100%")),
                           column(2, br(), actionBttn("admin_IdentityType_AddNewIdentityType", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("admin_IdentityType_UpdateIdentityType", "", width = "100%"))),
                           column(6, textInput("admin_IdentityType_UpdateDescription", "", width = "100%")),
                           column(2, br(), actionBttn("admin_IdentityType_UpdateIdentityType", "Update", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("admin_IdentityType_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("admin_IdentityType_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
                       column(2, actionBttn("admin_IdentityType_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("admin_IdentityType_MFC_SCP_CSC_ControlForm", "ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))
}
observeEvent(input$admin_IdentityType, {
  showModal(
    admin_IdentityType_Modal("IdentityType")
  )
})


# reactive values

loadIdentityType <- reactive({
   pool %>% tbl("tblIdentityType") %>% collect()
})

CV <- reactiveValues()

output$admin_IdentityType_Table <- renderRHandsontable({
  dt <- CV$Data <- loadIdentityType()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$admin_IdentityType_Table_select$select$r,{
  
  temp <- isolate(input$admin_IdentityType_Table)
  df <- hot_to_r(temp)
  
  r <- input$admin_IdentityType_Table_select$select$r
  updateTextInput(session, "admin_IdentityType_UpdateIdentityType", label = "", value = df[r, 1])
  updateTextInput(session, "admin_IdentityType_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$admin_IdentityType_UpdateIdentityType,{
  temp <- isolate(input$admin_IdentityType_Table)
  df <- hot_to_r(temp)
  
  id=input$admin_IdentityType_UpdateIdentityType
  col="Description"
  value=input$admin_IdentityType_UpdateDescription
  
  sql <- "UPDATE tblIdentityType SET Description = ?value, IdentityType = ?id2 WHERE IdentityType = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadIdentityType()
  CV$Data
  shinyjs::js$refresh()
})


## add new IdentityType

# define fields to add

observeEvent(input$admin_IdentityType_AddNewIdentityType,{
  req(input$admin_IdentityType_AddNewIdentityType)
  dt <- CV$Data <- loadIdentityType()# admin_IdentityType_AddNewIdentityType
  df <- data.frame(IdentityType = input$admin_IdentityType_AddNewIdentityType, Description = input$admin_IdentityType_AddNewDescription)
  if((df$IdentityType %in% dt$IdentityType)==TRUE){
    shinyalert("Oops!", "IdentityType Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblIdentityType', df, append = T)
    shinyalert("Success!", "IdentityType Added", type = "success")
  }
})


observeEvent(input$admin_IdentityType_Refresh,{
  CV$Data <- loadIdentityType()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$admin_IdentityType_Clear, {
  reset(input$admin_IdentityType_Form)
  updateTextInput(session,  inputId = "admin_IdentityType_AddNewIdentityType", "Identity Type", value = "")
  updateTextInput(session,  inputId = "admin_IdentityType_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "admin_IdentityType_UpdateIdentityType", "", value = "")
  updateTextInput(session,  inputId = "admin_IdentityType_UpdateDescription", "", value = "")
})

observeEvent(input$admin_IdentityType_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$admin_IdentityType_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

