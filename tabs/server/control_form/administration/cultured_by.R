
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

admin_CulturedBy_Modal <-  function(text){
  modalDialog(size = "l",
              title = tags$h2(style="color:#800000;","Cultured By"),
              useShinyalert(), 
              shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
              div(id = "admin_CulturedBy_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("admin_CulturedBy_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("admin_CulturedBy_AddNewCulturedBy", "Cultured By", value = "", width = "100%")),
                           column(6, textInput("admin_CulturedBy_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("admin_CulturedBy_AddNewIdentityType", "Add New Cultured By", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("admin_CulturedBy_UpdateCulturedBy", "", value = "", width = "100%"))),
                           column(6, textInput("admin_CulturedBy_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("admin_CulturedBy_UpdateIdentityType", "Update the Cultured By", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("admin_CulturedBy_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("admin_CulturedBy_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("admin_CulturedBy_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("admin_CulturedBy_MFC_SCP_CSC_ControlForm", "MFC SCP CSC ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))
}
observeEvent(input$admin_CulturedBy, {
  showModal(
    admin_CulturedBy_Modal("CulturedBy")
  )
})


# reactive values

loadCulturedBy <- reactive({
  pool %>% tbl("tblCulturedBy") %>% collect()
})

CV <- reactiveValues()

output$admin_CulturedBy_Table <- renderRHandsontable({
  dt <- CV$Data <- loadCulturedBy()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$admin_CulturedBy_Table_select$select$r,{
  
  temp <- isolate(input$admin_CulturedBy_Table)
  df <- hot_to_r(temp)
  
  r <- input$admin_CulturedBy_Table_select$select$r
  updateTextInput(session, "admin_CulturedBy_UpdateCulturedBy", label = "", value = df[r, 1])
  updateTextInput(session, "admin_CulturedBy_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$admin_CulturedBy_UpdateIdentityType,{
  temp <- isolate(input$admin_CulturedBy_Table)
  df <- hot_to_r(temp)
  
  id=input$admin_CulturedBy_UpdateCulturedBy
  col="Description"
  value=input$admin_CulturedBy_UpdateDescription
  
  sql <- "UPDATE tblCulturedBy SET Description = ?value, CulturedBy = ?id2 WHERE CulturedBy = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadCulturedBy()
  CV$Data
  shinyjs::js$refresh()
})


## add new CulturedBy

# define fields to add

observeEvent(input$admin_CulturedBy_AddNewIdentityType,{
  req(input$admin_CulturedBy_AddNewCulturedBy)
  dt <- CV$Data <- loadCulturedBy()
  df <- data.frame(CulturedBy = input$admin_CulturedBy_AddNewCulturedBy, Description = input$admin_CulturedBy_AddNewDescription)
  if((df$CulturedBy %in% dt$CulturedBy)==TRUE){
    shinyalert("Oops!", "CulturedBy Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblCulturedBy', df, append = T)
    shinyalert("Success!", "CulturedBy Added", type = "success")
  }
})


observeEvent(input$admin_CulturedBy_Refresh,{
  CV$Data <- loadCulturedBy()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$admin_CulturedBy_Clear, {
  reset(input$admin_CulturedBy_Form)
  updateTextInput(session,  inputId = "admin_CulturedBy_AddNewCulturedBy", "Cultured By", value = "")
  updateTextInput(session,  inputId = "admin_CulturedBy_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "admin_CulturedBy_UpdateCulturedBy", "", value = "")
  updateTextInput(session,  inputId = "admin_CulturedBy_UpdateDescription", "", value = "")
})

observeEvent(input$admin_CulturedBy_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$admin_CulturedBy_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

