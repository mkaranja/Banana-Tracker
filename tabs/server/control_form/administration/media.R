
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

admin_Media_Modal <-  function(text){
  modalDialog(size = "l",
              title = tags$h2(style="color:#800000;","Media"),
              useShinyalert(), 
              shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
              div(id = "admin_Media_Form",
                  fluidRow(
                    column(10, offset = 1,
                           rHandsontableOutput("admin_Media_Table", height = "200px"), 
                           tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                           column(3, textInput("admin_Media_AddNewMedia", "Media", value = "", width = "100%")),
                           column(6, textInput("admin_Media_AddNewDescription", "Description",value = "", width = "100%")),
                           column(2, br(), actionBttn("admin_Media_AddNewIdentityType", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
                    ),
                    column(10, offset = 1, 
                           column(3, disabled(textInput("admin_Media_UpdateMedia", "", value = "", width = "100%"))),
                           column(6, textInput("admin_Media_UpdateDescription", "", value="", width = "100%")),
                           column(2, br(), actionBttn("admin_Media_UpdateIdentityType", "Update", style = "jelly", size = "xs", color = "primary", block=T))
                    )
                  )
              ), br(), br(),
              fluidRow(
                column(10, offset = 1,
                       column(2, actionBttn("admin_Media_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("admin_Media_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
                       column(2, actionBttn("admin_Media_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("admin_Media_MFC_SCP_CSC_ControlForm", "ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
                )
              ))
}
observeEvent(input$admin_Media, {
  showModal(
    admin_Media_Modal("Media")
  )
})


# reactive values

loadMedia <- reactive({
  pool %>% tbl("tblMedia") %>% collect()
})

CV <- reactiveValues()

output$admin_Media_Table <- renderRHandsontable({
  dt <- CV$Data <- loadMedia()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$admin_Media_Table_select$select$r,{
  
  temp <- isolate(input$admin_Media_Table)
  df <- hot_to_r(temp)
  
  r <- input$admin_Media_Table_select$select$r
  updateTextInput(session, "admin_Media_UpdateMedia", label = "", value = df[r, 1])
  updateTextInput(session, "admin_Media_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$admin_Media_UpdateIdentityType,{
  temp <- isolate(input$admin_Media_Table)
  df <- hot_to_r(temp)
  
  id=input$admin_Media_UpdateMedia
  col="Description"
  value=input$admin_Media_UpdateDescription
  
  sql <- "UPDATE tblMedia SET Description = ?value, Media = ?id2 WHERE Media = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadMedia()
  CV$Data
  shinyjs::js$refresh()
})


## add new Media

# define fields to add

observeEvent(input$admin_Media_AddNewIdentityType,{
  req(input$admin_Media_AddNewMedia)
  dt <- CV$Data <- loadMedia()
  df <- data.frame(Media = input$admin_Media_AddNewMedia, Description = input$admin_Media_AddNewDescription)
  if((df$Media %in% dt$Media)==TRUE){
    shinyalert("Oops!", "Media Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblMedia', df, append = T)
    shinyalert("Success!", "Media Added", type = "success")
  }
})


observeEvent(input$admin_Media_Refresh,{
  CV$Data <- loadMedia()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$admin_Media_Clear, {
  reset(input$admin_Media_Form)
  updateTextInput(session,  inputId = "admin_Media_AddNewMedia", "Media", value = "")
  updateTextInput(session,  inputId = "admin_Media_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "admin_Media_UpdateMedia", "", value = "")
  updateTextInput(session,  inputId = "admin_Media_UpdateDescription", "", value = "")
})

observeEvent(input$admin_Media_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$admin_Media_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

