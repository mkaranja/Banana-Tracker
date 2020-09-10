
#jscode <- "shinyjs.refresh = function() { location.reload(); }"

admin_Cultivar_Modal <-  function(text){
    modalDialog(size = "l",
         title = tags$h2(style="color:#800000;","Cultivar"),
         useShinyalert(), 
         shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
         div(id = "admin_Cultivar_Form",
           fluidRow(
             column(10, offset = 1,
                    rHandsontableOutput("admin_Cultivar_Table", height = "200px"), 
                    tags$style(type="text/css", "#table1 th {font-weight:bold;}"), br(),
                    column(3, textInput("admin_Cultivar_AddNewCultivar", "Cultivar", value = "", width = "100%")),
                    column(6, textInput("admin_Cultivar_AddNewDescription", "Description",value = "", width = "100%")),
                    column(2, br(), actionBttn("admin_Cultivar_AddNewIdentityType", "Add New", style = "jelly", size = "xs", color = "primary", block=T))
             ),
             column(10, offset = 1, 
                    column(3, disabled(textInput("admin_Cultivar_UpdateCultivar", "", value = "", width = "100%"))),
                    column(6, textInput("admin_Cultivar_UpdateDescription", "", value="", width = "100%")),
                    column(2, br(), actionBttn("admin_Cultivar_UpdateIdentityType", "Update", style = "jelly", size = "xs", color = "primary", block=T))
             )
           )
           ), br(), br(),
         fluidRow(
           column(10, offset = 1,
                  column(2, actionBttn("admin_Cultivar_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "primary", block=T)),
                  column(2, actionBttn("admin_Cultivar_Clear", "Clear", style = "jelly", size = "xs", color = "warning", block=T)),
                  column(2, actionBttn("admin_Cultivar_Refresh", "Refresh", style = "jelly", size = "xs", color = "primary", block=T)),
                  column(2, actionBttn("admin_Cultivar_MFC_SCP_CSC_ControlForm", "ControlForm", style = "jelly", size = "xs", color = "warning", block=T))
           )
         ))
}
observeEvent(input$admin_Cultivar, {
  showModal(
    admin_Cultivar_Modal("Cultivar")
  )
})


# reactive values

loadCultivar <- reactive({
  pool %>% tbl("tblCultivar") %>% collect()
})

CV <- reactiveValues()

output$admin_Cultivar_Table <- renderRHandsontable({
  dt <- CV$Data <- loadCultivar()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})


## update

observeEvent(input$admin_Cultivar_Table_select$select$r,{
  
  temp <- isolate(input$admin_Cultivar_Table)
  df <- hot_to_r(temp)
  
  r <- input$admin_Cultivar_Table_select$select$r
  updateTextInput(session, "admin_Cultivar_UpdateCultivar", label = "", value = df[r, 1])
  updateTextInput(session, "admin_Cultivar_UpdateDescription", label = "", value = df[r, 2])
})


# 
observeEvent(input$admin_Cultivar_UpdateIdentityType,{
  temp <- isolate(input$admin_Cultivar_Table)
  df <- hot_to_r(temp)
  
  id=input$admin_Cultivar_UpdateCultivar
  col="Description"
  value=input$admin_Cultivar_UpdateDescription
  
  sql <- "UPDATE tblCultivar SET Description = ?value, Cultivar = ?id2 WHERE Cultivar = ?id1;"
  query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
  dbExecute(pool, query)
  
  CV$Data <- loadCultivar()
  CV$Data
  shinyjs::js$refresh()
})


## add new cultivar

# define fields to add

observeEvent(input$admin_Cultivar_AddNewIdentityType,{
  req(input$admin_Cultivar_AddNewCultivar)
  dt <- CV$Data <- loadCultivar()
  df <- data.frame(Cultivar = input$admin_Cultivar_AddNewCultivar, Description = input$admin_Cultivar_AddNewDescription)
  if((df$Cultivar %in% dt$Cultivar)==TRUE){
    shinyalert("Oops!", "Cultivar Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblCultivar', df, append = T)
    shinyalert("Success!", "Cultivar Added", type = "success")
  }
})


observeEvent(input$admin_Cultivar_Refresh,{
  CV$Data <- loadCultivar()
  CV$Data
  shinyjs::js$refresh()
})


observeEvent(input$admin_Cultivar_Clear, {
  reset(input$admin_Cultivar_Form)
  updateTextInput(session,  inputId = "admin_Cultivar_AddNewCultivar", "Cultivar", value = "")
  updateTextInput(session,  inputId = "admin_Cultivar_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "admin_Cultivar_UpdateCultivar", "", value = "")
  updateTextInput(session,  inputId = "admin_Cultivar_UpdateDescription", "", value = "")
})

observeEvent(input$admin_Cultivar_MFC_SCP_CSC_ControlForm,{
  removeModal()
})

observeEvent(input$admin_Cultivar_MFC_SCP_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

