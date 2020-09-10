
tab_files <- list.files(path = "tabs/server/control_form/data_form/cell_suspension_culture_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


observeEvent(input$cell_suspension_culture_module,{
  
  showModal(modalDialog(tags$h3(style="color:#800000;text-align:center;","Cell Suspension Culture Module"),
                         
      tabsetPanel( type = "pills", 
                   search_CSC,
                   new_cell_suspension_culture,
                   update_last_subculture,
                   labels_CSC
                   
      ), easyClose = F, size = "l"
  ))
  
})


# Load data
csc_data <- reactive({
  tbl(pool, "tblCSC") %>% collect()
})
csc_val <- reactiveValues()




## --------------------------------------------------------- SEARCH CSC ------------------------------------------------------------------------------
output$search_CSC_Fields_Output <- renderUI({
  
  csc_val$Data<- csc_data()
  df <- csc_val$Data 
  
  div(
    column(2, selectInput("search_CSC_Identify", "Identify", choices = c('', df$CSCIdentity), selected = '')),
    column(2, selectInput("search_CSC_Source", "Source", choices = c('', df$Source), selected = '')),
    column(2, selectInput("search_CSC_Cultivar", "Cultivar", choices = c('', df$Cultivar), selected = '')),
    column(2, selectInput("search_CSC_CultivarConfirmed", "Cultivar Confirmed", choices = c("","Yes","No"))),
    column(2, selectInput("search_CSC_VirusIndexed", "Virus Indexed", choices = c("","Yes","No"))),
    column(2, selectInput("search_CSC_PermitNumber", "Permit Number", choices = c('')))
  )
})

observeEvent(input$new_CSC_ReadyToCulture,{
  cultured_by <- tbl(pool, "tblCulturedBy") %>% collect()
  updateSelectInput(session, "new_CSC_CulturedBy", "Cultured By", choices = c('', cultured_by$CulturedBy))
})
output$search_CSC_DateOfStarterCulture_Output <- renderUI({
  csc_val$Data<- csc_data()
  df <- csc_val$Data 
  
  dateRangeInput("search_CSC_DateOfStarterCulture", "Date of Starter Culture", 
                 min = min(df$DateOfStarterCulture), max = max(df$DateOfStarterCulture), 
                 start = min(df$DateOfStarterCulture), end = max(df$DateOfStarterCulture), separator = " TO ")
})

search_CSC_ResultsTable_Input <- reactive({
  
  csc_val$Data<- csc_data()
  dt <- csc_val$Data 
  
  if(nchar(input$search_CSC_Identify) > 0){
    dt <- dt %>%
      dplyr::filter(ExplantIdentity == input$search_CSC_Identify)
  }
  if(nchar(input$search_CSC_Source)>0){
    dt <- dt %>%
      dplyr::filter(Source == input$search_CSC_Source)
  }
  if(nchar(input$search_CSC_Cultivar)>0){
    dt <- dt %>%
      dplyr::filter(Cultivar == input$search_CSC_Cultivar)
  } 
  if(nchar(input$search_CSC_CultivarConfirmed)>0){
    dt <- dt %>%
      dplyr::filter(CultivarConfirmed == input$search_CSC_CultivarConfirmed)
  }
  if(nchar(input$search_CSC_VirusIndexed)>0){
    dt <- dt %>%
      dplyr::filter(VirusIndexed == input$search_CSC_VirusIndexed)
  }
  if(nchar(input$search_CSC_PermitNumber)>0){
    dt <- dt %>%
      dplyr::filter(PermitNumber1 == input$search_CSC_PermitNumber)
  }
  # if(!is.na(input$search_CSC_DateOfStarterCulture)){
  dt <- dt %>%
    dplyr::filter(between(lubridate::ymd(DateOfStarterCulture), input$search_CSC_DateOfStarterCulture[1], input$search_CSC_DateOfStarterCulture[2]))
  # }
  dt
})


# Table results
output$search_CSC_ResultsTable_Output <- renderUI({
  # Search Results Table
  if(input$search_CSC_SearchDeletedCSC == FALSE){
    rHandsontableOutput("search_CSC_ResultsTable")
  } else {
    # Search Deleted CSC
    rHandsontableOutput("search_CSC_SearchDeletedCSC_ResultsTable")
  }
})



# Search Results
observeEvent(input$search_CSC_ActionSearch,{
  
  if(nchar(input$search_CSC_Identify) == 0 && nchar(input$search_CSC_Source) == 0 && nchar(input$search_CSC_Cultivar) == 0 &&
     nchar(input$search_CSC_CultivarConfirmed) == 0 && nchar(input$search_CSC_VirusIndexed) == 0 && 
     nchar(input$search_CSC_PermitNumber) == 0 && is.na(input$search_CSC_DateOfStarterCulture)){
    shinyalert("Oops!", "Select one of the search criteria", type = "warning")
  } else {
    tryCatch({
      output$search_CSC_ResultsTable <- renderRHandsontable({
        dt <- isolate(search_CSC_ResultsTable_Input())
        
        rhandsontable(dt, selectCallback = T, readOnly = T, rowHeaders=F) %>%
          hot_table(stretchH = "all")
      })
    })
  }
})

# reset on click on the dropdown button (or you can reset on send button if you prefer)
search_CSC_ActionSearch <- reactiveValues(value=NULL)
observeEvent(input[["sw-btn-MY_BTN"]], {
  search_CSC_ActionSearch$value <- NULL # or FALSE
}, ignoreInit = TRUE)



observeEvent(input$search_CSC_ResultsTable_select$select$r,{
  dv <- search_CSC_ResultsTable_Input()
  df <- tbl(pool, "tblCultures") %>% collect()
  r <- input$search_CSC_ResultsTable_select$select$r
  c <- dv[r,'ExplantIdentity']
  if(length(c)>0)
    dt = df[df$ExplantIdentity == c$ExplantIdentity, ]
  
  output$search_CSC_ExplantIdentiyResultsTable <- renderRHandsontable({
    rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
      hot_table(stretchH = "all", columnSorting = TRUE)
  })
  
  ## update Explant Identity
  # updateSelectInput(session, "search_CSC_Identify", "Identify", choices = c('', CSC$ExplantIdentity), selected = c$ExplantIdentity)
  
  observeEvent(input$search_CSC_ActionCulture,{
    f <- input$search_CSC_ExplantIdentiyResultsTable_select$select$r
    h <-  as.character(df[f,'ExplantIdentity'])
    if(length(f)>0){
      id <- h #input$search_CSC_Identify
      
      output$search_CSC_ActionCulture_Output <- renderUI({
        cultured_by <- tbl(pool, "tblCulturedBy") %>% collect()
        media <- tbl(pool, "tblMedia") %>% collect()
        additives <- tbl(pool, "tblAdditives") %>% collect()
        div(
          panel_div(class_type = "default",
                    content = tags$div(
                      
                      column(2,
                             div(id = "search_CSC_Culture_Form",
                                 disabled(textInput("search_CSC_SelectedIdentity","Selected Identity", value = id, width = "100%")),
                                 numericInput("search_CSC_NumberOfCultures",labelMandatory("Number of Cultures"), min = 0, value = NULL, width = "100%"),
                                 dateInput("search_CSC_DateOfCulture",labelMandatory("Date of Cultures"), width = "100%"),
                                 selectInput("search_CSC_CulturedBy",labelMandatory("Cultured By"), choices = c('', cultured_by$CulturedBy), width = "100%"),
                                 selectInput("search_CSC_MediaForCultures",labelMandatory("Media"), choices = c('', media$Media), width = "100%"),
                                 numericInput("search_CSC_LabBookNumberForCultures",labelMandatory("Lab Book Number"), min = 0, value = NULL, width = "100%"),
                                 numericInput("search_CSC_PageNumberForCultures",labelMandatory("Page Number"), min = 0, value = NULL, width = "100%"),
                                 textInput("search_CSC_Comments", "Comments", width = "100%")
                             )
                      ),
                      column(2, br(), br(),br(),br(), br(),br(),
                             panel_div(class_type = "default",
                                       content = tags$div(
                                         tags$b("Additives"),
                                         awesomeCheckboxGroup(inputId = "search_CSC_AdditivesForCultures",
                                                              label = "", choices = c(additives$Additives), selected = NULL, status = "info")
                                       )
                             )
                      ),
                      column(3,
                             conditionalPanel(
                               condition = "input.search_CSC_SaveRecord",
                               panel_div(class_type = "default",
                                         content = tags$div(
                                           tags$b("Select the Fields"),
                                           awesomeCheckboxGroup(inputId = "search_CSC_SelectTheFields", label = "",
                                                                choices = c("ExplantIdentity","IdentityType","Cultivar","Source", "DateOfStarterCulture",
                                                                            "Media","Additives","LabBookNumber","PageNumber", "NumberOfCultures","DateOfCulture",
                                                                            "CulturedBy","MediaForCultures","AdditivesForCultures","LabBookNumberForCultures","PageNumberForCultures"), 
                                                                selected = c("ExplantIdentity","IdentityType","Cultivar","Source", "DateOfStarterCulture","Media",
                                                                             "Additives","LabBookNumber","PageNumber", "NumberOfCultures","DateOfCulture","CulturedBy",
                                                                             "MediaForCultures","AdditivesForCultures","LabBookNumberForCultures","PageNumberForCultures"), status = "info")
                                         )), br(), br(),
                               downloadBttn("search_CSC_ExportToExcel", "Export to Excel", style = "bordered", size = "xs", color = "primary")
                             )
                      ),
                      column(12, offset = 6, actionBttn("search_CSC_SaveRecord", "Save Record", style = "fill", size = "xs", color = "primary")
                      )
                    ))
        )
      })
    } else {
      showNotification("Select an Identity", type = "error")
    }
    
  })
  
})


# Clear Form
observeEvent(input$search_CSC_ActionClearForm,{
  confirmSweetAlert(
    session = session,
    inputId = "search_CSC_ActionClearForm_confirm",
    type = "warning",
    title ="", 
    text = "Do you really want to clear the form?",
    btn_labels = c("Cancel", "Yes, clear!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

# Create reactiveValues containing confirmation
search_CSC_ActionClearForm_confirm <- reactiveValues(value = NULL)

# update according to user choice
observeEvent(input$search_CSC_ActionClearForm_confirm, {
  search_CSC_ActionClearForm_confirm$value <- input$search_CSC_ActionClearForm_confirm
  
  if(search_CSC_ActionClearForm_confirm$value == TRUE){
    reset("search_CSC_Form")
    
    # Hide ExplantIdentity
    shinyjs::hide("search_CSC_ResultsTable_Output")
    shinyjs::hide("search_CSC_Culture_Form1")
  }
  
}, ignoreInit = TRUE)

# reset on click on the dropdown button (or you can reset on send button if you prefer)
observeEvent(input[["sw-btn-MY_BTN"]], {
  search_CSC_ActionClearForm_confirm$value <- NULL # or FALSE
}, ignoreInit = TRUE)



# Save Cultured Record

search_CSC_mandatory_fields <- c("search_CSC_NumberOfCultures","search_CSC_CulturedBy",
                                 "search_CSC_MediaForCultures", "search_CSC_LabBookNumberForCultures",
                                 "search_CSC_PageNumberForCultures")
observe({
  # check if all mandatory fields have a value
  mandatoryFilled <-
    vapply(search_CSC_mandatory_fields,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  
  # enable/disable the submit button
  shinyjs::toggleState(id = "search_CSC_SaveRecord", "Save Record", condition = mandatoryFilled)
  
})


observeEvent(input$search_CSC_SaveRecord,{
  
  dt <- data.frame(
    ExplantIdentity = input$search_CSC_SelectedIdentity,
    NumberOfCultures = input$search_CSC_NumberOfCultures,
    DateOfCulture = input$search_CSC_DateOfCulture,
    CulturedBy = input$search_CSC_CulturedBy,
    Comments = ifelse(nchar(input$search_CSC_Comments)>0,input$search_CSC_Comments,''),
    MediaForCultures  = input$search_CSC_MediaForCultures,
    AdditivesForCultures = ifelse(!is.null(input$search_CSC_AdditivesForCultures),input$search_CSC_AdditivesForCultures,''),
    LabBookNumberForCultures = input$search_CSC_LabBookNumberForCultures,
    PageNumberForCultures = input$search_CSC_PageNumberForCultures
  )
  
  try(expr = dbWriteTable(conn = pool,   name = "tblCulturesCSC", value = dt, overwrite = F, append = T))
  shinyalert("", "Record updated", type = "success")
  
  # showNotification("Select an Identity", type = "error")
})

# Export to Excel

search_CSC_ExportToExcel_Input <- reactive({
  dt <- data.frame(
    ExplantIdentity = trimws(input$search_CSC_SelectedIdentity),
    NumberOfCultures = input$search_CSC_NumberOfCultures,
    DateOfCulture = input$search_CSC_DateOfCulture,
    CulturedBy = input$search_CSC_CulturedBy,
    Comments = ifelse(nchar(input$search_CSC_Comments)>0,input$search_CSC_Comments,''),
    MediaForCultures  = input$search_CSC_MediaForCultures,
    AdditivesForCultures = ifelse(!is.null(input$search_CSC_AdditivesForCultures),input$search_CSC_AdditivesForCultures,''),
    LabBookNumberForCultures = input$search_CSC_LabBookNumberForCultures,
    PageNumberForCultures = input$search_CSC_PageNumberForCultures
  )
  csc <- tbl(pool, "tblCSC") %>% collect()
  csc$ExplantIdentity <- trimws(csc$ExplantIdentity)
  
  if (length(input$search_CSC_SelectTheFields) > 0){
    dt <- csc %>%
      dplyr::left_join(dt) 
    dt <- dt %>%
      dplyr::select(!!!input$search_CSC_SelectTheFields)
  }
  dt[rep(row.names(dt), input$search_CSC_NumberOfCultures),]
})

output$search_CSC_ExportToExcel <- downloadHandler(
  filename = function(){paste(input$search_CSC_SelectedIdentity, Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(search_CSC_ExportToExcel_Input(), path = file, col_names = T, format_headers = T )
  }
)

# Delete
observeEvent(input$search_CSC_ActionDelete,{
    confirmSweetAlert(
      session = session,
      inputId = "search_CSC_ActionDelete_confirm",
      type = "warning",
      title = "",
      text = "Do you really want to DELETE the record?",
      btn_labels = c("Cancel", "Yes, clear!"),
      btn_colors = c("#D3D3D3", "#DD6B55")
    )
})



observeEvent(input$search_CSC_ActionDelete_confirm, {
  if(input$search_CSC_ActionDelete_confirm == TRUE){
    dv <- search_CSC_ResultsTable_Input()
    r <- input$search_CSC_ResultsTable_select$select$r
    c <- dv[r,'CSCIdentity']
    id <- c$ExplantIdentity
    
    sql <- "DELETE FROM tblCSC WHERE ExplantIdentity = ?id1;"
    query <- sqlInterpolate(pool, sql, id1 = id)
    dbExecute(pool, query) # delete record
    dbWriteTable( conn = pool,   name = "tblDeletedCSC", value = c, overwrite = F, append = T) # save deleted record
  }
}, ignoreInit = TRUE)



# Search deleted CSC

output$search_CSC_SearchDeletedCSC_ResultsTable <- renderRHandsontable({
  reset("search_CSC_Form")
  dt <- deletedCSC
  rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
    hot_table(stretchH = "all", columnSorting = TRUE)
})






# observeEvent(input$search_CSC_SearchDeletedCSC,{
#    # Hide Explant Identity
#    shinyjs::hide("search_CSC_Culture_Form1")
#
# })
#


observeEvent(input$search_CSC_FormToPicture, {
  js$winprint()
})
observeEvent(input$search_CSC_CSC_SCP_CSC_ControlForm, {
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})


observeEvent(input$search_CSC_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "search_CSC_Exit_Confrim",
    type = "warning",
    title = "",
    text = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$search_CSC_Exit_Confrim, {
  if(input$search_CSC_Exit_Confrim == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)



## --------------------------------------------------------------------------- NEW CSC----------------------------------------------------------------------
# Update + Generate ID

observeEvent(input$cell_suspension_culture_module,{
  csc_val$Data<- csc_data()
  df <- csc_val$Data
  media <- tbl(pool, "tblMedia") %>% collect()
  additives <- tbl(pool, "tblAdditives")  %>% collect()
  
  updateSelectInput(session, "new_CSC_ParentIdentity", "Parent Identity", choices = c("", df$CSCIdentity))
  updateSelectInput(session, "new_CSC_Media", "", choices = c('', media$Media))
  updateSelectInput(session,"new_CSC_Additives", "", choices =  c('', additives$Additives))
})

observeEvent(input$new_CSC_GetData,{
  
  if(input$new_CSC_ParentIdentity == ""){
    shinyalert("", text = "Select Parent Identity", type = "warning")
  }
  
  csc_val$Data<- csc_data()
  df <- csc_val$Data %>%
    dplyr::filter(trimws(ExplantIdentity) == trimws(input$new_CSC_ParentIdentity))
  
  updateTextInput(session, "new_CSC_IdentityType", "Identity Type", value = df$IdentityType[1])
  updateTextInput(session, "new_CSC_Cultivar", "Cultivar", value = df$Cultivar[1])
  # updateTextInput(session, "new_CSC_CultivarConfirmed",tags$p("Cultivar Confirmed"), value = NULL)
  updateTextInput(session, "new_CSC_Source", "Source", value = df$Source[1])
  # updateTextInput(sesson, "new_CSC_VirusIndexed", tags$p("Virus Indexed"), value = NULL)
  
  observeEvent(input$new_CSC_GenerateIdentity, {
    id <- paste0(df$IdentityType[1], substr(input$new_CSC_Year,3,5), (max(as.integer(substr(trimws(df$CSCIdentity),6,20))) + 1), sep = "")
    updateTextInput(session, "new_CSC_Identity", "Identity", value = id)
  })
  
})

# check if mandator firlds are filled
new_CSC_mandatory_fields <- c("new_CSC_ParentIdentity", "new_CSC_Media", "new_CSC_LabBookNumber","new_CSC_PageNumber")

observe({
  # check if all mandatory fields have a value
  mandatoryFilled <-
    vapply(new_CSC_mandatory_fields,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != "" && !is.na(input[[x]])
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  
  # enable/disable the submit button
  shinyjs::toggleState(id = "new_CSC_SaveStarterCulture_Ready_No", "Save Record", condition = mandatoryFilled)
})

# Save New Record
observeEvent(input$new_CSC_SaveStarterCulture_Ready_No, {
  req(input$new_CSC_ParentIdentity)
  req(input$new_CSC_CSCInitialCultureDate)
  req(input$new_CSC_Media)
  req(input$new_CSC_LabBookNumber)
  req(input$new_CSC_PageNumber)
  
  csc_val$Data <- csc_data()
  csc <- tbl(pool, "tblCSC")  %>% collect()
  
  new_csc <- data.frame(
    ExplantIdentity = input$new_CSC_ParentIdentity,
    CSCIdentity = input$new_CSC_Identity,
    IdentityType = input$new_CSC_IdentityType,
    Cultivar = input$new_CSC_Cultivar,
    Source = input$new_CSC_Source,
    DateOfStarterCulture = input$new_CSC_CSCInitialCultureDate,
    Media  = input$new_CSC_Media,
    Additives = ifelse(input$new_CSC_Additives == '','', input$new_CSC_Additives),
    LabBookNumber = input$new_CSC_LabBookNumber,
    PageNumber = input$new_CSC_PageNumber
  )
  
  if((new_csc$CSCIdentity %in% csc$CSCIdentity)==TRUE){
    shinyalert("Oops!", "CSC Identity Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblCSC', new_csc, append = T)
    shinyalert("", "New CSC Added", type = "success")
  }
}, ignoreInit = TRUE)


# Validate required fields
observeEvent(input$new_CSC_SaveStarterCulture_Ready_No, {
  
  if(input$new_CSC_Media==""){
    showNotification("Please select Media", type = "error")
  }
  if(is.null(input$new_CSC_LabBookNumber)){
    showNotification("Please add Lab Book Number", type = "error")
  }
  if(is.null(input$new_CSC_PageNumber)){
    showNotification("Please add Page Number", type = "error")
  }
  
})
observeEvent(input$new_CSC_SaveStarterCulture_Ready_Yes, {
  
  req(input$new_CSC_ParentIdentity)
  req(input$new_CSC_CSCInitialCultureDate)
  req(input$new_CSC_Media)
  req(input$new_CSC_LabBookNumber)
  req(input$new_CSC_PageNumber)
  
  req(input$new_CSC_Identity)
  req(input$new_CSC_NumberOfCultures)
  req(input$new_CSC_DateOfCultures)
  req(input$new_CSC_CulturedBy)
  
  csc_cultures <- tbl(pool, "tblCulturesCSC")  %>% collect() 
  csc_val$Data <- csc_data()
  csc <- csc_val$Data  # tbl(pool, paste0("tblCSC")) # tbl(pool, "tblCSC") %>% collect()# csc_val$Data
  # Export to Excel FIelds
  updateAwesomeCheckboxGroup(session = session, inputId = "select_csc_fields_to_be_exported_to_excel", label = "", choices = c(names(csc)))
  
  new_csc <- data.frame(
    ExplantIdentity = input$new_CSC_ParentIdentity,
    CSCIdentity = input$new_CSC_Identity,
    IdentityType = input$new_CSC_IdentityType,
    Cultivar = input$new_CSC_Cultivar,
    Source = input$new_CSC_Source,
    DateOfStarterCulture = input$new_CSC_CSCInitialCultureDate,
    Media  = ifelse(input$new_CSC_Media == '','',input$new_CSC_Media),
    Additives = ifelse(input$new_CSC_Additives == '','', input$new_CSC_Additives),
    LabBookNumber = ifelse(is.null(input$new_CSC_LabBookNumber),0,input$new_CSC_LabBookNumber),
    PageNumber = ifelse(is.null(input$new_CSC_PageNumber),0,input$new_CSC_PageNumber)
  )
  
  new_csc_cultures <- data.frame(
    CSCIdentity = input$new_CSC_Identity,
    NumberOfCultures = input$new_CSC_NumberOfCultures,
    DateOfCulture = input$new_CSC_DateOfCultures,
    CulturedBy = input$new_CSC_CulturedBy,
    Comments = ifelse(is.null(input$new_CSC_Comments),'',input$new_CSC_Comments),
    MediaForCultures  = ifelse(input$new_CSC_Media == '','',input$new_CSC_Media),
    AdditivesForCultures = ifelse(input$new_CSC_Additives == '','', input$new_CSC_Additives),
    LabBookNumberForCultures = ifelse(is.null(input$new_CSC_LabBookNumber),0,input$new_CSC_LabBookNumber),
    PageNumberForCultures = ifelse(is.null(input$new_CSC_PageNumber),0,input$new_CSC_PageNumber)
  )
  if((new_csc$CSCIdentity %in% csc$CSCIdentity)==TRUE){
    shinyalert("Oops!", "CSC Identity Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblCSC', new_csc, append = T)
    shinyalert("", "New CSC Added", type = "success")
  }
  
  if((new_csc_cultures$CSCIdentity %in% csc_cultures$CSCIdentity)==F){
    dbWriteTable(pool, 'tblCulturesCSC', new_csc_cultures, append = T)
  }
  
}, ignoreInit = TRUE)

# Validate required fields
observeEvent(input$new_CSC_SaveStarterCulture_Ready_Yes, {
  if(input$new_CSC_Media==""){
    showNotification("Please select Media", type = "error")
  }
  if(is.null(input$new_CSC_LabBookNumber)){
    showNotification("Please add Lab Book Number", type = "error") 
  }
  if(is.null(input$new_CSC_PageNumber)){
    showNotification("Please add Page Number", type = "error")
  }
  if(is.null(input$new_CSC_Identity)){
    showNotification("Please click on Generate Identity", type = "error")
  }
  if(is.null(input$new_CSC_NumberOfCultures)){
    showNotification("Please enter Number of Cultures", type = "error")
  }
  if(input$new_CSC_CulturedBy==""){
    showNotification("Please select Cultured By", type = "error")
  }
})
# Export to Excel FIelds
csc_export_to_excel <- reactive({
  new_csc <- data.frame(
    CSCIdentity = input$new_CSC_Identity,
    IdentityType = input$new_CSC_IdentityType,
    Cultivar = input$new_CSC_Cultivar,
    Source = input$new_CSC_Source,
    DateOfStarterCulture = input$new_CSC_CSCInitialCultureDate,
    Media  = ifelse(input$new_CSC_Media == '','',input$new_CSC_Media),
    Additives = ifelse(input$new_CSC_Additives == '','', input$new_CSC_Additives),
    LabBookNumber = ifelse(is.null(input$new_CSC_LabBookNumber),0,input$new_CSC_LabBookNumber),
    PageNumber = ifelse(is.null(input$new_CSC_PageNumber),0,input$new_CSC_PageNumber),
    NumberOfCultures = input$new_CSC_NumberOfCultures,
    DateOfCulture = input$new_CSC_DateOfCultures,
    CulturedBy = input$new_CSC_CulturedBy,
    Comments = ifelse(is.null(input$new_CSC_Comments),'',input$new_CSC_Comments)
  )
  
  if (length(input$select_csc_fields_to_be_exported_to_excel) > 0){
    dt = new_csc %>% 
      dplyr::select(!!!input$select_csc_fields_to_be_exported_to_excel)
  }
  dt[rep(row.names(dt), input$new_CSC_NumberOfCultures),]
})

output$new_CSC_SelectFieldToExportToExcel <- downloadHandler(
  filename = function(){paste0(input$new_CSC_Identity,"_",input$new_CSC_CulturedBy,"_", Sys.time(), '.xls', sep = "")},
  content = function(file) {
    writexl::write_xlsx(csc_export_to_excel(), path = file, col_names = T, format_headers = T )
  }
)

# Clear Form

observeEvent(input$new_CSC_ClearForm,{
  confirmSweetAlert(
    session = session,
    inputId = "new_CSC_ClearForm_Confirm",
    type = "warning",
    title = "",
    text = "Do you really to clear all the fields?",
    btn_labels = c("Cancel", "Yes, Clear!"),
    btn_colors = c("#DD6B55", "#D3D3D3")
  )
})

observeEvent(input$new_CSC_ClearForm_Confirm, {
  if(input$new_CSC_ClearForm_Confirm == TRUE){
    reset('new_CSC_form')
  }
}, ignoreInit = TRUE)

# Exit
observeEvent(input$new_CSC_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "new_CSC_Exit_Confirm",
    type = "warning",
    title = "", 
    text = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$new_CSC_Exit_Confirm, {
  if(input$new_CSC_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)



## --------------------------------------------------------------Updating Last Subculture----------------------------------------------------------------------
output$updating_last_subculture_CSC_CellSuspensionCultureInitiationIdentity_output <- renderUI({
  csc_val$Data<- csc_data()
  dt <- csc_val$Data
  selectInput("updating_last_subculture_CSC_CellSuspensionCultureInitiationIdentity", "", choices = c('', dt$CSCIdentity), width = "100%")
})

update_last_subculture_CSC_input <- reactive({
  csc_val$Data<- csc_data()
  csc_val$Data  %>%
    filter(trimws(CSCIdentity) == trimws(input$updating_last_subculture_CSC_CellSuspensionCultureInitiationIdentity))
})


update_last_subculture_CSC_cultures_input <- reactive({
  tbl(pool, "tblCulturesCSC")  %>% collect() %>%
    filter(trimws(CSCIdentity) == trimws(input$updating_last_subculture_CSC_CellSuspensionCultureInitiationIdentity))
})

update_last_subculture_CSC_values <- reactiveValues()

observeEvent(input$updating_last_subculture_CSC_LoadData,{
  
  update_last_subculture_CSC_values$Data <- update_last_subculture_CSC_cultures_input()
  dt <- update_last_subculture_CSC_values$Data
  
  updateNumericInput(session, "updating_last_subculture_CSC_NumberOfCultures", "Number of Cultures", value = dt$NumberOfCultures)
  updateDateInput(session, "updating_last_subculture_CSC_DateOfCulture", "Date of Culture", value = update_last_subculture_CSC_input()$DateOfStarterCulture)
  updateSelectInput(session, 'updating_last_subculture_CSC_CulturedBy', "Cultured By", choices = c(update_last_subculture_CSC_cultures_input()$CulturedBy), selected = update_last_subculture_CSC_cultures_input()$CulturedBy)
  updateSelectInput(session, "updating_last_subculture_CSC_Media", "Media", choices = c(update_last_subculture_CSC_input()$Media), selected = update_last_subculture_CSC_input()$Media)
  updateTextInput(session, "updating_last_subculture_CSC_Additives", "Additives", value = update_last_subculture_CSC_input()$Additives)
  updateNumericInput(session, "updating_last_subculture_CSC_LabBookNumber", "Lab Book Number", value = update_last_subculture_CSC_input()$LabBookNumber)
  updateNumericInput(session, "updating_last_subculture_CSC_PageNumber", "Page Number", value = update_last_subculture_CSC_input()$PageNumber)
  updateTextAreaInput(session, "updating_last_subculture_CSC_Comments","Comments", value = dt$Comments)
  
})

# Comments nchars
observeEvent(input$updating_last_subculture_CSC_Comments,{
  if(nchar(input$updating_last_subculture_CSC_Comments)>1000){
    updateTextInput(session,'updating_last_subculture_CSC_Comments',value=substr(input$updating_last_subculture_CSC_Comments,1,1000))
    showNotification("Comments: Max length is 1000 characters", type = "error")
  }
})


observeEvent(input$updating_last_subculture_CSC_Update, {
  
  id <- trimws(input$updating_last_subculture_CSC_CellSuspensionCultureInitiationIdentity)
  NumberOfCultures = input$updating_last_subculture_CSC_NumberOfCultures
  Comments = input$updating_last_subculture_CSC_Comments
  
  sql <- "UPDATE tblCulturesCSC SET NumberOfCultures = ?val1, Comments = ?val2 WHERE CSCIdentity = ?id1;"
  query <- sqlInterpolate(pool, sql, val1 = NumberOfCultures, val2 = Comments, id1 = id)
  dbExecute(pool, query)
  
  update_last_subculture_CSC_values$Data <- update_last_subculture_CSC_cultures_input()
  update_last_subculture_CSC_values$Data
  
  shinyalert("", "Subculture Updated", type = "success")
  
  # reset the fields after upload
  selectInput("updating_last_subculture_CSC_CellSuspensionCultureInitiationIdentity", "", choices = NULL)
  updateNumericInput(session, "updating_last_subculture_CSC_NumberOfCultures", "Number of Cultures", value = NULL)
  updateDateInput(session, "updating_last_subculture_CSC_DateOfCulture", "Date of Culture", value = NULL)
  updateSelectInput(session, 'updating_last_subculture_CSC_CulturedBy', "Cultured By", choices = NULL)
  updateSelectInput(session, "updating_last_subculture_CSC_Media", "Media", choices = NULL)
  updateTextInput(session, "updating_last_subculture_CSC_Additives", "Additives", value = NULL)
  updateNumericInput(session, "updating_last_subculture_CSC_LabBookNumber", "Lab Book Number", value = NULL)
  updateNumericInput(session, "updating_last_subculture_CSC_PageNumber", "Page Number", value = NULL)
  updateTextAreaInput(session, "updating_last_subculture_CSC_Comments","Comments", value = NULL)
  
  reset("updating_last_subculture_CSC_Form")
})

# Clear
observeEvent(input$updating_last_subculture_CSC_Clear,{
  confirmSweetAlert(
    session = session,
    inputId = "updating_last_subculture_CSC_Clear_Confirm",
    type = "warning",
    title = "", 
    text = "Do you really want to Clear the fields?",
    btn_labels = c("Cancel", "Yes, Clear!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$updating_last_subculture_CSC_Clear_Confirm, {
  if(input$updating_last_subculture_CSC_Clear_Confirm == TRUE){
    reset("updating_last_subculture_CSC_Form")
  }
}, ignoreInit = TRUE)


observeEvent(input$updating_last_subculture_CSC_FormToPicture, {
  js$winprint()
})


observeEvent(input$updating_last_subculture_CSC_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})


## Exit
observeEvent(input$updating_last_subculture_CSC_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "updating_last_subculture_CSC_Exit_Confirm",
    type = "warning",
    title = "", 
    text = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$updating_last_subculture_CSC_Exit_Confirm, {
  if(input$updating_last_subculture_CSC_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)



## ------------------------------------------------------------------- Labels Tab--------------------------------------------------------------------------

output$labels_CSC_SelectDate_output <- renderUI({
  dt <- tbl(pool, "tblCulturesCSC")  %>% collect()
  dt$DateOfCulture <- lubridate::ymd(dt$DateOfCulture)
  dateInput("labels_CSC_SelectDate", "", value = max(dt$DateOfCulture), 
            min = min(dt$DateOfCulture),max = max(dt$DateOfCulture))
})


labels_CSC_Table_Input <- reactive({
  req(input$labels_CSC_SelectDate)
  tbl(pool, "tblCulturesCSC")  %>% collect() %>%
    dplyr::filter(lubridate::ymd(DateOfCulture) == input$labels_CSC_SelectDate)
})

observeEvent(input$labels_CSC_LOadData,{
  output$labels_CSC_Table <- renderRHandsontable({
    dt <- labels_CSC_Table_Input()
    
    rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
      hot_table(stretchH = "all", columnSorting = TRUE)
  })
})

# Export to Excel

output$labels_CSC_ExportToExcel <- downloadHandler(
  filename = function(){paste("Labels-", Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(labels_CSC_Table_Input(), path = file, col_names = T, format_headers = T )
  }
)


observeEvent(input$labels_CSC_Clear,{
  shinyjs::hide("labels_CSC_Table")
})


# Barcode

labels_CSC_excelLabels_In <- reactive({
  dt <- labels_CSC_Table_Input() %>%
    dplyr::select(CSCIdentity, NumberOfCultures)
  
  dt = data.frame(dt)
  dt = dt[rep(row.names(dt), dt$NumberOfCultures),]
  dt <- dt %>%
    dplyr::select(CSCIdentity) %>%
    dplyr::mutate(label = CSCIdentity)
  dt
})

output$labels_CSC_barcodeLabels <- downloadHandler(
  filename = function(){paste("CSC Labels-", Sys.time(), '.pdf')},
  
  content = function(file) {
    pdf(file, width=8.0, height = 11, paper = 'letter', pagecentre=T) # right align width=6.0 # left width=2.0,
    par(mfrow=c(5, 4),mar=c(2.5,2.5,2.5,2.5), oma=c(2,2,2,2)) # right align mar=c(0,30,3,0)
    for(i in 1:(nrow(labels_CSC_excelLabels_In()))){
      image(qrencode_raster(as.character(labels_CSC_excelLabels_In()[i,1])), # QRcode
            cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F, 
            xlab="", ylab="", subtitle = mtext(paste(as.character(labels_CSC_excelLabels_In()[i,1])), side = 1, line = 0,
                                               outer = F, at = NA, adj = 0, padj = 0.5, cex = 1, col = 1, las=1, font = 10)
      )
      
    }
    dev.off()
  }
)

# 
# for(i in 1:(nrow(labels_CSC_Table_Input()))){
#   image(qrencode_raster(as.character(labels_CSC_Table_Input()[i,2])), # QRcode
#         cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F, 
#         xlab="", ylab="", subtitle = mtext(paste(as.character(labels_CSC_Table_Input()[i,2]),"\n", 
#                                                  labels_CSC_Table_Input()[i,1],"\n", 
#                                                  labels_CSC_Table_Input()[i,4]), side = 4, line = 0,
#                                            outer = F, at = NA, adj = 0, padj = 0.5, cex = 1, col = 1, las=1, font = 10))
#   
# }


output$labels_CSC_excelLabels <- downloadHandler(
  filename = function(){paste("Labels-", Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(labels_CSC_excelLabels_In(), path = file, col_names = T, format_headers = T )
  }
)

# Exit
observeEvent(input$labels_CSC_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "labels_CSC_Exit_Confirm",
    type = "warning",
    title = "",
    text = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$labels_CSC_Exit_Confirm, {
  if(input$labels_CSC_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)



