
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
                   update_last_subcultureCSC,
                   labels_CSC
                   
      ), easyClose = F, size = "l"
  ))
  
})

## ------------------------------------------------------------------------------------------------------------------------------ NEW CSC
# Update + Generate ID
csc_data <- reactive({
  tbl(pool, "tblCSC") %>% collect()
})
csc_val <- reactiveValues()

observeEvent(input$cell_suspension_culture_module,{
  csc_val$Data <- csc_data()
  csc <- csc_val$Data
  updateSelectInput(session, "new_CSC_ParentIdentity", "Parent Identity", choices = c("", csc$ExplantIdentity))
})

observeEvent(input$new_CSC_GetData,{
  
  if(input$new_CSC_ParentIdentity == ""){
    shinyalert("", text = "Select Parent Identity", type = "warning")
  }
  df <- tbl(pool, "tblCSC") %>% collect() %>%
    dplyr::filter(trimws(ExplantIdentity) == trimws(input$new_CSC_ParentIdentity))
  
  updateTextInput(session, "new_CSC_IdentityType", "Identity Type", value = df$IdentityType[1])
  updateTextInput(session, "new_CSC_Cultivar", "Cultivar", value = df$Cultivar[1])
  # updateTextInput(session, "new_CSC_CultivarConfirmed",tags$p("Cultivar Confirmed"), value = NULL)
  updateTextInput(session, "new_CSC_Source", "Source", value = df$Source[1])
  # updateTextInput(sesson, "new_CSC_VirusIndexed", tags$p("Virus Indexed"), value = NULL)
  
  observeEvent(input$new_CSC_GenerateIdentity, {
    id <- paste0(df$IdentityType[1], substr(input$new_CSC_Year,3,5), (max(as.integer(substr(trimws(df$CSCIdentity),6,20))) + 1))
    updateTextInput(session, "new_CSC_Identity", "Identity", value = id)
  })
 
})

# Save New Record
observeEvent(input$new_CSC_SaveStarterCulture_Ready_No, {
  req(input$new_CSC_ParentIdentity)
  req(input$new_CSC_CSCInitialCultureDate)
  req(input$new_CSC_Media)
  req(input$new_CSC_LabBookNumber)
  req(input$new_CSC_PageNumber)
  
  csc_val$Data <- csc_data()
  csc <- tbl(pool, "tblCSC") %>% collect()# csc_val$Data
  
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
    shinyalert("Success!", "New CSC Added", type = "success")
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

  csc_cultures <- tbl(pool, "tblCulturesCSC") %>% collect()
  csc_val$Data <- csc_data()
  csc <- tbl(pool, "tblCSC") %>% collect()# csc_val$Data
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
    shinyalert("Success!", "New CSC Added", type = "success")
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
  filename = function(){paste0(input$new_CSC_Identity,"_",input$new_CSC_CulturedBy,"_", Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(csc_export_to_excel(), path = file, col_names = T, format_headers = T )
  }
)

# Clear Form

observeEvent(input$new_CSC_ClearForm,{
  
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset('new_CSC_form')
    },
    text = "Do you really to clear all the fields?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})

## ------------------------------------------------------------------------------------------------------------------------------------- SEARCH CSC

output$search_CSC_Fields <- renderUI({
  csc <- tbl(pool, "tblCSC") %>% collect() 
  
  div(
    column(2, selectInput("search_CSC_Identify", "Identify", choices = c('', csc$CSCIdentity), selected = '')),
    column(2, selectInput("search_CSC_Source", "Source", choices = c('', csc$Source), selected = '')),
    column(2, selectInput("search_CSC_Cultivar", "Cultivar", choices = c('', csc$Cultivar), selected = '')),
    column(2, selectInput("search_CSC_CultivarConfirmed", "Cultivar Confirmed", choices = c("","Yes","No"))),
    column(2, selectInput("search_CSC_VirusIndexed", "Virus Indexed", choices = c("","Yes","No"))),
    column(2, numericInput("search_CSC_PermitNUmber", "Permit Number", value = NULL))
  )
})
 
output$search_CSC_DateOfStarterCulture_output <- renderUI({
  csc_val$Data <- csc_data()
  csc <- csc_val$Data
  dateRangeInput("search_CSC_DateOfStarterCulture", "Date of Starter Culture", start = min(csc$DateOfStarterCulture), end = max(csc$DateOfStarterCulture), 
                 min = min(csc$DateOfStarterCulture), max = max(csc$DateOfStarterCulture), separator = "  TO  ")
})
search_CSC_ResultsTable_Input <- reactive({
  #csc_val$Data <- csc_data()
  csc <- tbl(pool, "tblCSC") %>% collect()
  
  if(input$search_CSC_Identify !=''){
    csc <- csc %>%
      dplyr::filter(trimws(CSCIdentity) == trimws(input$search_CSC_Identify))
  }
  if(input$search_CSC_Source!=''){
    csc <- csc %>%
      dplyr::filter(trimws(Source) == trimws(input$search_CSC_Source))
  }
  if(input$search_CSC_Cultivar !=''){
    csc <- csc %>%
      dplyr::filter(trimws(Cultivar) == trimws(input$search_CSC_Cultivar))
  } 
  if(input$search_CSC_CultivarConfirmed !=''){
    csc <- csc %>%
      dplyr::filter(CultivarConfirmed == input$search_CSC_CultivarConfirmed)
  }
  if(input$search_CSC_VirusIndexed !=''){
    csc <- csc %>%
      dplyr::filter(VirusIndexed == input$search_CSC_VirusIndexed)
  }
  if(!is.null(input$search_CSC_DateOfStarterCulture)){
    csc <- csc %>%
      dplyr::filter(between(lubridate::ymd(DateOfStarterCulture), input$search_CSC_DateOfStarterCulture[1],input$search_CSC_DateOfStarterCulture[2]))
  }
  # else {
  #   dt <- data.frame(t(names(csc)))
  #   colnames(dt) <- names(csc)
  #   dt <- dt[-1,]
  # }
  csc
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
# Hide ExplantIdentity

# observe({
#    if(input$search_CSC_Identify == '' & input$search_CSC_Source =='' & input$search_CSC_Cultivar =='' & 
#       input$search_CSC_CultivarConfirmed =='' & input$search_CSC_VirusIndexed =='' & is.null(input$search_CSC_DateOfStarterCulture)){
#       
#       shinyjs::hide("search_CSC_ResultsTable_Output")
#       shinyjs::hide("search_CSC_Culture_Form1")
#    } 
# })

# Search Results
observeEvent(input$search_CSC_Search,{
  
  if(input$search_CSC_Identify == '' & input$search_CSC_Source =='' & input$search_CSC_Cultivar =='' & 
     input$search_CSC_CultivarConfirmed =='' & input$search_CSC_VirusIndexed =='' & is.null(input$search_CSC_DateOfStarterCulture)){
    shinyalert("Oops!", "Select one of the search criteria", type = "warning")
  } else {
    tryCatch({
      output$search_CSC_ResultsTable <- renderRHandsontable({
        dt <- search_CSC_ResultsTable_Input() 
        # dt <- data.frame(t(names(CSC)))
        # colnames(dt) <- names(CSC)
        #dt <- dt[-1,]
        rhandsontable(dt, selectCallback = T, readOnly = T, rowHeaders=F) %>%
          hot_table(stretchH = "all")
      })
    })
  }
})


observeEvent(input$search_CSC_ResultsTable_select$select$r,{
  df <- tbl(pool, "tblCulturesCSC") %>% collect()
  dv <- search_CSC_ResultsTable_Input()
  r <- input$search_CSC_ResultsTable_select$select$r
  c <- dv[r,'CSCIdentity']
  if(length(c)>0){
    dt = df[trimws(df$CSCIdentity) == trimws(c$CSCIdentity), ]
    output$search_CSC_IdentiyResultsTable <- renderRHandsontable({
      rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
        hot_table(stretchH = "all", columnSorting = TRUE)
    })
  }
  ## update Explant Identity
  # updateSelectInput(session, "search_CSC_Identify", "Identify", choices = c('', CSC$ExplantIdentity), selected = c$ExplantIdentity)
})


observeEvent(input$search_CSC_ClearForm,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset("search_CSC_Form")
      
      # Hide ExplantIdentity
      shinyjs::hide("search_CSC_ResultsTable_Output")
      shinyjs::hide("search_CSC_Culture_Form1")
    },
    text = "Do you really want to clear the form?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
  # input$search_CSC_ResultsTable_select$select$r <- NULL
  
})

observeEvent(input$search_CSC_Culture,{
  if(input$search_CSC_Identify !=''){
    id <- input$search_CSC_Identify
    
    output$search_CSC_Culture_Output <- renderUI({
      csc_cultures <- tbl(pool, "tblCulturesCSC") %>% collect()
      csc_val$Data <- csc_data()
      csc <- csc_val$Data
      
      div(
        panel_div(class_type = "default",
                  content = tags$div(
                    
                    column(3, 
                           div(id = "search_CSC_Culture_Form",
                               disabled(textInput("search_CSC_SelectedIdentity","Selected Identity", value = id, width = "100%")),
                               numericInput("search_CSC_NumberOfCultures",labelMandatory("Number of Cultures"), value = NULL, width = "100%"),
                               dateInput("search_CSC_DateOfCulture",labelMandatory("Date of Cultures"), width = "100%"),
                               selectInput("search_CSC_CulturedBy",labelMandatory("Cultured By"), choices = c('', csc_cultures$CulturedBy), width = "100%"),
                               selectInput("search_CSC_MediaForCultures",labelMandatory("Media"), choices = c('', cscedia), width = "100%"),
                               numericInput("search_CSC_LabBookNumberForCultures",labelMandatory("Lab Book Number"), value = NULL, width = "100%"),
                               numericInput("search_CSC_PageNumberForCultures",labelMandatory("Page Number"), value = NULL, width = "100%"),
                               textInput("search_CSC_Comments", "Comments", width = "100%")
                           )
                    ), 
                    column(1, br(), br(),br(),br(), br(),br(), 
                           awesomeCheckboxGroup(inputId = "search_CSC_AdditivesForCultures", 
                                                label = "Additives", choices = c(additives$Additives), selected = NULL, status = "info")),
                    column(3, 
                           conditionalPanel(
                             condition = "input.search_CSC_SaveRecord",
                             awesomeCheckboxGroup(inputId = "search_CSC_SelectTheFields", label = "Select the Fields",
                                                  choices = c(names(CSC)), selected = names(CSC), status = "info"), br(), br(),
                             downloadBttn("search_CSC_ExportToExcel", "Export to Excel", style = "simple", size = "sm", color = "primary")
                           )
                    ),
                    column(12, offset = 6, actionBttn("search_CSC_SaveRecord", "Save Record", style = "fill", size = "sm", color = "primary"))
                  ))
      )
    })
  }else {
    showNotification("Select an Identity", type = "error")
  }
})

observeEvent(input$search_CSC_Delete,{
  dv <- search_CSC_ResultsTable_Input()
  r <- input$search_CSC_ResultsTable_select$select$r
  c <- dv[r,'CSCIdentity']
  id <- c$CSCIdentity
  sql <- "DELETE FROM tblCSC WHERE CSCIdentity = ?id1;"
  query <- sqlInterpolate(pool, sql, id1 = id)
  dbExecute(pool, query)
  
  # Save deleted CSC details
  try(expr = dbWriteTable( conn = pool,   name = "tblDeletedCSC", value = c, overwrite = F, append = T))
})

# Search deleted CSC

output$search_CSC_SearchDeletedCSC_ResultsTable <- renderRHandsontable({
  dt <- tbl(pool, "tblDeletedCSC") %>% collect()
  rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
    hot_table(stretchH = "all", columnSorting = TRUE)
})



# Export to Excel
search_CSC_Culture_Form_Data <- reactive({
  
  req(input$search_CSC_SelectedIdentity)
  req(input$search_CSC_NumberOfCultures)
  req(input$search_CSC_DateOfCulture)
  req(input$search_CSC_CulturedBy)
  req(input$search_CSC_MediaForCultures)
  req(input$search_CSC_LabBookNumberForCultures)
  req(input$search_CSC_PageNumberForCultures)
  
  dt <-data.frame(
    ExplantIdentity = input$search_CSC_SelectedIdentity,
    NumberOfCultures = input$search_CSC_NumberOfCultures,
    DateOfCulture = input$search_CSC_DateOfCulture,
    CulturedBy = input$search_CSC_CulturedBy,
    MediaForCultures = input$search_CSC_MediaForCultures,
    LabBookNumberForCultures = input$search_CSC_LabBookNumberForCultures,
    PageNumberForCultures = input$search_CSC_PageNumberForCultures,
    comments = ifelse(nchar(input$search_CSC_Comments)>0,input$search_CSC_Comments,''), 
    AdditivesForCultures = ifelse(nchar(input$search_CSC_AdditivesForCultures)>0,input$search_CSC_AdditivesForCultures,'')
  )#ifelse(nchar(input$new_CSC_PermitNumber6)>0, 
  dt
})
search_CSC_ExportToExcel_Input <- reactive({
  dt <- search_CSC_Culture_Form_Data() %>%
    data.frame()
  # CSCV$Data <- load_CSC() %>%
  # dplyr::filter(ExplantIdentity == input$new_CSC_ExplantIdentity)
  
  if (length(input$search_CSC_SelectTheFields) > 0){
    dt = dt %>% 
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


# observeEvent(input$search_CSC_SearchDeletedCSC,{
#    # Hide Explant Identity 
#    shinyjs::hide("search_CSC_Culture_Form1")
#    
# })
# 


observeEvent(input$search_CSC_FormToPicture, {
  js$winprint()
})

observeEvent(input$search_CSC_SCP_CSC_ControlForm, {
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})
observeEvent(input$search_CSC_Exit, {
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        js$closeWindow()
        stopApp()
    },
    text = "Do you really want to EXIT the application?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})

## --------------------------------------------------------------------------------------------------------------------------------------------Updating Last Subculture
observe({
  
})
## --------------------------------------------------------------------------------------------------------------------------------------------- Labels Tab

labels_CSC_Table_Input <- reactive({
  req(input$labels_CSC_SelectDate)
  tbl(pool, "tblCulturesCSC") %>% 
    collect() %>%
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




