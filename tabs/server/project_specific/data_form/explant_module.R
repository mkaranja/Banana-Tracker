tab_files <- list.files(path = "tabs/server/project_specific/data_form/explant_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

# -----------------VECTOR INVENTORY MODULE -----------------------------

observeEvent(input$explant_module,{
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Explant Module - ", input$project_selected)),
                        
                        tabsetPanel(id = "Explant_Tabs", type = "pills", 
                                    explant_search,
                                    new_explant,
                                    explant_update_last_subculture,
                                    explant_labels
                        ), easyClose = F, size = "l"
  ))
})


# Update + Generate ID

observeEvent(input$cell_suspension_culture_module,{
  explant_val$Data <- explant_data()
  explant <- explant_val$Data
  updateSelectInput(session, "new_explant_ParentIdentity", "Parent Identity", choices = c("", explant$ExplantIdentity))
})

observeEvent(input$new_explant_GetData,{
  
  if(input$new_explant_ParentIdentity == ""){
    shinyalert("", text = "Select Parent Identity", type = "warning")
  }
  df <- tbl(pool, paste0(input$project_selected,"_tblExplant")) %>% collect() %>%
    dplyr::filter(trimws(ExplantIdentity) == trimws(input$new_explant_ParentIdentity))
  
  updateTextInput(session, "new_explant_IdentityType", "Identity Type", value = df$IdentityType[1])
  updateTextInput(session, "new_explant_Cultivar", "Cultivar", value = df$Cultivar[1])
  # updateTextInput(session, "new_explant_CultivarConfirmed",tags$p("Cultivar Confirmed"), value = NULL)
  updateTextInput(session, "new_explant_Source", "Source", value = df$Source[1])
  # updateTextInput(sesson, "new_explant_VirusIndexed", tags$p("Virus Indexed"), value = NULL)
  
  observeEvent(input$new_explant_GenerateIdentity, {
    id <- paste0(df$IdentityType[1], substr(input$new_explant_Year,3,5), (max(as.integer(substr(trimws(df$explantIdentity),6,20))) + 1))
    updateTextInput(session, "new_explant_Identity", "Identity", value = id)
  })
  
})

# Save New Record
observeEvent(input$new_explant_SaveStarterCulture_Ready_No, {
  req(input$new_explant_ParentIdentity)
  req(input$new_explant_explantInitialCultureDate)
  req(input$new_explant_Media)
  req(input$new_explant_LabBookNumber)
  req(input$new_explant_PageNumber)
  
  explant_val$Data <- explant_data()
  explant <- tbl(pool, paste0(input$project_selected,"_tblExplant")) %>% collect()# explant_val$Data
  
  new_explant <- data.frame(
    ExplantIdentity = input$new_explant_ParentIdentity,
    explantIdentity = input$new_explant_Identity,
    IdentityType = input$new_explant_IdentityType,
    Cultivar = input$new_explant_Cultivar,
    Source = input$new_explant_Source,
    DateOfStarterCulture = input$new_explant_explantInitialCultureDate,
    Media  = input$new_explant_Media,
    Additives = ifelse(input$new_explant_Additives == '','', input$new_explant_Additives),
    LabBookNumber = input$new_explant_LabBookNumber,
    PageNumber = input$new_explant_PageNumber
  )
  
  if((new_explant$explantIdentity %in% explant$explantIdentity)==TRUE){
    shinyalert("Oops!", "explant Identity Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblExplant', new_explant, append = T)
    shinyalert("Success!", "New explant Added", type = "success")
  }
}, ignoreInit = TRUE)

# Validate required fields
observeEvent(input$new_explant_SaveStarterCulture_Ready_No, {
  
  if(input$new_explant_Media==""){
    showNotification("Please select Media", type = "error")
  }
  if(is.null(input$new_explant_LabBookNumber)){
    showNotification("Please add Lab Book Number", type = "error")
  }
  if(is.null(input$new_explant_PageNumber)){
    showNotification("Please add Page Number", type = "error")
  }
  
})
observeEvent(input$new_explant_SaveStarterCulture_Ready_Yes, {
  
  req(input$new_explant_ParentIdentity)
  req(input$new_explant_explantInitialCultureDate)
  req(input$new_explant_Media)
  req(input$new_explant_LabBookNumber)
  req(input$new_explant_PageNumber)
  
  req(input$new_explant_Identity)
  req(input$new_explant_NumberOfCultures)
  req(input$new_explant_DateOfCultures)
  req(input$new_explant_CulturedBy)
  
  explant_cultures <- tbl(pool, paste0(input$project_selected, "_tblCulturesexplant")) %>% collect()
  explant_val$Data <- explant_data()
  explant <- tbl(pool, paste0(input$project_selected, "_tblExplant")) %>% collect()# explant_val$Data
  # Export to Excel FIelds
  updateAwesomeCheckboxGroup(session = session, inputId = "select_explant_fields_to_be_exported_to_excel", label = "", choices = c(names(explant)))
  
  new_explant <- data.frame(
    ExplantIdentity = input$new_explant_ParentIdentity,
    explantIdentity = input$new_explant_Identity,
    IdentityType = input$new_explant_IdentityType,
    Cultivar = input$new_explant_Cultivar,
    Source = input$new_explant_Source,
    DateOfStarterCulture = input$new_explant_explantInitialCultureDate,
    Media  = ifelse(input$new_explant_Media == '','',input$new_explant_Media),
    Additives = ifelse(input$new_explant_Additives == '','', input$new_explant_Additives),
    LabBookNumber = ifelse(is.null(input$new_explant_LabBookNumber),0,input$new_explant_LabBookNumber),
    PageNumber = ifelse(is.null(input$new_explant_PageNumber),0,input$new_explant_PageNumber)
  )
  
  new_explant_cultures <- data.frame(
    explantIdentity = input$new_explant_Identity,
    NumberOfCultures = input$new_explant_NumberOfCultures,
    DateOfCulture = input$new_explant_DateOfCultures,
    CulturedBy = input$new_explant_CulturedBy,
    Comments = ifelse(is.null(input$new_explant_Comments),'',input$new_explant_Comments),
    MediaForCultures  = ifelse(input$new_explant_Media == '','',input$new_explant_Media),
    AdditivesForCultures = ifelse(input$new_explant_Additives == '','', input$new_explant_Additives),
    LabBookNumberForCultures = ifelse(is.null(input$new_explant_LabBookNumber),0,input$new_explant_LabBookNumber),
    PageNumberForCultures = ifelse(is.null(input$new_explant_PageNumber),0,input$new_explant_PageNumber)
  )
  if((new_explant$explantIdentity %in% explant$explantIdentity)==TRUE){
    shinyalert("Oops!", "explant Identity Exists", type = "error")
  }else {
    dbWriteTable(pool, 'tblExplant', new_explant, append = T)
    shinyalert("Success!", "New explant Added", type = "success")
  }
  
  if((new_explant_cultures$explantIdentity %in% explant_cultures$explantIdentity)==F){
    dbWriteTable(pool, 'tblCulturesexplant', new_explant_cultures, append = T)
  }
  
}, ignoreInit = TRUE)

# Validate required fields
observeEvent(input$new_explant_SaveStarterCulture_Ready_Yes, {
  if(input$new_explant_Media==""){
    showNotification("Please select Media", type = "error")
  }
  if(is.null(input$new_explant_LabBookNumber)){
    showNotification("Please add Lab Book Number", type = "error") 
  }
  if(is.null(input$new_explant_PageNumber)){
    showNotification("Please add Page Number", type = "error")
  }
  if(is.null(input$new_explant_Identity)){
    showNotification("Please click on Generate Identity", type = "error")
  }
  if(is.null(input$new_explant_NumberOfCultures)){
    showNotification("Please enter Number of Cultures", type = "error")
  }
  if(input$new_explant_CulturedBy==""){
    showNotification("Please select Cultured By", type = "error")
  }
})
# Export to Excel FIelds
explant_export_to_excel <- reactive({
  new_explant <- data.frame(
    explantIdentity = input$new_explant_Identity,
    IdentityType = input$new_explant_IdentityType,
    Cultivar = input$new_explant_Cultivar,
    Source = input$new_explant_Source,
    DateOfStarterCulture = input$new_explant_explantInitialCultureDate,
    Media  = ifelse(input$new_explant_Media == '','',input$new_explant_Media),
    Additives = ifelse(input$new_explant_Additives == '','', input$new_explant_Additives),
    LabBookNumber = ifelse(is.null(input$new_explant_LabBookNumber),0,input$new_explant_LabBookNumber),
    PageNumber = ifelse(is.null(input$new_explant_PageNumber),0,input$new_explant_PageNumber),
    NumberOfCultures = input$new_explant_NumberOfCultures,
    DateOfCulture = input$new_explant_DateOfCultures,
    CulturedBy = input$new_explant_CulturedBy,
    Comments = ifelse(is.null(input$new_explant_Comments),'',input$new_explant_Comments)
  )
  
  if (length(input$select_explant_fields_to_be_exported_to_excel) > 0){
    dt = new_explant %>% 
      dplyr::select(!!!input$select_explant_fields_to_be_exported_to_excel)
  }
  dt[rep(row.names(dt), input$new_explant_NumberOfCultures),]
})

output$new_explant_SelectFieldToExportToExcel <- downloadHandler(
  filename = function(){paste0(input$new_explant_Identity,"_",input$new_explant_CulturedBy,"_", Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(explant_export_to_excel(), path = file, col_names = T, format_headers = T )
  }
)

# Clear Form

observeEvent(input$new_explant_ClearForm,{
  
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset('new_explant_form')
    },
    text = "Do you really to clear all the fields?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})

## ------------------------------------------------------------------------------------------------------------------------------------- SEARCH explant

output$explant_search_Fields <- renderUI({
  explant <- tbl(pool, paste0(input$project_selected, "_tblExplant")) %>% collect() 
  
  div(
    column(2, selectInput("explant_search_Identify", "Identify", choices = c('', explant$explantIdentity), selected = '')),
    column(2, selectInput("explant_search_Source", "Source", choices = c('', explant$Source), selected = '')),
    column(2, selectInput("explant_search_Cultivar", "Cultivar", choices = c('', explant$Cultivar), selected = '')),
    column(2, selectInput("explant_search_CultivarConfirmed", "Cultivar Confirmed", choices = c("","Yes","No"))),
    column(2, selectInput("explant_search_VirusIndexed", "Virus Indexed", choices = c("","Yes","No"))),
    column(2, numericInput("explant_search_PermitNUmber", "Permit Number", value = NULL))
  )
})

output$explant_search_DateOfStarterCulture_output <- renderUI({
  explant_val$Data <- explant_data()
  explant <- explant_val$Data
  dateRangeInput("explant_search_DateOfStarterCulture", "Date of Starter Culture", start = min(explant$DateOfStarterCulture), end = max(explant$DateOfStarterCulture), 
                 min = min(explant$DateOfStarterCulture), max = max(explant$DateOfStarterCulture), separator = "  TO  ")
})
explant_search_ResultsTable_Input <- reactive({
  #explant_val$Data <- explant_data()
  explant <- tbl(pool, paste0(input$project_selected, "_tblExplant")) %>% collect()
  
  if(input$explant_search_Identify !=''){
    explant <- explant %>%
      dplyr::filter(trimws(explantIdentity) == trimws(input$explant_search_Identify))
  }
  if(input$explant_search_Source!=''){
    explant <- explant %>%
      dplyr::filter(trimws(Source) == trimws(input$explant_search_Source))
  }
  if(input$explant_search_Cultivar !=''){
    explant <- explant %>%
      dplyr::filter(trimws(Cultivar) == trimws(input$explant_search_Cultivar))
  } 
  if(input$explant_search_CultivarConfirmed !=''){
    explant <- explant %>%
      dplyr::filter(CultivarConfirmed == input$explant_search_CultivarConfirmed)
  }
  if(input$explant_search_VirusIndexed !=''){
    explant <- explant %>%
      dplyr::filter(VirusIndexed == input$explant_search_VirusIndexed)
  }
  if(!is.null(input$explant_search_DateOfStarterCulture)){
    explant <- explant %>%
      dplyr::filter(between(lubridate::ymd(DateOfStarterCulture), input$explant_search_DateOfStarterCulture[1],input$explant_search_DateOfStarterCulture[2]))
  }
  # else {
  #   dt <- data.frame(t(names(explant)))
  #   colnames(dt) <- names(explant)
  #   dt <- dt[-1,]
  # }
  explant
})


# Table results
output$explant_search_ResultsTable_Output <- renderUI({
  # Search Results Table
  if(input$explant_Search_Deleted_Explant == FALSE){
    rHandsontableOutput("explant_search_ResultsTable")
  } else {
    # Search Deleted explant 
    rHandsontableOutput("explant_Search_Deleted_Explant_ResultsTable")
  }
})
# Hide ExplantIdentity

# observe({
#    if(input$explant_search_Identify == '' & input$explant_search_Source =='' & input$explant_search_Cultivar =='' & 
#       input$explant_search_CultivarConfirmed =='' & input$explant_search_VirusIndexed =='' & is.null(input$explant_search_DateOfStarterCulture)){
#       
#       shinyjs::hide("explant_search_ResultsTable_Output")
#       shinyjs::hide("explant_search_Culture_Form1")
#    } 
# })

# Search Results
observeEvent(input$explant_search_Search,{
  
  if(input$explant_search_Identify == '' & input$explant_search_Source =='' & input$explant_search_Cultivar =='' & 
     input$explant_search_CultivarConfirmed =='' & input$explant_search_VirusIndexed =='' & is.null(input$explant_search_DateOfStarterCulture)){
    shinyalert("Oops!", "Select one of the search criteria", type = "warning")
  } else {
    tryCatch({
      output$explant_search_ResultsTable <- renderRHandsontable({
        dt <- explant_search_ResultsTable_Input() 
        # dt <- data.frame(t(names(explant)))
        # colnames(dt) <- names(explant)
        #dt <- dt[-1,]
        rhandsontable(dt, selectCallback = T, readOnly = T, rowHeaders=F) %>%
          hot_table(stretchH = "all")
      })
    })
  }
})


observeEvent(input$explant_search_ResultsTable_select$select$r,{
  df <- tbl(pool, paste0(input$project_selected, "_tblCulturesexplant")) %>% collect()
  dv <- explant_search_ResultsTable_Input()
  r <- input$explant_search_ResultsTable_select$select$r
  c <- dv[r,'explantIdentity']
  if(length(c)>0){
    dt = df[trimws(df$explantIdentity) == trimws(c$explantIdentity), ]
    output$explant_search_IdentiyResultsTable <- renderRHandsontable({
      rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
        hot_table(stretchH = "all", columnSorting = TRUE)
    })
  }
  ## update Explant Identity
  # updateSelectInput(session, "explant_search_Identify", "Identify", choices = c('', explant$ExplantIdentity), selected = c$ExplantIdentity)
})


observeEvent(input$explant_search_ClearForm,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset("explant_search_Form")
      
      # Hide ExplantIdentity
      shinyjs::hide("explant_search_ResultsTable_Output")
      shinyjs::hide("explant_search_Culture_Form1")
    },
    text = "Do you really want to clear the form?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
  # input$explant_search_ResultsTable_select$select$r <- NULL
  
})

observeEvent(input$explant_search_Culture,{
  if(input$explant_search_Identify !=''){
    id <- input$explant_search_Identify
    
    output$explant_search_Culture_Output <- renderUI({
      explant_cultures <- tbl(pool, paste0(input$project_selected, "_tblCulturesexplant")) %>% collect()
      explant_val$Data <- explant_data()
      explant <- explant_val$Data
      
      div(
        panel_div(class_type = "default",
                  content = tags$div(
                    
                    column(3, 
                           div(id = "explant_search_Culture_Form",
                               disabled(textInput("explant_search_SelectedIdentity","Selected Identity", value = id, width = "100%")),
                               numericInput("explant_search_NumberOfCultures",labelMandatory("Number of Cultures"), value = NULL, width = "100%"),
                               dateInput("explant_search_DateOfCulture",labelMandatory("Date of Cultures"), width = "100%"),
                               selectInput("explant_search_CulturedBy",labelMandatory("Cultured By"), choices = c('', explant_cultures$CulturedBy), width = "100%"),
                               selectInput("explant_search_MediaForCultures",labelMandatory("Media"), choices = c('', explantedia), width = "100%"),
                               numericInput("explant_search_LabBookNumberForCultures",labelMandatory("Lab Book Number"), value = NULL, width = "100%"),
                               numericInput("explant_search_PageNumberForCultures",labelMandatory("Page Number"), value = NULL, width = "100%"),
                               textInput("explant_search_Comments", "Comments", width = "100%")
                           )
                    ), 
                    column(1, br(), br(),br(),br(), br(),br(), 
                           awesomeCheckboxGroup(inputId = "explant_search_AdditivesForCultures", 
                                                label = "Additives", choices = c(additives$Additives), selected = NULL, status = "info")),
                    column(3, 
                           conditionalPanel(
                             condition = "input.explant_search_SaveRecord",
                             awesomeCheckboxGroup(inputId = "explant_search_SelectTheFields", label = "Select the Fields",
                                                  choices = c(names(explant)), selected = names(explant), status = "info"), br(), br(),
                             downloadBttn("explant_search_ExportToExcel", "Export to Excel", style = "simple", size = "sm", color = "primary")
                           )
                    ),
                    column(12, offset = 6, actionBttn("explant_search_SaveRecord", "Save Record", style = "fill", size = "sm", color = "primary"))
                  ))
      )
    })
  }else {
    showNotification("Select an Identity", type = "error")
  }
})

observeEvent(input$explant_search_Delete,{
  dv <- explant_search_ResultsTable_Input()
  r <- input$explant_search_ResultsTable_select$select$r
  c <- dv[r,'explantIdentity']
  id <- c$explantIdentity
  sql <- "DELETE FROM tblExplant WHERE explantIdentity = ?id1;"
  query <- sqlInterpolate(pool, sql, id1 = id)
  dbExecute(pool, query)
  
  # Save deleted explant details
  try(expr = dbWriteTable( conn = pool,   name = paste0(input$project_selected, "_tblDeletedExplant"), value = c, overwrite = F, append = T))
})

# Search deleted explant

output$explant_Search_Deleted_Explant_ResultsTable <- renderRHandsontable({
  dt <- tbl(pool, paste0(input$project_selected, "_tblDeletedExplant")) %>% collect()
  rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
    hot_table(stretchH = "all", columnSorting = TRUE)
})



# Export to Excel
explant_search_Culture_Form_Data <- reactive({
  
  req(input$explant_search_SelectedIdentity)
  req(input$explant_search_NumberOfCultures)
  req(input$explant_search_DateOfCulture)
  req(input$explant_search_CulturedBy)
  req(input$explant_search_MediaForCultures)
  req(input$explant_search_LabBookNumberForCultures)
  req(input$explant_search_PageNumberForCultures)
  
  dt <-data.frame(
    ExplantIdentity = input$explant_search_SelectedIdentity,
    NumberOfCultures = input$explant_search_NumberOfCultures,
    DateOfCulture = input$explant_search_DateOfCulture,
    CulturedBy = input$explant_search_CulturedBy,
    MediaForCultures = input$explant_search_MediaForCultures,
    LabBookNumberForCultures = input$explant_search_LabBookNumberForCultures,
    PageNumberForCultures = input$explant_search_PageNumberForCultures,
    comments = ifelse(nchar(input$explant_search_Comments)>0,input$explant_search_Comments,''), 
    AdditivesForCultures = ifelse(nchar(input$explant_search_AdditivesForCultures)>0,input$explant_search_AdditivesForCultures,'')
  )#ifelse(nchar(input$new_explant_PermitNumber6)>0, 
  dt
})
explant_search_ExportToExcel_Input <- reactive({
  dt <- explant_search_Culture_Form_Data() %>%
    data.frame()
  # explantV$Data <- load_explant() %>%
  # dplyr::filter(ExplantIdentity == input$new_explant_ExplantIdentity)
  
  if (length(input$explant_search_SelectTheFields) > 0){
    dt = dt %>% 
      dplyr::select(!!!input$explant_search_SelectTheFields)
  }
  
  dt[rep(row.names(dt), input$explant_search_NumberOfCultures),]
  
})
output$explant_search_ExportToExcel <- downloadHandler(
  filename = function(){paste(input$explant_search_SelectedIdentity, Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(explant_search_ExportToExcel_Input(), path = file, col_names = T, format_headers = T )
  }
)


# observeEvent(input$explant_Search_Deleted_Explant,{
#    # Hide Explant Identity 
#    shinyjs::hide("explant_search_Culture_Form1")
#    
# })
# 


observeEvent(input$explant_search_FormToPicture, {
  js$winprint()
})

observeEvent(input$explant_search_SCP_explant_ControlForm, {
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})


## Exit
observeEvent(input$explant_search_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "explant_search_Exit_Confirm",
    type = "warning",
    title = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#DD6B55", "#4BB543")
  )
})

observeEvent(input$explant_search_Exit_Confirm, {
  
  if(input$explant_search_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
  
}, ignoreInit = TRUE)

## UPDATING LAST Subculture
observeEvent(input$plant_tissue_culture_module,{
  tb <- paste0(input$project_selected, "_tblCulturesExplant")
  updateSelectInput(session, "explant_updating_last_subculture_ExplantID", "", choices = c("",loadData(tb)$ExplantID))
})

observeEvent(input$explant_updating_last_subculture_LoadData,{
  tb <- paste0(input$project_selected, "_tblCulturesExplant")
  dt <- loadData(tb) %>%
    dplyr::filter(trimws(ExplantID) == trimws(input$updating_last_subculture_ExplantID))
  
  updateNumericInput(session, "explant_updating_last_subculture_NumberOfCultures", "", value = dt$NumberOfCultures[1])
  updateDateInput(session, "explant_updating_last_subculture_DateOfCulture", "", value = dt$DateOfCulture[1])
  updateSelectInput(session, 'explant_updating_last_subculture_CulturedBy', "", choices = c(dt$CulturedBy), selected = dt$CulturedBy[1])
  updateSelectInput(session, "explant_updating_last_subculture_Media", "", choices = c(dt$Media), selected = dt$Media[1])
  updateTextInput(session, "explant_updating_last_subculture_Additives", "", value = dt$Additives[1])
  updateNumericInput(session, "explant_updating_last_subculture_LabBookNumber", "", value = dt$LabBookNumber[1])
  updateNumericInput(session, "explant_updating_last_subculture_PageNumber", "", value = dt$PageNumber[1])
  updateTextAreaInput(session, "explant_updating_last_subculture_Comments", "", value = dt$Comments[1])
})

# Update
observeEvent(input$explant_updating_last_subculture_Update,{
  id <- trimws(input$updating_last_subculture_ExplantID)
  NumberOfCultures = input$explant_updating_last_subculture_NumberOfCultures
  Comments = input$explant_updating_last_subculture_Comments
  
  tb <- paste0(input$project_selected, "_tblCulturesExplant")
  sql <- paste("UPDATE ", tb, "SET NumberOfCultures = ?val1, Comments = ?val2 WHERE ExplantID = ?id1;")
  query <- sqlInterpolate(pool, sql, val1 = NumberOfCultures, val2 = Comments, id1 = id)
  dbExecute(pool, query)
  
  shinyalert("Success!", "Record updated", type = "success")
  reset("explant_updating_last_subculture_Form")
})
## --------------------------------------------------------------------------------------------------------------------------------------------- Labels Tab

labels_explant_Table_Input <- reactive({
  req(input$labels_explant_SelectDate)
  tbl(pool, paste0(input$project_selected, "_tblCulturesexplant")) %>%
    collect () %>%
    dplyr::filter(lubridate::ymd(DateOfCulture) == input$labels_explant_SelectDate)
})

observeEvent(input$labels_explant_LOadData,{
  output$labels_explant_Table <- renderRHandsontable({
    dt <- labels_explant_Table_Input()
    
    rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
      hot_table(stretchH = "all", columnSorting = TRUE)
  })
})

# Export to Excel

output$labels_explant_ExportToExcel <- downloadHandler(
  filename = function(){paste("Labels-", Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(labels_explant_Table_Input(), path = file, col_names = T, format_headers = T )
  }
)


observeEvent(input$labels_explant_Clear,{
  shinyjs::hide("labels_explant_Table")
})




