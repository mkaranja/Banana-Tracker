
tab_files <- list.files(path = "tabs/server/project_specific/data_form/plant_tissue_culture_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))


observeEvent(input$plant_tissue_culture_module, {
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Plant Tissue Culture Module - ",input$project_selected)),
                        
                        tabsetPanel(type = "pills",id = "PTC",
                                    search_PTC,
                                    updating_last_subculture#,
                                    #new_PTC
                                    #PTC_deployment,
                                    #TMPL_UMPL
                        ),
                        easyClose = F, size = "l"
  ))
})


## SEARCH PLANT TISSUE CULTURE -----------------------------------------------------------

observeEvent(input$plant_tissue_culture_module, {
    updateSelectInput(session, "searchPTC_ID", "Identity", choices = c('', unique(PTC()$PTCIdentity)))
    updateSelectInput(session, "searchPTC_VectorID", "Vector ID", choices = c('', unique(PTC()$VectorID1)))
    updateDateRangeInput(session, "searchPTC_DateOfStarterCulture", "Date of Starter Culture",
                         min=min(PTC()$DateOfStarterCulture), max=max(PTC()$DateOfStarterCulture),
                         start=min(PTC()$DateOfStarterCulture), end=max(PTC()$DateOfStarterCulture))
})

# Clear
observeEvent(input$searchPTC_ClearForm,{
  confirmSweetAlert(
    session = session,
    inputId = "searchPTC_ClearForm_Confirm",
    type = "",
    title = "",
    text = "Do you really want to clear the fields?",
    btn_labels = c("Cancel", "Yes, Clear!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})
observeEvent(input$searchPTC_ClearForm_Confirm, {
  if(input$searchPTC_ClearForm_Confirm == TRUE){
    reset("searchPTC_form")
    shinyjs::hide("searchPTC_Table")
    shinyjs::hide("searchPTCDeleted_Table")
    shinyjs::hide("searchPTC_Culture_Form")
  }
}, ignoreInit = TRUE)

# Search 
observeEvent(input$searchPTC_LoadData,{
  dt <- vector_inventory() %>%
    dplyr::filter(trimws(VectorID) == trimws(input$searchPTC_VectorID))
  updateTextInput(session, "searchPTC_PlantSelection", "Plant Selection", value = as.character(dt$PlantSelection[1])) # update plantSelection
})

# Search Table
output$searchPTC_Table_Output <- renderUI({
  if(input$search_deletedPTC==TRUE){
    rHandsontableOutput("searchPTCDeleted_Table")
  }else {
    rHandsontableOutput("searchPTC_Table")
  }
})

# PTC table
searchPTC_input <- reactive({
  dt <- PTC()
  df <- dt %>%
     dplyr::filter(between(lubridate::ymd(DateOfStarterCulture), input$searchPTC_DateOfStarterCulture[1], input$searchPTC_DateOfStarterCulture[2]))
  
  if(input$searchPTC_ID !=""){
    df <- df[trimws(df$PTCIdentity)==trimws(input$searchPTC_ID),]
  } 
  if(input$searchPTC_VectorID !=""){
    df <- df[trimws(df$VectorID1)==trimws(input$searchPTC_VectorID),]
  } 
  df
})
observeEvent(input$searchPTC_Search,{
  if(input$search_deletedPTC==FALSE){
      output$searchPTC_Table <- renderRHandsontable({
        rhandsontable(searchPTC_input(), selectCallback = T, readOnly = T, rowHeaders=F) %>%
          hot_table(stretchH = "all")
      })
  }
})

# deleted PTC table
searchPTCDeleted_input <- reactive({
  dt <- deletedPTC()
  df <- dt %>%
    dplyr::filter(between(lubridate::ymd(DateOfStarterCulture), input$searchPTC_DateOfStarterCulture[1], input$searchPTC_DateOfStarterCulture[2]))
  
  if(input$searchPTC_ID !=""){
    df <- dt[trimws(dt$PTCIdentity)==trimws(input$searchPTC_ID),]
  }
  if(input$searchPTC_VectorID !=""){
    df <- dt[trimws(dt$VectorID1)==trimws(input$searchPTC_VectorID),]
  }
})
observeEvent(input$searchPTC_Search,{
  if(input$search_deletedPTC==TRUE){
    if(!is.null(searchPTCDeleted_input())){
      output$searchPTCDeleted_Table <- renderRHandsontable({
        rhandsontable(searchPTCDeleted_input(), selectCallback = T, readOnly = T, rowHeaders=F) %>%
          hot_table(stretchH = "all")
      })
    }else{
      showNotification("Not data to display.", type = "warning")
    }
  }
})

output$test10 <- renderPrint({
  length(input$searchPTC_Table_select$select)
})
# culture

observeEvent(input$searchPTC_Table_select$select,{
  r <- isolate(input$searchPTC_Table_select$select$r)
  c <- searchPTC_input()[r,]
  
  output$searchPTC_Culture_Table <- renderRHandsontable({
    rhandsontable(c, selectCallback = T, readOnly = T, rowHeaders=F) %>%
      hot_table(stretchH = "all")
  })
})
observeEvent(input$searchPTC_Culture,{
  
  r <- isolate(input$searchPTC_Table_select$select$r)
  c <- searchPTC_input()[r,]
  id <- c$PTCIdentity
  media <- tbl(pool, "tblMedia") %>% collect()
  culturedby <- tbl(pool, "tblCulturedBy") %>% collect()
  
  if(length(r)>0){
  output$searchPTC_Culture_Output <- renderUI({
    div(id="searchPTC_Culture_Form",
      column(7,
         panel_div(class_type = "default",
                   content = tags$div(
                     column(6,
                          disabled(textInput("searchPTC_SelectedIdentity","Identity", value = id, width = "100%")),
                          numericInput("searchPTC_NumberOfCultures",labelMandatory("Number of Cultures"), min = 0, value = NULL, width = "100%"),
                          dateInput("searchPTC_DateOfCulture",labelMandatory("Date of Cultures"), width = "100%"),
                          selectInput("searchPTC_CulturedBy",labelMandatory("Cultured By"), choices = c('', loadData("tblCulturedBy")$CulturedBy), width = "100%"),
                          selectInput("searchPTC_MediaForCultures",labelMandatory("Media"), choices = c('', loadData("tblMedia")$Media), width = "100%"),
                          numericInput("searchPTC_LabBookNumberForCultures",labelMandatory("Lab Book Number"), min = 0, value = NULL, width = "100%"),
                          numericInput("searchPTC_PageNumberForCultures",labelMandatory("Page Number"), min = 0, value = NULL, width = "100%"),
                          textInput("searchPTC_Comments", "Comments", width = "100%")
                     ),
                     column(6, br(), br(),br(),br(), br(),br(),
                        panel_div(class_type = "default",
                            content = tags$div(
                                tags$b("Additives"),
                                awesomeCheckboxGroup(inputId = "searchPTC_AdditivesForCultures", label = "", choices = c(loadData("tblAdditives")$Additives), selected = NULL, status = "info")
                              )), br(), br(),br(),br(), br(),br(), br(),br(),br(), br(),
                        actionBttn("searchPTC_Culture_Save","Save Culture", style = "fill", size = "xs", color = "primary")
                         )
                     )
       )
    ))
  })
  } else {
    shinyalert::shinyalert("", "Select at least one Identity in the table", type = "warning")
  }
})

searchPTC_Culture_MandatoryFields <- 
  c("searchPTC_SelectedIdentity","searchPTC_NumberOfCultures","searchPTC_CulturedBy", 
    "searchPTC_MediaForCultures", "searchPTC_LabBookNumberForCultures", "searchPTC_PageNumberForCultures")
observe({
  # check if all mandatory fields have a value
  mandatoryFilled <-
    vapply(searchPTC_Culture_MandatoryFields,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != ""
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  # enable/disable the submit button
  shinyjs::toggleState(id = "searchPTC_Culture_Save", "Save Culture", condition = mandatoryFilled)
})
# Save
observeEvent(input$searchPTC_Culture_Save,{
  tb <- paste0(input$project_selected, "_tblCulturesPlantTissueCulture")
  dt <- data.frame(
    PTCIdentity = input$searchPTC_SelectedIdentity,
    NumberOfCultures = input$searchPTC_NumberOfCultures,
    DateOfCulture = input$searchPTC_DateOfCulture,
    CulturedBy = input$searchPTC_CulturedBy,
    Comments = ifelse(nchar(input$searchPTC_Comments)>0,input$searchPTC_Comments,''),
    MediaForCultures  = input$searchPTC_MediaForCultures,
    AdditivesForCultures = ifelse(!is.null(input$searchPTC_AdditivesForCultures),input$searchPTC_AdditivesForCultures,''),
    LabBookNumberForCultures = input$searchPTC_LabBookNumberForCultures,
    PageNumberForCultures = input$searchPTC_PageNumberForCultures
  )
  try(expr = dbWriteTable(conn = pool,   name = tb, value = dt, overwrite = F, append = T))
  #saveData(dt, tb)
  shinyalert("Success!", "Record Saved", type = "success")
  
  shinyjs::reset("searchPTC_form")
  shinyjs::hide("searchPTC_Culture_Form")
  shinyjs::hide("searchPTC_Table")
  shinyjs::hide("searchPTCDeleted_Table")
  shinyjs::hide("searchPTC_Culture_Table")
  
})

# delete selected
observeEvent(input$searchPTC_DeleteSelected,{
  confirmSweetAlert(
    session = session,
    inputId = "searchPTC_DeleteSelected_Confirm",
    type = "warning",
    title = "",
    text = "Do you really want to DELETE the record?",
    btn_labels = c("Cancel", "Yes, Delete!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})

observeEvent(input$searchPTC_DeleteSelected_Confirm,{
  if(input$searchPTC_DeleteSelected_Confirm==TRUE){
      r <- input$searchPTC_Table_select$select$r
      c <- searchPTC_input()[r,]
      id <- c$PTCIdentity
      
      if(input$userName %in% isAdmin$UserName){
        if(length(c>0)){
          tb <- paste0(input$project_selected,"_tblPlantTissueCulture")
          tb2 <- paste0(input$project_selected, "_tblDeletedPlantTissueCulture")
          sql <- paste("DELETE FROM",tb,"WHERE PTCIdentity = ?id;")
          query <- sqlInterpolate(pool, sql, id = id)
          dbExecute(pool, query)# delete
          dbWriteTable(conn = pool,   name = "tblDeletedPlantTissueCulture", value = c, overwrite = F, append = T)# save deleted record
          reset("searchPTC_form")
          shinyjs::hide("searchPTC_Table")
          shinyjs::hide("searchPTCDeleted_Table")
          shinyjs::hide("searchPTC_Culture_Form")
        }
      }else {
        shinyalert("Oops!", "You don't have permissions to delete this record.", type = "error")
      }
  }
})
        
## UPDATING LAST SUBCULTURE
observeEvent(input$plant_tissue_culture_module,{
  updateSelectInput(session, "updating_last_subculture_PTCIdentity", "Plant Tissue Culture Identity", choices = c("",culturesPTC()$PTCIdentity))
})

observeEvent(input$updating_last_subculture_PTCLoadData,{
  dt <- culturesPTC() %>%
    dplyr::filter(trimws(PTCIdentity) == trimws(input$updating_last_subculture_PTCIdentity))
  
  updateNumericInput(session, "updating_last_subculture_PTCNumberOfCultures", "Number of Cultures", value = dt$NumberOfCultures[1])
  updateDateInput(session, "updating_last_subculture_PTCDateOfCulture", "Date of Culture", value = dt$DateOfCulture[1])
  updateSelectInput(session, 'updating_last_subculture_PTCCulturedBy', "Cultured By", choices = c(dt$CulturedBy), selected = dt$CulturedBy[1])
  updateSelectInput(session, "updating_last_subculture_PTCMedia", "Media", choices = c(dt$Media), selected = dt$Media[1])
  updateTextInput(session, "updating_last_subculture_PTCAdditives", "Additives", value = dt$Additives[1])
  updateNumericInput(session, "updating_last_subculture_PTCLabBookNumber", "Lab Book Number", value = dt$LabBookNumber[1])
  updateNumericInput(session, "updating_last_subculture_PTCPageNumber", "Page Number", value = dt$PageNumber[1])
  updateTextAreaInput(session, "updating_last_subculture_PTCComments", "Comments", value = dt$Comments[1])
})

# Update
observeEvent(input$updating_last_subculture_PTCUpdate,{
  id <- trimws(input$updating_last_subculture_PTCIdentity)
  NumberOfCultures = input$updating_last_subculture_PTCNumberOfCultures
  Comments = input$updating_last_subculture_PTCComments
  
  tb <- paste0(input$project_selected, "_tblCulturesPlantTissueCulture")
  sql <- paste("UPDATE ", tb, "SET NumberOfCultures = ?val1, Comments = ?val2 WHERE PTCIdentity = ?id1;")
  query <- sqlInterpolate(pool, sql, val1 = NumberOfCultures, val2 = Comments, id1 = id)
  dbExecute(pool, query)
  
  shinyalert("Success!", "Record updated", type = "success")
  reset("updating_last_subculture_PTCForm")
})

# Clear
observeEvent(input$updating_last_subculture_PTCClear,{
  confirmSweetAlert(
    session = session,
    inputId = "updating_last_subculture_PTCClear_Confirm",
    type = "",
    title = "",
    text = "Do you really want to clear the fields?",
    btn_labels = c("Cancel", "Yes, Clear!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})
observeEvent(input$updating_last_subculture_PTCClear_Confirm, {
  if(input$updating_last_subculture_PTCClear_Confirm == TRUE){
    reset("updating_last_subculture_PTCForm")
  }
}, ignoreInit = TRUE)


## NEW PLANT TISSUE CULTURE 

observeEvent(input$plant_tissue_culture_module, {
  updateSelectInput(session, "new_PTC_TPCID", "TPC ID", choices = c('', unique(PTC()$TPCIdentity)))
  updateSelectInput(session, "new_PTC_UPCID", "UPC ID", choices = c('', unique(PTC()$UPCIdentity)))
  updateSelectInput(session, "new_PTC_ExplantID", "Explant ID", choices = c('', unique(PTC()$ExplantID)))
})


new_PTC_Input <- reactive({
  dt <- PTC()
  if(input$new_PTC_TPCID != ""){
    dt <- dt %>%
      dplyr::filter(TPCIdentity == input$new_PTC_TPCID)
  }
  if(input$new_PTC_UPCID != ""){
    dt <- dt %>%
      dplyr::filter(TPCIdentity == input$new_PTC_UPCID)
  }
  if(input$new_PTC_ExplantID != ""){
    dt <- dt %>%
      dplyr::filter(TPCIdentity == input$new_PTC_ExplantID)
  }
  dt
})
## update fields on data loading

observeEvent(input$new_PTC_ID_LoadData,{
  #updateTextInput(session, "new_PTC_VectorID", "VectorID", value = new_PTC_Input()$VectorID1)
  updateTextInput(session, "new_PTC_VectorID1_PromoterGene","Vector ID 1 Promoter-Gene", value = new_PTC_Input()$VectorID1) 
  updateTextInput(session, "new_PTC_VectorID2_PromoterGene","Vector ID 2 Promoter-Gene", value = new_PTC_Input()$VectorID2)
  #updateTextInput(session, "new_PTC_VirusIndexed", "Virus Indexed")
  
  updateTextInput(session, "new_PTC_PTCIdentity","PTC Identity", value = new_PTC_Input()$PTCIdentity)
  updateTextInput(session, "new_PTC_IdentityType","Identity Type", value = new_PTC_Input()$IdentityType)
  
  updateSelectInput(session, "new_PTC_Media", "Media", choices = c(new_PTC_Input()$Media))
  updatePrettyCheckboxGroup(session, "new_PTC_Additives", "Additives", choices = new_PTC_Input()$Additives, animation = "jelly")
  
})

# update fields on New input
observeEvent(input$new_PTC_source,{
  tb <- input$project_selected
  cultivar <- tbl(pool,"tblCultivar") %>% collect()
  source <- tbl(pool,"tblSource") %>% collect()
  # virusIndexedBy <- tbl(pool, paste0(tb, "_tblVirusIndexedBy")) %>% collect()
  permitType <- tbl(pool, "tblPermitType") %>% collect()
  
  if(input$new_PTC_source == "New"){
    updateSelectInput(session, "new_PTC_Cultivar", "Cultivar", choices = c('', cultivar$Cultivar))
    updateSelectInput(session, "new_PTC_Source", "Source", choices = c('', source$Source))
    updateSelectInput(session, "new_PTC_VirusIndexedBy", "Virus Indexed By", choices = input$username)
    updateSelectInput(session, "new_PTC_PermitType1A", "", choices = permitType$PermitType)
    updateSelectInput(session, "new_PTC_PermitType1B", "", choices = permitType$PermitType)
    updateSelectInput(session, "new_PTC_PermitType2A", "", choices = permitType$PermitType)
    updateSelectInput(session, "new_PTC_PermitType2B", "", choices = permitType$PermitType)
    updateSelectInput(session, "new_PTC_PermitType3A", "", choices = permitType$PermitType)
    updateSelectInput(session, "new_PTC_PermitType3B", "", choices = permitType$PermitType)
  }
})

## Load Data on PTC Identity Input

observeEvent(input$new_PTC_LoadData,{
  
})


## Update PTC Identity

observeEvent(input$new_PTC_Update,{
  
})
## Save record

observeEvent(input$new_PTC_SaveStarterCultureAndSubCulture,{
  
})

## Save Culture

#observeEvent(input$,{
# tb <- IBBTV_tblCulturesPlantTissueCulture
#})
## Clear

observeEvent(input$new_PTC_ClearTheForm,{
  reset("new_PTC_form")
})


# DEPLOYMENT

observeEvent(input$plant_tissue_culture_module, {
  updateSelectInput(session, "PTC_deployment_TPLIdentity","TPL Identity", choices = c('', unique(PTC()$TPCIdentity)))
  updateSelectInput(session, "new_PTC_UPCID", "UPC ID", choices = c('', unique(PTC()$UPCIdentity)))
  updateSelectInput(session, "new_PTC_ExplantID", "Explant ID", choices = c('', unique(PTC()$ExplantID)))
})

observeEvent(input$PTC_deployment_TPLGetVectorData,{
  
})