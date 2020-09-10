 tab_files <- list.files(path = "tabs/server/project_specific/data_form/vector_inventory_module", full.names = T, recursive = T)
 suppressMessages(lapply(tab_files, source))

# -----------------VECTOR INVENTORY MODULE -----------------------------

observeEvent(input$vector_inventory_module,{
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Vector Inventory Module - ", input$project_selected)),
                        
                          tabsetPanel(id = "vector_inventory_Tabs", type = "pills", selected = "vector_inventory_1", br(),
                                    vector_inventory1,
                                    vector_inventory2,
                                    vector_inventory3,
                                    vector_inventory4,
                                    vector_inventory5,
                                    vector_inventory_search
                        ), easyClose = F, size = "l"
  ))
})


# # clear all tabs

observeEvent(input$vector_inventory_1_ClearAllTheTabs,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_1_ClearAllTheTabs_Confrim",
    type = "warning",
    title = "",
    text = "Do you really want to clear all tabs?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$vector_inventory_1_ClearAllTheTabs_Confrim, {
  if(input$vector_inventory_1_ClearAllTheTabs_Confrim == TRUE){
    reset("vector_inventory_1_VectorID"); reset("vector_inventory_1_form_a"); reset("vector_inventory_1_form_b")
    reset("vector_inventory_2_form")
    reset("vector_inventory_3_form")
    reset("vector_inventory_4_form")
    reset("vector_inventory_5_form")
    reset("vector_inventory_search_form")
  }
}, ignoreInit = TRUE)




# clonedBy <- reactive({
#   tbl(pool, paste0(input$project_selected, "_tblClonedBy")) %>% collect()
# })

## ----------------------------1. Vector Inventory 1
output$vector_inventory_1_VectorSuffix_output <- renderUI({
  dt <- vector_inventory_values$Data <- vector_inventory()
  disabled(textInput("vector_inventory_1_VectorSuffix","Vector Suffix", value = as.integer(nrow(dt)+1), width = "100%"))
})
# Update VectorID & Suffix
observeEvent(input$vector_inventory_1_VectorCode,{
  dt <- vector_inventory_values$Data <- vector_inventory()
  df <- dt %>%
    dplyr::filter(VectorID == input$vector_inventory_search_SelectedVectorID)
  if(input$vector_inventory_search_LoadDataToView>0 || input$vector_inventory_search_LoadDataToUpdate>0){
    updateTextInput(session, "vector_inventory_1_VectorSuffix", "Vector Suffix", value = df$VectorSuffix)
  }
  #else {
  #   updateTextInput(session, "vector_inventory_1_VectorSuffix", "Vector Suffix", value = as.integer(nrow(dt)+1))
  # }
})

observeEvent(input$vector_inventory_1_VectorCode,{
  id <- paste0(input$vector_inventory_1_VectorPrefix,"-",input$vector_inventory_1_VectorCode, "-",input$vector_inventory_1_VectorSuffix)
  if(input$vector_inventory_1_VectorCode !=''){
    updateTextInput(session, "vector_inventory_1_VectorID", "Vector ID", value = id)
  }
})

output$tt1 <- renderPrint({
  input$vector_inventory_search_LoadDataToUpdate>0
})
# Update SelectInputs
observeEvent(input$vector_inventory_module,{
    BacterialSelection <- tbl(pool, paste0(input$project_selected, "_tblBacterialSelection")) %>% collect()
    PlantSelection <- tbl(pool, paste0(input$project_selected, "_tblPlantSelection")) %>% collect()
    backbone <- tbl(pool, paste0(input$project_selected, "_tblBackbone")) %>% collect()
    ClonedBy <- vector_inventory_values$Data <- vector_inventory()

    updateTextInput(session, "vector_inventory_1_VectorPrefix", "Vector Prefix", value = paste0("p",input$project_selected, ""))
    updateSelectizeInput(session, "vector_inventory_1_BacterialSelection","Bacterial Selection", choices = c("", BacterialSelection$BacterialSelection))
    updateSelectizeInput(session, "vector_inventory_1_PlantSelection","Plant Selection", choices = c("", PlantSelection$PlantSelection))
    updateSelectizeInput(session, "vector_inventory_1_Backbone","", choices = c("", backbone$Backbone))
    updateSelectizeInput(session, "vector_inventory_1_ClonedBy","", choices = c("", ClonedBy$ClonedBy))
})

# clear current tab

observeEvent(input$vector_inventory_1_Clear,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset("vector_inventory_1_VectorID"); reset("vector_inventory_1_form_a"); reset("vector_inventory_1_form_b")
    },
    text = "Do you really want to clear the fields?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})

# Back control form
observeEvent(input$vector_inventory_1_ControlForm,{
  removeModal()
})

## Exit
observeEvent(input$vector_inventory_1_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_1_Exit_Confirm",
    type = "warning",
    title = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$vector_inventory_1_Exit_Confirm, {
  if(input$vector_inventory_1_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)

## ---------------------------------------------------------------------------- 2. Vector Inventory 2

# inputs

observeEvent(input$vector_inventory_module,{

      promoter <- tbl(pool, paste0(input$project_selected, "_tblPromoter")) %>% collect()
      gene <- tbl(pool, paste0(input$project_selected, "_tblGene")) %>% collect()
      terminator <- tbl(pool, paste0(input$project_selected, "_tblTerminator")) %>% collect()
      feature <- tbl(pool, paste0(input$project_selected, "_tblNewFeature")) %>% collect()

      updateSelectInput(session, "vector_inventory_2_Cassette1Promoter","", choices = c('', promoter$Promoter))
      updateSelectInput(session, "vector_inventory_2_Cassette1Gene","", choices =  c('', gene$Gene))
      updateSelectInput(session, "vector_inventory_2_Cassette1Terminator","", choices = c('', terminator$Terminator))
      updateSelectInput(session, "vector_inventory_2_Cassette1Feature1","", choices = c('', feature$NewFeature))
      updateSelectInput(session, "vector_inventory_2_Cassette1Feature2","", choices = c('', feature$NewFeature))
      updateSelectInput(session, "vector_inventory_2_Cassette1Feature3","", choices = c('', feature$NewFeature))

      updateSelectInput(session, "vector_inventory_2_Cassette6Promoter","", choices = c('', promoter$Promoter))
      updateSelectInput(session, "vector_inventory_2_Cassette6Gene", "", choices =  c('', gene$Gene))
      updateSelectInput(session, "vector_inventory_2_Cassette6Terminator","", choices = c('', terminator$Terminator))
      updateSelectInput(session, "vector_inventory_2_Cassette6Feature1","", choices = c('', feature$NewFeature))
      updateSelectInput(session, "vector_inventory_2_Cassette6Feature2","", choices = c('', feature$NewFeature))
      updateSelectInput(session, "vector_inventory_2_Cassette6Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette2Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette2Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette2Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette2Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette2Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette2Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette7Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette7Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette7Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette7Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette7Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette7Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette3Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette3Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette3Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette3Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette3Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette3Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette8Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette8Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette8Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette8Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette8Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette8Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette4Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette4Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette4Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette4Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette4Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette4Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette9Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette9Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette9Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette9Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette9Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette9Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette5Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette5Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette5Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette5Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette5Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette5Feature3","", choices = c('', feature$NewFeature))

       updateSelectInput(session, "vector_inventory_2_Cassette10Promoter","", choices = c('', promoter$Promoter))
       updateSelectInput(session, "vector_inventory_2_Cassette10Gene","", choices =  c('', gene$Gene))
       updateSelectInput(session, "vector_inventory_2_Cassette10Terminator","", choices = c('', terminator$Terminator))
       updateSelectInput(session, "vector_inventory_2_Cassette10Feature1","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette10Feature2","", choices = c('', feature$NewFeature))
       updateSelectInput(session, "vector_inventory_2_Cassette10Feature3","", choices = c('', feature$NewFeature))

})

# Clear
observeEvent(input$vector_inventory_2_Clear,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_2_Clear_Confrim",
    type = "warning",
    title = "",
    text = "Do you really want to clear the fields?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})
observeEvent(input$vector_inventory_2_Clear_Confrim, {
  if(input$vector_inventory_2_Clear_Confrim == TRUE){
    reset("vector_inventory_2_form")
  }
}, ignoreInit = TRUE)

## Exit
observeEvent(input$vector_inventory_2_Exit,{
 confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_2_Exit_Confirm",
    type = "warning",
    title = "",
    text =  "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})
observeEvent(input$vector_inventory_2_Exit_Confirm, {
  if(input$vector_inventory_2_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)

## ---------------------------------------------------------------------------------------------------------------------- 3. Vector Inventory 3

# Exit

observeEvent(input$vector_inventory_3_Clear,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_3_Cleart_Confirm",
    type = "warning",
    title = "",
    text =  "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})

observeEvent(input$vector_inventory_3_Cleart_Confirm, {
  if(input$vector_inventory_3_Cleart_Confirm == TRUE){
    reset("vector_inventory_3_form")
  }
}, ignoreInit = TRUE)

## Exit
observeEvent(input$vector_inventory_3_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_3_Exit_Confirm",
    type = "warning",
    title = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})

observeEvent(input$vector_inventory_3_Exit_Confirm, {
  if(input$vector_inventory_3_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)
## -----------------------------------------------------------------------------------------------------------------------4. Vector Inventory 4

# update strain

observeEvent(input$vector_inventory_module,{
  strain <-  tbl(pool, paste0(input$project_selected, "_tblStrain")) %>% collect()
  updateSelectInput(session, "vector_inventory_4_Strain", "Strain", choices = c("",strain$Strain))
})

observeEvent(input$vector_inventory_4_Clear,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset("vector_inventory_4_form")
    },
    text = "Do you really want to clear the fields?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})


## Exit
observeEvent(input$vector_inventory_4_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_4_Exit_Confirm",
    type = "warning",
    title = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})

observeEvent(input$vector_inventory_4_Exit_Confirm, {
  if(input$vector_inventory_4_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)

## -----------------------------------------------------------------------------------------------------------------------5. Vector Inventory 5

# Save fields from all tabs

observeEvent(input$vector_inventory_5_SaveThePlantExpressionVectorRecord, {
  req(input$vector_inventory_1_VectorCode)
  vector_inventory_values$Data <- vector_inventory()
  dt <- vector_inventory_values$Data

      # gather data from tabs
  df <- data.frame(
    VectorID = paste0(input$vector_inventory_1_VectorPrefix,"-", input$vector_inventory_1_VectorCode, "-", input$vector_inventory_1_VectorSuffix),
    VectorPrefix = input$vector_inventory_1_VectorPrefix,
    VectorCode = input$vector_inventory_1_VectorCode,
    VectorSuffix = as.integer(nrow(dt)+1),
    BacterialSelection = ifelse(nchar(input$vector_inventory_1_BacterialSelection)>0, input$vector_inventory_1_BacterialSelection,''),
    PlantSelection = ifelse(nchar(input$vector_inventory_1_PlantSelection)>0, input$vector_inventory_1_PlantSelection,''),
    Synonym1 = ifelse(nchar(input$vector_inventory_1_Synonym1)>0, input$vector_inventory_1_Synonym1,''),
    Synonym2 = ifelse(nchar(input$vector_inventory_1_Synonym2)>0, input$vector_inventory_1_Synonym2,''),
    Synonym3 = ifelse(nchar(input$vector_inventory_1_Synonym3)>0, input$vector_inventory_1_Synonym3,''),
    Synonym4 = ifelse(nchar(input$vector_inventory_1_Synonym4)>0, input$vector_inventory_1_Synonym4,''),
    Synonym5 = ifelse(nchar(input$vector_inventory_1_Synonym5)>0, input$vector_inventory_1_Synonym5,''),
    Backbone = ifelse(nchar(input$vector_inventory_1_Backbone)>0, input$vector_inventory_1_Backbone,''),
    Cassette1Promoter = ifelse(nchar(input$vector_inventory_2_Cassette1Promoter)>0, input$vector_inventory_2_Cassette1Promoter,''),
    Cassette1Gene = ifelse(nchar(input$vector_inventory_2_Cassette1Gene)>0, input$vector_inventory_2_Cassette1Gene,''),
    Cassette1Terminator = ifelse(nchar(input$vector_inventory_2_Cassette1Terminator)>0, input$vector_inventory_2_Cassette1Terminator,''),
    Cassette1Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette1Feature1)>0, input$vector_inventory_2_Cassette1Feature1,''),
    Cassette1Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette1Feature1Desc)>0, input$vector_inventory_2_Cassette1Feature1Desc,''),
    Cassette1Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette1Feature2)>0, input$vector_inventory_2_Cassette1Feature2,''),
    Cassette1Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette1Feature2Desc)>0, input$vector_inventory_2_Cassette1Feature2Desc,''),
    Cassette1Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette1Feature3)>0, input$vector_inventory_2_Cassette1Feature3,''),
    Cassette1Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette1Feature3Desc)>0, input$vector_inventory_2_Cassette1Feature3Desc,''),
    Cassette2Promoter = ifelse(nchar(input$vector_inventory_2_Cassette2Promoter)>0, input$vector_inventory_2_Cassette2Promoter,''),
    Cassette2Gene = ifelse(nchar(input$vector_inventory_2_Cassette2Gene)>0, input$vector_inventory_2_Cassette2Gene,''),
    Cassette2Terminator = ifelse(nchar(input$vector_inventory_2_Cassette2Terminator)>0, input$vector_inventory_2_Cassette2Terminator,''),
    Cassette2Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette2Feature1)>0, input$vector_inventory_2_Cassette2Feature1,''),
    Cassette2Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette2Feature1Desc)>0, input$vector_inventory_2_Cassette2Feature1Desc,''),
    Cassette2Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette2Feature2)>0, input$vector_inventory_2_Cassette2Feature2,''),
    Cassette2Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette2Feature2Desc)>0, input$vector_inventory_2_Cassette2Feature2Desc,''),
    Cassette2Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette2Feature3)>0, input$vector_inventory_2_Cassette2Feature3,''),
    Cassette2Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette2Feature3Desc)>0, input$vector_inventory_2_Cassette2Feature3Desc,''),
    Cassette3Promoter = ifelse(nchar(input$vector_inventory_2_Cassette3Promoter)>0, input$vector_inventory_2_Cassette3Promoter,''),
    Cassette3Gene = ifelse(nchar(input$vector_inventory_2_Cassette3Gene)>0, input$vector_inventory_2_Cassette3Gene,''),
    Cassette3Terminator = ifelse(nchar(input$vector_inventory_2_Cassette3Terminator)>0, input$vector_inventory_2_Cassette3Terminator,''),
    Cassette3Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette3Feature1)>0, input$vector_inventory_2_Cassette3Feature1,''),
    Cassette3Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette3Feature1Desc)>0, input$vector_inventory_2_Cassette3Feature1Desc,''),
    Cassette3Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette3Feature2)>0, input$vector_inventory_2_Cassette3Feature2,''),
    Cassette3Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette3Feature2Desc)>0, input$vector_inventory_2_Cassette3Feature2Desc,''),
    Cassette3Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette3Feature3)>0, input$vector_inventory_2_Cassette3Feature3,''),
    Cassette3Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette3Feature3Desc)>0, input$vector_inventory_2_Cassette3Feature3Desc,''),
    Cassette4Promoter = ifelse(nchar(input$vector_inventory_2_Cassette4Promoter)>0, input$vector_inventory_2_Cassette4Promoter,''),
    Cassette4Gene = ifelse(nchar(input$vector_inventory_2_Cassette4Gene)>0, input$vector_inventory_2_Cassette4Gene,''),
    Cassette4Terminator = ifelse(nchar(input$vector_inventory_2_Cassette4Terminator)>0, input$vector_inventory_2_Cassette4Terminator,''),
    Cassette4Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette4Feature1)>0, input$vector_inventory_2_Cassette4Feature1,''),
    Cassette4Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette4Feature1Desc)>0, input$vector_inventory_2_Cassette4Feature1Desc,''),
    Cassette4Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette4Feature2)>0, input$vector_inventory_2_Cassette4Feature2,''),
    Cassette4Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette4Feature2Desc)>0, input$vector_inventory_2_Cassette4Feature2Desc,''),
    Cassette4Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette4Feature3)>0, input$vector_inventory_2_Cassette4Feature3,''),
    Cassette4Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette4Feature3Desc)>0, input$vector_inventory_2_Cassette4Feature3Desc,''),
    Cassette5Promoter = ifelse(nchar(input$vector_inventory_2_Cassette5Promoter)>0, input$vector_inventory_2_Cassette5Promoter,''),
    Cassette5Gene = ifelse(nchar(input$vector_inventory_2_Cassette5Gene)>0, input$vector_inventory_2_Cassette5Gene,''),
    Cassette5Terminator = ifelse(nchar(input$vector_inventory_2_Cassette5Terminator)>0, input$vector_inventory_2_Cassette5Terminator,''),
    Cassette5Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette5Feature1)>0, input$vector_inventory_2_Cassette5Feature1,''),
    Cassette5Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette5Feature1Desc)>0, input$vector_inventory_2_Cassette5Feature1Desc,''),
    Cassette5Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette5Feature2)>0, input$vector_inventory_2_Cassette5Feature2,''),
    Cassette5Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette5Feature2Desc)>0, input$vector_inventory_2_Cassette5Feature2Desc,''),
    Cassette5Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette5Feature3)>0, input$vector_inventory_2_Cassette5Feature3,''),
    Cassette5Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette5Feature3Desc)>0, input$vector_inventory_2_Cassette5Feature3Desc,''),
    VNTI_Map_Location = ifelse(!is.null(input$vector_inventory_1_VNTILocate), input$vector_inventory_1_VNTILocate,''),
    ClonedBy = ifelse(nchar(input$vector_inventory_1_ClonedBy)>0, input$vector_inventory_1_ClonedBy,''),
    ClonedDate = input$vector_inventory_1_DateOfCloning,
    LabBookNumber = input$vector_inventory_1_LabBookNumber,
    PageNumber = input$vector_inventory_1_PageNumber,
    Cassette1ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette1ForwardName)>0, input$vector_inventory_3_Cassette1ForwardName,''),
    Cassette1ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette1ForwardCode)>0, input$vector_inventory_3_Cassette1ForwardCode,''),
    Cassette1ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette1ForwardSequence)>0, input$vector_inventory_3_Cassette1ForwardSequence,''),
    Cassette1ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette1ReverseName)>0, input$vector_inventory_3_Cassette1ReverseName,''),
    Cassette1ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette1ReverseCode)>0, input$vector_inventory_3_Cassette1ReverseCode,''),
    Cassette1ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette1ReverseSequence)>0, input$vector_inventory_3_Cassette1ReverseSequence,''),
    Cassette2ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette2ForwardName)>0, input$vector_inventory_3_Cassette2ForwardName,''),
    Cassette2ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette2ForwardCode)>0, input$vector_inventory_3_Cassette2ForwardCode,''),
    Cassette2ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette2ForwardSequence)>0, input$vector_inventory_3_Cassette2ForwardSequence,''),
    Cassette2ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette2ReverseName)>0, input$vector_inventory_3_Cassette2ReverseName,''),
    Cassette2ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette2ReverseCode)>0, input$vector_inventory_3_Cassette2ReverseCode,''),
    Cassette2ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette2ReverseSequence)>0, input$vector_inventory_3_Cassette2ReverseSequence,''),
    Cassette3ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette3ForwardName)>0, input$vector_inventory_3_Cassette3ForwardName,''),
    Cassette3ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette3ForwardCode)>0, input$vector_inventory_3_Cassette3ForwardCode,''),
    Cassette3ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette3ForwardSequence)>0, input$vector_inventory_3_Cassette3ForwardSequence,''),
    Cassette3ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette3ReverseName)>0, input$vector_inventory_3_Cassette3ReverseName,''),
    Cassette3ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette3ReverseCode)>0, input$vector_inventory_3_Cassette3ReverseCode,''),
    Cassette3ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette3ReverseSequence)>0, input$vector_inventory_3_Cassette3ReverseSequence,''),
    Cassette4ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette4ForwardName)>0, input$vector_inventory_3_Cassette4ForwardName,''),
    Cassette4ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette4ForwardCode)>0, input$vector_inventory_3_Cassette4ForwardCode,''),
    Cassette4ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette4ForwardSequence)>0, input$vector_inventory_3_Cassette4ForwardSequence,''),
    Cassette4ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette4ReverseName)>0, input$vector_inventory_3_Cassette4ReverseName,''),
    Cassette4ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette4ReverseCode)>0, input$vector_inventory_3_Cassette4ReverseCode,''),
    Cassette4ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette4ReverseSequence)>0, input$vector_inventory_3_Cassette4ReverseSequence,''),
    Cassette5ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette5ForwardName)>0, input$vector_inventory_3_Cassette5ForwardName,''),
    Cassette5ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette5ForwardCode)>0, input$vector_inventory_3_Cassette5ForwardCode,''),
    Cassette5ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette5ForwardSequence)>0, input$vector_inventory_3_Cassette5ForwardSequence,''),
    Cassette5ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette5ReverseName)>0, input$vector_inventory_3_Cassette5ReverseName,''),
    Cassette5ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette5ReverseCode)>0, input$vector_inventory_3_Cassette5ReverseCode,''),
    Cassette5ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette5ReverseSequence)>0, input$vector_inventory_3_Cassette5ReverseSequence,''),
    SequencingPrimers = ifelse(nchar(input$vector_inventory_4_SequencingPrimers)>0, input$vector_inventory_4_SequencingPrimers,''),
    SequencingCompleted = ifelse(nchar(input$vector_inventory_4_SequencingCompleted)>0, input$vector_inventory_4_SequencingCompleted,''),
    DateOfSequencing = input$vector_inventory_4_DateOfSequencing,
    SeqPrimersLabBookNumber = input$vector_inventory_4_SeqPrimersLabBookNumber,
    SeqPrimersPageNumber = input$vector_inventory_4_SeqPrimersPageNumber,
    ContigExpressSequencingAlignment = ifelse(!is.null(input$vector_inventory_4_ContigExpressSequencingAlignment), input$vector_inventory_4_ContigExpressSequencingAlignment,''),
    SequencingFiles = ifelse(!is.null(input$vector_inventory_4_SequencingFiles), input$vector_inventory_4_SequencingFiles,''),
    CheckedBy = ifelse(nchar(input$vector_inventory_4_CheckedBy)>0, input$vector_inventory_4_CheckedBy,''),
    CheckedDate = input$vector_inventory_4_CheckedDate,
    VerifiedBy = ifelse(nchar(input$vector_inventory_4_VerifiedBy)>0, input$vector_inventory_4_VerifiedBy,''),
    VerifiedDate = input$vector_inventory_4_VerifiedDate,
    TranformedIntoAgro = ifelse(nchar(input$vector_inventory_4_TranformedIntoAgro)>0, input$vector_inventory_4_TranformedIntoAgro,''),
    Strain = ifelse(nchar(input$vector_inventory_4_Strain)>0, input$vector_inventory_4_Strain,''),
    ConfirmedByPCR = ifelse(nchar(input$vector_inventory_4_ConfirmedByPCR)>0, input$vector_inventory_4_ConfirmedByPCR,''),
    ConfirmedByPCRDate = input$vector_inventory_4_ConfirmedByPCRDate,
    DNAStorageLocation = ifelse(nchar(input$vector_inventory_5_DNAStorageLocation)>0, input$vector_inventory_5_DNAStorageLocation,''),
    DNAStorageBox = ifelse(nchar(input$vector_inventory_5_DNAStorageBox)>0, input$vector_inventory_5_DNAStorageBox,''),
    DNAStoredBy = ifelse(nchar(input$vector_inventory_5_DNAStoredBy)>0, input$vector_inventory_5_DNAStoredBy,''),
    DNAStorageDate = input$vector_inventory_5_DNAStorageDate,
    EColiGlycerolStorageLocation = ifelse(nchar(input$vector_inventory_5_EcoliGlycerolStorageLocation)>0, input$vector_inventory_5_EcoliGlycerolStorageLocation,''),
    EColiGlycerolStorageBox = ifelse(nchar(input$vector_inventory_5_EcoliGlycerolStorageBox)>0, input$vector_inventory_5_EcoliGlycerolStorageBox,''),
    EColiGlycerolStoredBy = ifelse(nchar(input$vector_inventory_5_EcoliGlycerolStoredBy)>0, input$vector_inventory_5_EcoliGlycerolStoredBy,''),
    EColiGlycerolStorageDate = input$vector_inventory_5_EcoliGlycerolStorageDate,
    AgroGlycerolStorageLocation = ifelse(nchar(input$vector_inventory_5_AgroGlycerolStorageLocation)>0, input$vector_inventory_5_AgroGlycerolStorageLocation,''),
    AgroGlycerolStorageBox = ifelse(nchar(input$vector_inventory_5_AgroGlycerolStorageBox)>0, input$vector_inventory_5_AgroGlycerolStorageBox,''),
    AgroGlycerolStoredBy = ifelse(nchar(input$vector_inventory_5_AgroGlycerolStoredBy)>0, input$vector_inventory_5_AgroGlycerolStoredBy,''),
    AgroGlycerolStorageDate = input$vector_inventory_5_AgroGlycerolStorageDate,
    Cassette6Promoter = ifelse(nchar(input$vector_inventory_2_Cassette6Promoter)>0, input$vector_inventory_2_Cassette6Promoter,''),
    Cassette6Gene = ifelse(nchar(input$vector_inventory_2_Cassette6Gene)>0, input$vector_inventory_2_Cassette6Gene,''),
    Cassette6Terminator = ifelse(nchar(input$vector_inventory_2_Cassette6Terminator)>0, input$vector_inventory_2_Cassette6Terminator,''),
    Cassette6Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette6Feature1)>0, input$vector_inventory_2_Cassette6Feature1,''),
    Cassette6Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette6Feature1Desc)>0, input$vector_inventory_2_Cassette6Feature1Desc,''),
    Cassette6Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette6Feature2)>0, input$vector_inventory_2_Cassette6Feature2,''),
    Cassette6Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette6Feature2Desc)>0, input$vector_inventory_2_Cassette6Feature2Desc,''),
    Cassette6Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette6Feature3)>0, input$vector_inventory_2_Cassette6Feature3,''),
    Cassette6Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette6Feature3Desc)>0, input$vector_inventory_2_Cassette6Feature3Desc,''),
    Cassette7Promoter = ifelse(nchar(input$vector_inventory_2_Cassette7Promoter)>0, input$vector_inventory_2_Cassette7Promoter,''),
    Cassette7Gene = ifelse(nchar(input$vector_inventory_2_Cassette7Gene)>0, input$vector_inventory_2_Cassette7Gene,''),
    Cassette7Terminator = ifelse(nchar(input$vector_inventory_2_Cassette7Terminator)>0, input$vector_inventory_2_Cassette7Terminator,''),
    Cassette7Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette7Feature1)>0, input$vector_inventory_2_Cassette7Feature1,''),
    Cassette7Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette7Feature1Desc)>0, input$vector_inventory_2_Cassette7Feature1Desc,''),
    Cassette7Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette7Feature2)>0, input$vector_inventory_2_Cassette7Feature2,''),
    Cassette7Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette7Feature2Desc)>0, input$vector_inventory_2_Cassette7Feature2Desc,''),
    Cassette7Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette7Feature3)>0, input$vector_inventory_2_Cassette7Feature3,''),
    Cassette7Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette7Feature3Desc)>0, input$vector_inventory_2_Cassette7Feature3Desc,''),
    Cassette8Promoter = ifelse(nchar(input$vector_inventory_2_Cassette8Promoter)>0, input$vector_inventory_2_Cassette8Promoter,''),
    Cassette8Gene = ifelse(nchar(input$vector_inventory_2_Cassette8Gene)>0, input$vector_inventory_2_Cassette8Gene,''),
    Cassette8Terminator = ifelse(nchar(input$vector_inventory_2_Cassette8Terminator)>0, input$vector_inventory_2_Cassette8Terminator,''),
    Cassette8Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette8Feature1)>0, input$vector_inventory_2_Cassette8Feature1,''),
    Cassette8Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette8Feature1Desc)>0, input$vector_inventory_2_Cassette8Feature1Desc,''),
    Cassette8Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette8Feature2)>0, input$vector_inventory_2_Cassette8Feature2,''),
    Cassette8Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette8Feature2Desc)>0, input$vector_inventory_2_Cassette8Feature2Desc,''),
    Cassette8Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette8Feature3)>0, input$vector_inventory_2_Cassette8Feature3,''),
    Cassette8Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette8Feature3Desc)>0, input$vector_inventory_2_Cassette8Feature3Desc,''),
    Cassette9Promoter = ifelse(nchar(input$vector_inventory_2_Cassette9Promoter)>0, input$vector_inventory_2_Cassette9Promoter,''),
    Cassette9Gene = ifelse(nchar(input$vector_inventory_2_Cassette9Gene)>0, input$vector_inventory_2_Cassette9Gene,''),
    Cassette9Terminator = ifelse(nchar(input$vector_inventory_2_Cassette9Terminator)>0, input$vector_inventory_2_Cassette9Terminator,''),
    Cassette9Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette9Feature1)>0, input$vector_inventory_2_Cassette9Feature1,''),
    Cassette9Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette9Feature1Desc)>0, input$vector_inventory_2_Cassette9Feature1Desc,''),
    Cassette9Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette9Feature2)>0, input$vector_inventory_2_Cassette9Feature2,''),
    Cassette9Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette9Feature2Desc)>0, input$vector_inventory_2_Cassette9Feature2Desc,''),
    Cassette9Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette9Feature3)>0, input$vector_inventory_2_Cassette9Feature3,''),
    Cassette9Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette9Feature3Desc)>0, input$vector_inventory_2_Cassette9Feature3Desc,''),
    Cassette10Promoter = ifelse(nchar(input$vector_inventory_2_Cassette10Promoter)>0, input$vector_inventory_2_Cassette10Promoter,''),
    Cassette10Gene = ifelse(nchar(input$vector_inventory_2_Cassette10Gene)>0, input$vector_inventory_2_Cassette10Gene,''),
    Cassette10Terminator = ifelse(nchar(input$vector_inventory_2_Cassette10Terminator)>0, input$vector_inventory_2_Cassette10Terminator,''),
    Cassette10Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette10Feature1)>0, input$vector_inventory_2_Cassette10Feature1,''),
    Cassette10Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette10Feature1Desc)>0, input$vector_inventory_2_Cassette10Feature1Desc,''),
    Cassette10Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette10Feature2)>0, input$vector_inventory_2_Cassette10Feature2,''),
    Cassette10Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette10Feature2Desc)>0, input$vector_inventory_2_Cassette10Feature2Desc,''),
    Cassette10Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette10Feature3)>0, input$vector_inventory_2_Cassette10Feature3,''),
    Cassette10Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette10Feature3Desc)>0, input$vector_inventory_2_Cassette10Feature3Desc,''),
    Cassette6ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette6ForwardName)>0, input$vector_inventory_3_Cassette6ForwardName,''),
    Cassette6ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette6ForwardCode)>0, input$vector_inventory_3_Cassette6ForwardCode,''),
    Cassette6ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette6ForwardSequence)>0, input$vector_inventory_3_Cassette6ForwardSequence,''),
    Cassette6ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette6ReverseName)>0, input$vector_inventory_3_Cassette6ReverseName,''),
    Cassette6ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette6ReverseCode)>0, input$vector_inventory_3_Cassette6ReverseCode,''),
    Cassette6ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette6ReverseSequence)>0, input$vector_inventory_3_Cassette6ReverseSequence,''),
    Cassette7ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette7ForwardName)>0, input$vector_inventory_3_Cassette7ForwardName,''),
    Cassette7ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette7ForwardCode)>0, input$vector_inventory_3_Cassette7ForwardCode,''),
    Cassette7ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette7ForwardSequence)>0, input$vector_inventory_3_Cassette7ForwardSequence,''),
    Cassette7ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette7ReverseName)>0, input$vector_inventory_3_Cassette7ReverseName,''),
    Cassette7ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette7ReverseCode)>0, input$vector_inventory_3_Cassette7ReverseCode,''),
    Cassette7ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette7ReverseSequence)>0, input$vector_inventory_3_Cassette7ReverseSequence,''),
    Cassette8ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette8ForwardName)>0, input$vector_inventory_3_Cassette8ForwardName,''),
    Cassette8ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette8ForwardCode)>0, input$vector_inventory_3_Cassette8ForwardCode,''),
    Cassette8ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette8ForwardSequence)>0, input$vector_inventory_3_Cassette8ForwardSequence,''),
    Cassette8ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette8ReverseName)>0, input$vector_inventory_3_Cassette8ReverseName,''),
    Cassette8ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette8ReverseCode)>0, input$vector_inventory_3_Cassette8ReverseCode,''),
    Cassette8ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette8ReverseSequence)>0, input$vector_inventory_3_Cassette8ReverseSequence,''),
    Cassette9ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette9ForwardName)>0, input$vector_inventory_3_Cassette9ForwardName,''),
    Cassette9ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette9ForwardCode)>0, input$vector_inventory_3_Cassette9ForwardCode,''),
    Cassette9ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette9ForwardSequence)>0, input$vector_inventory_3_Cassette9ForwardSequence,''),
    Cassette9ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette9ReverseName)>0, input$vector_inventory_3_Cassette9ReverseName,''),
    Cassette9ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette9ReverseCode)>0, input$vector_inventory_3_Cassette9ReverseCode,''),
    Cassette9ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette9ReverseSequence)>0, input$vector_inventory_3_Cassette9ReverseSequence,''),
    Cassette10ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette10ForwardName)>0, input$vector_inventory_3_Cassette10ForwardName,''),
    Cassette10ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette10ForwardCode)>0, input$vector_inventory_3_Cassette10ForwardCode,''),
    Cassette10ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette10ForwardSequence)>0, input$vector_inventory_3_Cassette10ForwardSequence,''),
    Cassette10ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette10ReverseName)>0, input$vector_inventory_3_Cassette10ReverseName,''),
    Cassette10ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette10ReverseCode)>0, input$vector_inventory_3_Cassette10ReverseCode,''),
    Cassette10ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette10ReverseSequence)>0, input$vector_inventory_3_Cassette10ReverseSequence,'')

  )

#  write into tables
  if(input$vector_inventory_1_VectorCode ==""){
    shinyalert("Error!", "Enter a Value for Vector Code in the First Tab", type = "error")
  } else {
    if((df$VectorID %in% dt$VectorID)==FALSE){
      dbWriteTable(pool, paste0(input$project_selected, "_tblVectorInventory"), df, append = T)
      shinyalert("", "Record Added", type = "success")
    } else {
      shinyalert("Warning!", "Record Exists", type = "error")
    }
  }
})



# Clear fields
observeEvent(input$vector_inventory_5_Clear,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_5_Clear_Confirm",
    type = "warning",
    title = "",
    text =  "Do you really want to clear the fields?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})


observeEvent(input$vector_inventory_5_Clear_Confirm, {
  if(input$vector_inventory_5_Clear_Confirm == TRUE){
    reset("vector_inventory_1_form")
    reset("vector_inventory_2_form")
    reset("vector_inventory_3_form")
    reset("vector_inventory_4_form")
    reset("vector_inventory_5_form")
  }
}, ignoreInit = TRUE)



## Exit
observeEvent(input$vector_inventory_5_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_5_Exit_Confirm",
    type = "warning",
    title = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})

observeEvent(input$vector_inventory_5_Exit_Confirm, {
  if(input$vector_inventory_5_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }
}, ignoreInit = TRUE)


# ---------------------------------------------------------------------------------------------------------------------- 6. Search

 vector_inventory_search_input_values <- reactiveValues()
 
 output$vector_inventory_search_Fields_Output <- renderUI({
   Gene <- vector_inventory() %>%
     dplyr::select(contains("Gene")) %>%
     as.character()
   Promoter <- vector_inventory() %>%
     dplyr::select(contains("Promoter")) %>%
     as.character()
   Terminator <- vector_inventory() %>%
     dplyr::select(contains("Terminator")) %>%
     as.character()
   Backbone <- vector_inventory() %>%
     dplyr::select(contains("Backbone")) %>%
     as.character()
   
   div(
     column(5,
            column(4, selectInput("vector_inventory_search_VectorID", "Vector ID", choices = c('', unique(vector_inventory()$VectorID)))),
            column(4, textInput("vector_inventory_search_Synonyms", "Synonyms")),
            column(4, selectInput("vector_inventory_search_Promoter", "Promoter", choices = c('', unique(unique(Promoter)))))
     ),
     column(7,
            column(3, selectInput("vector_inventory_search_Gene", "Gene", choices = c('', unique(unique(Gene))))),
            column(3, selectInput("vector_inventory_search_Terminator", "Terminator", choices = c('', unique(unique(Terminator))))),
            column(3, selectInput("vector_inventory_search_Backbone", "Backbone", choices = c('', unique(unique(Backbone))))),
            column(3, selectInput("vector_inventory_search_ClonedBy", "Cloned By", choices = c('', unique(vector_inventory()$ClonedBy))))
     )
   )
 })
 
 
 observeEvent(input$vector_inventory_module,{
   updateDateRangeInput(session, "vector_inventory_search_ClonedDate", "Date of Cloning", start = min(vector_inventory()$ClonedDate), end = max(vector_inventory()$ClonedDate),
                        min = min(vector_inventory()$ClonedDate), max = max(vector_inventory()$ClonedDate))
 })
 
vector_inventory_search_input <- reactive({
  df <- vector_inventory()
  # vectorID
  if(input$vector_inventory_search_VectorID !=''){
    df <- df %>%
      dplyr::filter(trimws(VectorID) == trimws(input$vector_inventory_search_VectorID))
  } 
  # Synonyms
  if(input$vector_inventory_search_Synonyms !=''){
    df <- df %>%
      dplyr::filter(trimws(Synonym1) == trimws(input$vector_inventory_search_Synonyms) |
                    trimws(Synonym2) == trimws(input$vector_inventory_search_Synonyms) |
                    trimws(Synonym3) == trimws(input$vector_inventory_search_Synonyms) |
                    trimws(Synonym4) == trimws(input$vector_inventory_search_Synonyms) |
                    trimws(Synonym5) == trimws(input$vector_inventory_search_Synonyms))
  } 
  # Promoter
  if(input$vector_inventory_search_Promoter !=''){
    df <- df %>%
      dplyr::filter(trimws(Cassette1Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette2Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette3Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette4Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette5Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette6Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette7Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette8Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette9Promoter) == trimws(input$vector_inventory_search_Promoter) |
                    trimws(Cassette10Promoter) == trimws(input$vector_inventory_search_Promoter))
  } 
  # gene
  if(input$vector_inventory_search_Gene !=''){
    df <- df %>%
      dplyr::filter(trimws(Cassette1Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette2Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette3Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette4Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette5Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette6Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette7Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette8Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette9Gene) == trimws(input$vector_inventory_search_Gene) |
                    trimws(Cassette10Gene) == trimws(input$vector_inventory_search_Gene))
  } 
  # terminator
  if(input$vector_inventory_search_Terminator !=''){
    df <- df %>%
      dplyr::filter(trimws(Cassette1Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette2Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette3Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette4Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette5Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette6Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette7Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette8Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette9Terminator) == trimws(input$vector_inventory_search_Terminator) |
                    trimws(Cassette10Terminator) == trimws(input$vector_inventory_search_Terminator)
                    )
  }
  # backbone
  if(input$vector_inventory_search_Backbone !=''){
    df <- df %>%
      dplyr::filter(trimws(Backbone) == trimws(input$vector_inventory_search_Backbone))
  } 
  # cloned by
  if(input$vector_inventory_search_ClonedBy !=''){
    df <- df %>%
      dplyr::filter(trimws(ClonedBy) == trimws(input$vector_inventory_search_ClonedBy))
  } 
  df 
})

# search results table
observeEvent(input$vector_inventory_search_Search, {
  output$vector_inventory_search_ResultsTable <- renderRHandsontable({
   dt <- vector_inventory_search_input()
   rhandsontable(dt, selectCallback = T, readOnly = T, rowHeaders=F) %>%
               hot_table(stretchH = "all")
  })
})

# Clear all entries
observeEvent(input$vector_inventory_search_ClearForm,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_search_ClearForm_Confirm",
    type = "warning",
    title = "",
    text = "Do you really want to clear the fields?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})

observeEvent(input$vector_inventory_search_ClearForm_Confirm, {
  if(input$vector_inventory_search_ClearForm_Confirm == TRUE){
    reset("vector_inventory_search_form")
    shinyjs::hide("vector_inventory_search_ResultsTable")
  }
}, ignoreInit = TRUE)

# update Selected Vecor ID
observeEvent(input$vector_inventory_search_ResultsTable_select$select$r,{
  r <- input$vector_inventory_search_ResultsTable_select$select$r
  dv <- vector_inventory_search_input()
  v <- as.character(dv[r,'VectorID'])

  updateTextInput(session, "vector_inventory_search_SelectedVectorID", "Selected Vector ID", value = v)
})


# Load data to View -- load into the first 5 tabs
observeEvent(input$vector_inventory_search_LoadDataToView, {
  shinyjs::hide("vector_inventory_5_SaveThePlantExpressionVectorRecord") # -----------------
  vector_inventory <- vector_inventory_values$Data <- vector_inventory() %>%
    dplyr::filter(VectorID == input$vector_inventory_search_SelectedVectorID)

  # inventory 1
  updateTextInput(session, "vector_inventory_1_VectorID", "Vector ID", value = vector_inventory$VectorID)
  updateTextInput(session, "vector_inventory_1_VectorPrefix", "Vector Prefix", value = vector_inventory$VectorPrefix)
  updateTextInput(session, "vector_inventory_1_VectorCode", "Vector Code", value = vector_inventory$VectorCode)
  updateTextInput(session, "vector_inventory_1_VectorSuffix", "Vector Suffix", value = vector_inventory$VectorSuffix)
  updateSelectInput(session, "vector_inventory_1_BacterialSelection","Bacterial Selection", choices = c(vector_inventory$BacterialSelection))
  updateSelectInput(session, "vector_inventory_1_PlantSelection","Plant Selection", choices = c(vector_inventory$PlantSelection))
  updateTextInput(session, "vector_inventory_1_Synonyms1", "Synonyms 1", value = vector_inventory$Synonym1)
  updateTextInput(session, "vector_inventory_1_Synonyms2", "Synonyms 2", value = vector_inventory$Synonym2)
  updateTextInput(session, "vector_inventory_1_Synonyms3", "Synonyms 3", value = vector_inventory$Synonym3)
  updateTextInput(session, "vector_inventory_1_Synonyms4", "Synonyms 4", value = vector_inventory$Synonym4)
  updateTextInput(session, "vector_inventory_1_Synonyms5", "Synonyms 5", value = vector_inventory$Synonym5)
  updateSelectInput(session, "vector_inventory_1_Backbone","", choices = c(vector_inventory$Backbone))
  updateSelectInput(session, "vector_inventory_1_ClonedBy","", choices = c(vector_inventory$ClonedBy))
  updateDateInput(session, "vector_inventory_1_DateOfCloning","", value = vector_inventory$ClonedDate)
  updateNumericInput(session, "vector_inventory_1_LabBookNumber", "", value = vector_inventory$LabBookNumber)
  updateNumericInput(session, "vector_inventory_1_PageNumber", "", value = vector_inventory$PageNumber)

 # inventory 2

 updateSelectInput(session,"vector_inventory_2_Cassette1Promoter","", choices = c(vector_inventory$Cassette1Promoter))
 updateSelectInput(session,"vector_inventory_2_Cassette2Promoter","", choices = c(vector_inventory$Cassette2Promoter))
 updateSelectInput(session,"vector_inventory_2_Cassette3Promoter","", choices = c(vector_inventory$Cassette3Promoter))
 updateSelectInput(session,"vector_inventory_2_Cassette4Promoter","", choices = c(vector_inventory$Cassette4Promoter))
 updateSelectInput(session,"vector_inventory_2_Cassette5Promoter","", choices = c(vector_inventory$Cassette5Promoter))

 updateSelectInput(session,"vector_inventory_2_Cassette1Gene","", choices =  c(vector_inventory$Cassette1Gene))
 updateSelectInput(session,"vector_inventory_2_Cassette2Gene","", choices =  c(vector_inventory$Cassette2Gene))
 updateSelectInput(session,"vector_inventory_2_Cassette3Gene","", choices =  c(vector_inventory$Cassette3Gene))
 updateSelectInput(session,"vector_inventory_2_Cassette4Gene","", choices =  c(vector_inventory$Cassette4Gene))
 updateSelectInput(session,"vector_inventory_2_Cassette5Gene","", choices =  c(vector_inventory$Cassette5Gene))

 updateSelectInput(session,"vector_inventory_2_Cassette1Terminator","", choices = c(vector_inventory$Cassette1Terminator))
 updateSelectInput(session,"vector_inventory_2_Cassette2Terminator","", choices = c(vector_inventory$Cassette2Terminator))
 updateSelectInput(session,"vector_inventory_2_Cassette3Terminator","", choices = c(vector_inventory$Cassette3Terminator))
 updateSelectInput(session,"vector_inventory_2_Cassette4Terminator","", choices = c(vector_inventory$Cassette4Terminator))
 updateSelectInput(session,"vector_inventory_2_Cassette5Terminator","", choices = c(vector_inventory$Cassette5Terminator))

 updateSelectInput(session,"vector_inventory_2_Cassette1Feature1","", choices = c(vector_inventory$Cassette1Feature1))
 updateSelectInput(session,"vector_inventory_2_Cassette2Feature1","", choices = c(vector_inventory$Cassette2Feature1))
 updateSelectInput(session,"vector_inventory_2_Cassette3Feature1","", choices = c(vector_inventory$Cassette3Feature1))
 updateSelectInput(session,"vector_inventory_2_Cassette4Feature1","", choices = c(vector_inventory$Cassette4Feature1))
 updateSelectInput(session,"vector_inventory_2_Cassette5Feature1","", choices = c(vector_inventory$Cassette5Feature1))

 updateTextInput(session,"vector_inventory_2_Cassette1Feature1Desc","", value = vector_inventory$Cassette1Feature1Desc)
 updateTextInput(session,"vector_inventory_2_Cassette2Feature1Desc","", value = vector_inventory$Cassette2Feature1Desc)
 updateTextInput(session,"vector_inventory_2_Cassette3Feature1Desc","", value = vector_inventory$Cassette3Feature1Desc)
 updateTextInput(session,"vector_inventory_2_Cassette4Feature1Desc","", value = vector_inventory$Cassette4Feature1Desc)
 updateTextInput(session,"vector_inventory_2_Cassette5Feature1Desc","", value = vector_inventory$Cassette5Feature1Desc)

 updateSelectInput(session,"vector_inventory_2_Cassette1Feature2","", choices = c(vector_inventory$Cassette1Feature2))
 updateSelectInput(session,"vector_inventory_2_Cassette2Feature2"," ", choices = c(vector_inventory$Cassette2Feature2))
 updateSelectInput(session,"vector_inventory_2_Cassette3Feature2","", choices = c(vector_inventory$Cassette3Feature2))
 updateSelectInput(session,"vector_inventory_2_Cassette4Feature2"," ", choices = c(vector_inventory$Cassette4Feature2))
 updateSelectInput(session,"vector_inventory_2_Cassette5Feature2","", choices = c(vector_inventory$Cassette5Feature2))

 updateTextInput(session,"vector_inventory_2_Cassette1Feature2Desc","", value = vector_inventory$Cassette1Feature2Desc)
 updateTextInput(session,"vector_inventory_2_Cassette2Feature2Desc","", value = vector_inventory$Cassette2Feature2Desc)
 updateTextInput(session,"vector_inventory_2_Cassette3Feature2Desc","", value = vector_inventory$Cassette3Feature2Desc)
 updateTextInput(session,"vector_inventory_2_Cassette4Feature2Desc","", value = vector_inventory$Cassette4Feature2Desc)
 updateTextInput(session,"vector_inventory_2_Cassette5Feature2Desc","", value = vector_inventory$Cassette5Feature2Desc)

 updateSelectInput(session,"vector_inventory_2_Cassette1Feature3","", choices = c(vector_inventory$Cassette1Feature3))
 updateSelectInput(session,"vector_inventory_2_Cassette2Feature3","", choices = c(vector_inventory$Cassette2Feature3))
 updateSelectInput(session,"vector_inventory_2_Cassette3Feature3","", choices = c(vector_inventory$Cassette3Feature3))
 updateSelectInput(session,"vector_inventory_2_Cassette4Feature3","", choices = c(vector_inventory$Cassette4Feature3))
 updateSelectInput(session,"vector_inventory_2_Cassette5Feature3","", choices = c(vector_inventory$Cassette5Feature3))

 updateTextInput(session,"vector_inventory_2_Cassette1Feature3Desc","", value = vector_inventory$Cassette1Feature3Desc)
 updateTextInput(session,"vector_inventory_2_Cassette2Feature3Desc","", value = vector_inventory$Cassette2Feature3Desc)
 updateTextInput(session,"vector_inventory_2_Cassette3Feature3Desc","", value = vector_inventory$Cassette3Feature3Desc)
 updateTextInput(session,"vector_inventory_2_Cassette4Feature3Desc","", value = vector_inventory$Cassette4Feature3Desc)
 updateTextInput(session,"vector_inventory_2_Cassette5Feature3Desc","", value = vector_inventory$Cassette5Feature3Desc)


  # inventory 3

 updateTextInput(session, "vector_inventory_3_Cassette1ForwardName", "", value = vector_inventory$Cassette1ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette1ForwardCode", "", value = vector_inventory$Cassette1ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette1ForwardSequence", "", value = vector_inventory$Cassette1ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette1ReverseName", "", value = vector_inventory$Cassette1ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette1ReverseCode", "", value = vector_inventory$Cassette1ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette1ReverseSequence", "", value = vector_inventory$Cassette1ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette2ForwardName", "", value = vector_inventory$Cassette2ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette2ForwardCode", "", value = vector_inventory$Cassette2ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette2ForwardSequence", "", value = vector_inventory$Cassette2ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette2ReverseName", "", value = vector_inventory$Cassette2ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette2ReverseCode", "", value = vector_inventory$Cassette2ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette2ReverseSequence", "", value = vector_inventory$Cassette2ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette3ForwardName", "", value = vector_inventory$Cassette3ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette3ForwardCode", "", value = vector_inventory$Cassette3ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette3ForwardSequence", "", value = vector_inventory$Cassette3ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette3ReverseName", "", value = vector_inventory$Cassette3ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette3ReverseCode", "", value = vector_inventory$Cassette3ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette3ReverseSequence", "", value = vector_inventory$Cassette3ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette4ForwardName", "", value = vector_inventory$Cassette4ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette4ForwardCode", "", value = vector_inventory$Cassette4ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette4ForwardSequence", "", value = vector_inventory$Cassette4ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette4ReverseName", "", value = vector_inventory$Cassette4ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette4ReverseCode", "", value = vector_inventory$Cassette4ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette4ReverseSequence", "", value = vector_inventory$Cassette4ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette5ForwardName", "", value = vector_inventory$Cassette5ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette5ForwardCode", "", value = vector_inventory$Cassette5ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette5ForwardSequence", "", value = vector_inventory$Cassette5ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette5ReverseName", "", value = vector_inventory$Cassette5ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette5ReverseCode", "", value = vector_inventory$Cassette5ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette5ReverseSequence", "", value = vector_inventory$Cassette5ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette6ForwardName", "", value = vector_inventory$Cassette6ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette6ForwardCode", "", value = vector_inventory$Cassette6ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette6ForwardSequence", "", value = vector_inventory$Cassette6ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette6ReverseName", "", value = vector_inventory$Cassette6ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette6ReverseCode", "", value = vector_inventory$Cassette6ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette6ReverseSequence", "", value = vector_inventory$Cassette6ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette7ForwardName", "", value = vector_inventory$Cassette7ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette7ForwardCode", "", value = vector_inventory$Cassette7ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette7ForwardSequence", "", value = vector_inventory$Cassette7ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette7ReverseName", "", value = vector_inventory$Cassette7ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette7ReverseCode", "", value = vector_inventory$Cassette7ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette7ReverseSequence", "", value = vector_inventory$Cassette7ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette8ForwardName", "", value = vector_inventory$Cassette8ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette8ForwardCode", "", value = vector_inventory$Cassette8ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette8ForwardSequence", "", value = vector_inventory$Cassette8ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette8ReverseName", "", value = vector_inventory$Cassette8ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette8ReverseCode", "", value = vector_inventory$Cassette8ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette8ReverseSequence", "", value = vector_inventory$Cassette8ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette9ForwardName", "", value = vector_inventory$Cassette9ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette9ForwardCode", "", value = vector_inventory$Cassette9ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette9ForwardSequence", "", value = vector_inventory$Cassette9ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette9ReverseName", "", value = vector_inventory$Cassette9ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette9ReverseCode", "", value = vector_inventory$Cassette9ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette9ReverseSequence", "", value = vector_inventory$Cassette9ReverseSequence)

 updateTextInput(session, "vector_inventory_3_Cassette10ForwardName", "", value = vector_inventory$Cassette10ForwardName)
 updateTextInput(session, "vector_inventory_3_Cassette10ForwardCode", "", value = vector_inventory$Cassette10ForwardCode)
 updateTextInput(session, "vector_inventory_3_Cassette10ForwardSequence", "", value = vector_inventory$Cassette10ForwardSequence)

 updateTextInput(session, "vector_inventory_3_Cassette10ReverseName", "", value = vector_inventory$Cassette10ReverseName)
 updateTextInput(session, "vector_inventory_3_Cassette10ReverseCode", "", value = vector_inventory$Cassette10ReverseCode)
 updateTextInput(session, "vector_inventory_3_Cassette10ReverseSequence", "", value = vector_inventory$Cassette10ReverseSequence)

  # inventory 4

 updateSelectInput(session, "vector_inventory_4_SequencingCompleted", "Sequencing Completed", choices = c(vector_inventory$SequencingCompleted))
 updateDateInput(session, "vector_inventory_4_DateOfSequencing", "Date of sequencing", value = vector_inventory$DateOfSequencing)
 numericInput("vector_inventory_4_SeqPrimersLabBookNumber", "Lab Book Number", value = vector_inventory$SeqPrimersLabBookNumber)
 numericInput("vector_inventory_4_SeqPrimersPageNumber", "Page Number", value = vector_inventory$SeqPrimersPageNumber)
 fileInput("vector_inventory_4_ContigExpressSequencingAlignment", "", )
 fileInput("vector_inventory_4_SequencingFiles", "", multiple = F, placeholder = "Locate")
 updateSelectInput(session, "vector_inventory_4_CheckedBy", "Checked By", choices = c(vector_inventory$CheckedBy))
 updateDateInput(session, "vector_inventory_4_CheckedDate", "Date", value = vector_inventory$CheckedDate)
 updateSelectInput(session, "vector_inventory_4_VerifiedBy", "Verified By", choices = c(vector_inventory$VerifiedBy))
 updateDateInput(session, "vector_inventory_4_VerifiedDate", "Date", value = vector_inventory$VerifiedDate)
 updateSelectInput(session, "vector_inventory_4_TranformedIntoAgro", "Tranformed Into Agro", choices = c(vector_inventory$TranformedIntoAgro))
 updateSelectInput(session, "vector_inventory_4_Strain", "Strain", choices = c(vector_inventory$Strain))
 updateSelectInput(session, "vector_inventory_4_ConfirmedByPCR", "Confirmed by PCR", choices = c(vector_inventory$ConfirmedByPCR))
 updateDateInput(session, "vector_inventory_4_ConfirmedByPCRDate", "Date",  value = lubridate::ymd(vector_inventory$ConfirmedByPCRDate))

  # inventory 5

 updateTextInput(session, "vector_inventory_5_DNAStorageLocation", "", value =  vector_inventory$DNAStorageLocation)
 updateTextInput(session, "vector_inventory_5_EcoliGlycerolStorageLocation", "",  value = vector_inventory$EColiGlycerolStorageLocation)
 updateTextInput(session, "vector_inventory_5_AgroGlycerolStorageLocation", "",  value = vector_inventory$AgroGlycerolStorageLocation)
 updateTextInput(session, "vector_inventory_5_DNAStorageBox", "",  value = vector_inventory$DNAStorageBox)
 updateTextInput(session, "vector_inventory_5_EcoliGlycerolStorageBox", "",  value = vector_inventory$EColiGlycerolStorageBox)
 updateTextInput(session, "vector_inventory_5_AgroGlycerolStorageBox", "", value =  vector_inventory$AgroGlycerolStorageBox)
 updateTextInput(session, "vector_inventory_5_DNAStoredBy", "",  value = vector_inventory$DNAStoredBy)
 updateTextInput(session, "vector_inventory_5_EcoliGlycerolStoredBy", "",  value = vector_inventory$EColiGlycerolStoredBy)
 updateTextInput(session, "vector_inventory_5_AgroGlycerolStoredBy","",  value = vector_inventory$AgroGlycerolStoredBy)
 updateDateInput(session, "vector_inventory_5_DNAStorageDate", "", value = lubridate::ymd(vector_inventory$DNAStorageDate))
 updateDateInput(session, "vector_inventory_5_EcoliGlycerolStorageDate", "", value = lubridate::ymd(vector_inventory$EColiGlycerolStorageDate))
 updateDateInput(session, "vector_inventory_5_AgroGlycerolStorageDate", "", value = lubridate::ymd(vector_inventory$AgroGlycerolStorageDate))

})

# Load data to update -- load to update the record
observeEvent(input$vector_inventory_search_LoadDataToUpdate,{
  shinyjs::hide("vector_inventory_5_SaveThePlantExpressionVectorRecord") # -----------------
  vector_inventory <- vector_inventory()

  dt <- vector_inventory() %>%
    dplyr::filter(trimws(VectorID) == trimws(input$vector_inventory_search_SelectedVectorID)) %>%
    #dplyr::arrange(desc(lubridate::ydm(DateOfCulture))) %>%
    .[1,] # sort in descending order and select the most recent
  # inventory 1
  updateTextInput(session, "vector_inventory_1_VectorID", "Vector ID", value = vector_inventory$VectorID)
  updateTextInput(session, "vector_inventory_1_VectorPrefix", "Vector Prefix", value = vector_inventory$VectorPrefix)
  updateTextInput(session, "vector_inventory_1_VectorCode", "Vector Code", value = vector_inventory$VectorCode)
  updateTextInput(session, "vector_inventory_1_VectorSuffix", "Vector Suffix", value = vector_inventory$VectorSuffix)
  updateSelectInput(session, "vector_inventory_1_BacterialSelection","Bacterial Selection", choices = c('',vector_inventory$BacterialSelection), selected = dt$BacterialSelection)
  updateSelectInput(session, "vector_inventory_1_PlantSelection","Plant Selection", choices = c('',vector_inventory$PlantSelection), selected = dt$PlantSelection)
  updateTextInput(session, "vector_inventory_1_Synonyms1", "Synonyms 1", value = vector_inventory$Synonym1)
  updateTextInput(session, "vector_inventory_1_Synonyms2", "Synonyms 2", value = vector_inventory$Synonym2)
  updateTextInput(session, "vector_inventory_1_Synonyms3", "Synonyms 3", value = vector_inventory$Synonym3)
  updateTextInput(session, "vector_inventory_1_Synonyms4", "Synonyms 4", value = vector_inventory$Synonym4)
  updateTextInput(session, "vector_inventory_1_Synonyms5", "Synonyms 5", value = vector_inventory$Synonym5)
  updateSelectInput(session, "vector_inventory_1_Backbone","", choices = c('',vector_inventory$Backbone), selected = dt$Backbone)
  updateSelectInput(session, "vector_inventory_1_ClonedBy","", choices = c('',vector_inventory$ClonedBy), selected = dt$ClonedBy)
  updateDateInput(session, "vector_inventory_1_DateOfCloning","", value = vector_inventory$ClonedDate)
  updateNumericInput(session, "vector_inventory_1_LabBookNumber", "", value = vector_inventory$LabBookNumber)
  updateNumericInput(session, "vector_inventory_1_PageNumber", "", value = vector_inventory$PageNumber)

  # inventory 2

  updateSelectInput(session,"vector_inventory_2_Cassette1Promoter","", choices = c('',vector_inventory$Cassette1Promoter), selected = dt$Cassette1Promoter)
  updateSelectInput(session,"vector_inventory_2_Cassette2Promoter","", choices = c('',vector_inventory$Cassette2Promoter), selected = dt$Cassette2Promoter)
  updateSelectInput(session,"vector_inventory_2_Cassette3Promoter","", choices = c('',vector_inventory$Cassette3Promoter), selected = dt$Cassette3Promoter)
  updateSelectInput(session,"vector_inventory_2_Cassette4Promoter","", choices = c('',vector_inventory$Cassette4Promoter), selected = dt$Cassette4Promoter)
  updateSelectInput(session,"vector_inventory_2_Cassette5Promoter","", choices = c('',vector_inventory$Cassette5Promoter), selected = dt$Cassette5Promoter)

  updateSelectInput(session,"vector_inventory_2_Cassette1Gene","", choices =  c('',vector_inventory$Cassette1Gene), selected = dt$Cassette1Gene)
  updateSelectInput(session,"vector_inventory_2_Cassette2Gene","", choices =  c('',vector_inventory$Cassette2Gene), selected = dt$Cassette2Gene)
  updateSelectInput(session,"vector_inventory_2_Cassette3Gene","", choices =  c('',vector_inventory$Cassette3Gene), selected = dt$Cassette3Gene)
  updateSelectInput(session,"vector_inventory_2_Cassette4Gene","", choices =  c('',vector_inventory$Cassette4Gene), selected = dt$Cassette4Gene)
  updateSelectInput(session,"vector_inventory_2_Cassette5Gene","", choices =  c('',vector_inventory$Cassette5Gene), selected = dt$Cassette5Gene)

  updateSelectInput(session,"vector_inventory_2_Cassette1Terminator","", choices = c('',vector_inventory$Cassette1Terminator), selected = dt$Cassette1Terminator)
  updateSelectInput(session,"vector_inventory_2_Cassette2Terminator","", choices = c('',vector_inventory$Cassette2Terminator), selected = dt$Cassette2Terminator)
  updateSelectInput(session,"vector_inventory_2_Cassette3Terminator","", choices = c('',vector_inventory$Cassette3Terminator), selected = dt$Cassette3Terminator)
  updateSelectInput(session,"vector_inventory_2_Cassette4Terminator","", choices = c('',vector_inventory$Cassette4Terminator), selected = dt$Cassette4Terminator)
  updateSelectInput(session,"vector_inventory_2_Cassette5Terminator","", choices = c('',vector_inventory$Cassette5Terminator), selected = dt$Cassette5Terminator)

  updateSelectInput(session,"vector_inventory_2_Cassette1Feature1","", choices = c('',vector_inventory$Cassette1Feature1), selected = dt$Cassette1Feature1)
  updateSelectInput(session,"vector_inventory_2_Cassette2Feature1","", choices = c('',vector_inventory$Cassette2Feature1), selected = dt$Cassette2Feature1)
  updateSelectInput(session,"vector_inventory_2_Cassette3Feature1","", choices = c('', vector_inventory$Cassette3Feature1), selected = dt$Cassette3Feature1)
  updateSelectInput(session,"vector_inventory_2_Cassette4Feature1","", choices = c('',vector_inventory$Cassette4Feature1), selected = dt$Cassette4Feature1)
  updateSelectInput(session,"vector_inventory_2_Cassette5Feature1","", choices = c('',vector_inventory$Cassette5Feature1), selected = dt$Cassette5Feature1)

  updateTextInput(session,"vector_inventory_2_Cassette1Feature1Desc","", value = vector_inventory$Cassette1Feature1Desc)
  updateTextInput(session,"vector_inventory_2_Cassette2Feature1Desc","", value = vector_inventory$Cassette2Feature1Desc)
  updateTextInput(session,"vector_inventory_2_Cassette3Feature1Desc","", value = vector_inventory$Cassette3Feature1Desc)
  updateTextInput(session,"vector_inventory_2_Cassette4Feature1Desc","", value = vector_inventory$Cassette4Feature1Desc)
  updateTextInput(session,"vector_inventory_2_Cassette5Feature1Desc","", value = vector_inventory$Cassette5Feature1Desc)

  updateSelectInput(session,"vector_inventory_2_Cassette1Feature2","", choices = c('',vector_inventory$Cassette1Feature2), selected = dt$Cassette1Feature2)
  updateSelectInput(session,"vector_inventory_2_Cassette2Feature2"," ", choices = c('',vector_inventory$Cassette2Feature2), selected = dt$Cassette2Feature2)
  updateSelectInput(session,"vector_inventory_2_Cassette3Feature2","", choices = c('',vector_inventory$Cassette3Feature2), selected = dt$Cassette3Feature2)
  updateSelectInput(session,"vector_inventory_2_Cassette4Feature2"," ", choices = c('',vector_inventory$Cassette4Feature2), selected = dt$Cassette4Feature2)
  updateSelectInput(session,"vector_inventory_2_Cassette5Feature2","", choices = c('',vector_inventory$Cassette5Feature2), selected = dt$Cassette5Feature2)

  updateTextInput(session,"vector_inventory_2_Cassette1Feature2Desc","", value = vector_inventory$Cassette1Feature2Desc)
  updateTextInput(session,"vector_inventory_2_Cassette2Feature2Desc","", value = vector_inventory$Cassette2Feature2Desc)
  updateTextInput(session,"vector_inventory_2_Cassette3Feature2Desc","", value = vector_inventory$Cassette3Feature2Desc)
  updateTextInput(session,"vector_inventory_2_Cassette4Feature2Desc","", value = vector_inventory$Cassette4Feature2Desc)
  updateTextInput(session,"vector_inventory_2_Cassette5Feature2Desc","", value = vector_inventory$Cassette5Feature2Desc)

  updateSelectInput(session,"vector_inventory_2_Cassette1Feature3","", choices = c('',vector_inventory$Cassette1Feature3), selected = dt$Cassette1Feature3)
  updateSelectInput(session,"vector_inventory_2_Cassette2Feature3","", choices = c('',vector_inventory$Cassette2Feature3), selected = dt$Cassette2Feature3)
  updateSelectInput(session,"vector_inventory_2_Cassette3Feature3","", choices = c('',vector_inventory$Cassette3Feature3), selected = dt$Cassette3Feature3)
  updateSelectInput(session,"vector_inventory_2_Cassette4Feature3","", choices = c('',vector_inventory$Cassette4Feature3), selected = dt$Cassette4Feature3)
  updateSelectInput(session,"vector_inventory_2_Cassette5Feature3","", choices = c('',vector_inventory$Cassette5Feature3), selected = dt$Cassette5Feature3)

  updateTextInput(session,"vector_inventory_2_Cassette1Feature3Desc","", value = vector_inventory$Cassette1Feature3Desc)
  updateTextInput(session,"vector_inventory_2_Cassette2Feature3Desc","", value = vector_inventory$Cassette2Feature3Desc)
  updateTextInput(session,"vector_inventory_2_Cassette3Feature3Desc","", value = vector_inventory$Cassette3Feature3Desc)
  updateTextInput(session,"vector_inventory_2_Cassette4Feature3Desc","", value = vector_inventory$Cassette4Feature3Desc)
  updateTextInput(session,"vector_inventory_2_Cassette5Feature3Desc","", value = vector_inventory$Cassette5Feature3Desc)


  # inventory 3

  updateTextInput(session, "vector_inventory_3_Cassette1ForwardName", "", value = vector_inventory$Cassette1ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette1ForwardCode", "", value = vector_inventory$Cassette1ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette1ForwardSequence", "", value = vector_inventory$Cassette1ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette1ReverseName", "", value = vector_inventory$Cassette1ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette1ReverseCode", "", value = vector_inventory$Cassette1ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette1ReverseSequence", "", value = vector_inventory$Cassette1ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette2ForwardName", "", value = vector_inventory$Cassette2ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette2ForwardCode", "", value = vector_inventory$Cassette2ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette2ForwardSequence", "", value = vector_inventory$Cassette2ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette2ReverseName", "", value = vector_inventory$Cassette2ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette2ReverseCode", "", value = vector_inventory$Cassette2ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette2ReverseSequence", "", value = vector_inventory$Cassette2ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette3ForwardName", "", value = vector_inventory$Cassette3ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette3ForwardCode", "", value = vector_inventory$Cassette3ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette3ForwardSequence", "", value = vector_inventory$Cassette3ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette3ReverseName", "", value = vector_inventory$Cassette3ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette3ReverseCode", "", value = vector_inventory$Cassette3ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette3ReverseSequence", "", value = vector_inventory$Cassette3ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette4ForwardName", "", value = vector_inventory$Cassette4ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette4ForwardCode", "", value = vector_inventory$Cassette4ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette4ForwardSequence", "", value = vector_inventory$Cassette4ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette4ReverseName", "", value = vector_inventory$Cassette4ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette4ReverseCode", "", value = vector_inventory$Cassette4ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette4ReverseSequence", "", value = vector_inventory$Cassette4ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette5ForwardName", "", value = vector_inventory$Cassette5ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette5ForwardCode", "", value = vector_inventory$Cassette5ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette5ForwardSequence", "", value = vector_inventory$Cassette5ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette5ReverseName", "", value = vector_inventory$Cassette5ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette5ReverseCode", "", value = vector_inventory$Cassette5ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette5ReverseSequence", "", value = vector_inventory$Cassette5ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette6ForwardName", "", value = vector_inventory$Cassette6ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette6ForwardCode", "", value = vector_inventory$Cassette6ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette6ForwardSequence", "", value = vector_inventory$Cassette6ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette6ReverseName", "", value = vector_inventory$Cassette6ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette6ReverseCode", "", value = vector_inventory$Cassette6ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette6ReverseSequence", "", value = vector_inventory$Cassette6ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette7ForwardName", "", value = vector_inventory$Cassette7ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette7ForwardCode", "", value = vector_inventory$Cassette7ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette7ForwardSequence", "", value = vector_inventory$Cassette7ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette7ReverseName", "", value = vector_inventory$Cassette7ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette7ReverseCode", "", value = vector_inventory$Cassette7ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette7ReverseSequence", "", value = vector_inventory$Cassette7ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette8ForwardName", "", value = vector_inventory$Cassette8ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette8ForwardCode", "", value = vector_inventory$Cassette8ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette8ForwardSequence", "", value = vector_inventory$Cassette8ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette8ReverseName", "", value = vector_inventory$Cassette8ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette8ReverseCode", "", value = vector_inventory$Cassette8ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette8ReverseSequence", "", value = vector_inventory$Cassette8ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette9ForwardName", "", value = vector_inventory$Cassette9ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette9ForwardCode", "", value = vector_inventory$Cassette9ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette9ForwardSequence", "", value = vector_inventory$Cassette9ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette9ReverseName", "", value = vector_inventory$Cassette9ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette9ReverseCode", "", value = vector_inventory$Cassette9ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette9ReverseSequence", "", value = vector_inventory$Cassette9ReverseSequence)

  updateTextInput(session, "vector_inventory_3_Cassette10ForwardName", "", value = vector_inventory$Cassette10ForwardName)
  updateTextInput(session, "vector_inventory_3_Cassette10ForwardCode", "", value = vector_inventory$Cassette10ForwardCode)
  updateTextInput(session, "vector_inventory_3_Cassette10ForwardSequence", "", value = vector_inventory$Cassette10ForwardSequence)

  updateTextInput(session, "vector_inventory_3_Cassette10ReverseName", "", value = vector_inventory$Cassette10ReverseName)
  updateTextInput(session, "vector_inventory_3_Cassette10ReverseCode", "", value = vector_inventory$Cassette10ReverseCode)
  updateTextInput(session, "vector_inventory_3_Cassette10ReverseSequence", "", value = vector_inventory$Cassette10ReverseSequence)

  # inventory 4

  updateSelectInput(session, "vector_inventory_4_SequencingCompleted", "Sequencing Completed", choices = c('',vector_inventory$SequencingCompleted), selected = dt$SequencingCompleted)
  updateDateInput(session, "vector_inventory_4_DateOfSequencing", "Date of sequencing", value = vector_inventory$DateOfSequencing)
  updateNumericInput(session, "vector_inventory_4_SeqPrimersLabBookNumber", "Lab Book Number", value = vector_inventory$SeqPrimersLabBookNumber)
  updateNumericInput(session,"vector_inventory_4_SeqPrimersPageNumber", "Page Number", value = vector_inventory$SeqPrimersPageNumber)
  # fileInput("vector_inventory_4_ContigExpressSequencingAlignment", "")
  # fileInput("vector_inventory_4_SequencingFiles", "", multiple = F, placeholder = "Locate")
  updateSelectInput(session, "vector_inventory_4_CheckedBy", "Checked By", choices = c('',vector_inventory$CheckedBy), selected = dt$CheckedBy)
  updateDateInput(session, "vector_inventory_4_CheckedDate", "Date", value = lubridate::ymd(vector_inventory$CheckedDate))
  updateSelectInput(session, "vector_inventory_4_VerifiedBy", "Verified By", choices = c('',vector_inventory$VerifiedBy), selected = dt$VerifiedBy)
  updateDateInput(session, "vector_inventory_4_VerifiedDate", "Date", value = lubridate::ymd(vector_inventory$VerifiedDate))
  updateSelectInput(session, "vector_inventory_4_TranformedIntoAgro", "Tranformed Into Agro", choices = c('',vector_inventory$TranformedIntoAgro), selected = dt$TranformedIntoAgro)
  updateSelectInput(session, "vector_inventory_4_Strain", "Strain", choices = c('',vector_inventory$Strain), selected = dt$Strain)
  updateSelectInput(session, "vector_inventory_4_ConfirmedByPCR", "Confirmed by PCR", choices = c('',vector_inventory$ConfirmedByPCR), selected = dt$ConfirmedByPCR)
  updateDateInput(session, "vector_inventory_4_ConfirmedByPCRDate", "Date",  value = lubridate::ymd(vector_inventory$ConfirmedByPCRDate))

  # inventory 5

  updateTextInput(session, "vector_inventory_5_DNAStorageLocation", "", value =  vector_inventory$DNAStorageLocation)
  updateTextInput(session, "vector_inventory_5_EcoliGlycerolStorageLocation", "",  value = vector_inventory$EColiGlycerolStorageLocation)
  updateTextInput(session, "vector_inventory_5_AgroGlycerolStorageLocation", "",  value = vector_inventory$AgroGlycerolStorageLocation)
  updateTextInput(session, "vector_inventory_5_DNAStorageBox", "",  value = vector_inventory$DNAStorageBox)
  updateTextInput(session, "vector_inventory_5_EcoliGlycerolStorageBox", "",  value = vector_inventory$EColiGlycerolStorageBox)
  updateTextInput(session, "vector_inventory_5_AgroGlycerolStorageBox", "", value =  vector_inventory$AgroGlycerolStorageBox)
  updateTextInput(session, "vector_inventory_5_DNAStoredBy", "",  value = vector_inventory$DNAStoredBy)
  updateTextInput(session, "vector_inventory_5_EcoliGlycerolStoredBy", "",  value = vector_inventory$EColiGlycerolStoredBy)
  updateTextInput(session, "vector_inventory_5_AgroGlycerolStoredBy","",  value = vector_inventory$AgroGlycerolStoredBy)
  updateDateInput(session, "vector_inventory_5_DNAStorageDate", "", value = lubridate::ymd(vector_inventory$DNAStorageDate))
  updateDateInput(session, "vector_inventory_5_EcoliGlycerolStorageDate", "", value = lubridate::ymd(vector_inventory$EColiGlycerolStorageDate))
  updateDateInput(session, "vector_inventory_5_AgroGlycerolStorageDate", "", value = lubridate::ymd(vector_inventory$AgroGlycerolStorageDate))
})

## ------------------------------------------------------------------------ Save Update

observeEvent(input$vector_inventory_5_UpdateThePlantExpressionVectorRecord,{
  
  VectorID = paste0(input$vector_inventory_1_VectorPrefix,"-", input$vector_inventory_1_VectorCode, "-", input$vector_inventory_1_VectorSuffix)
  VectorPrefix = input$vector_inventory_1_VectorPrefix
  VectorCode = input$vector_inventory_1_VectorCode
  VectorSuffix = as.integer(nrow(dt)+1)
  BacterialSelection = ifelse(nchar(input$vector_inventory_1_BacterialSelection)>0, input$vector_inventory_1_BacterialSelection,'')
  PlantSelection = ifelse(nchar(input$vector_inventory_1_PlantSelection)>0, input$vector_inventory_1_PlantSelection,'')
  Synonym1 = ifelse(nchar(input$vector_inventory_1_Synonym1)>0, input$vector_inventory_1_Synonym1,'')
  Synonym2 = ifelse(nchar(input$vector_inventory_1_Synonym2)>0, input$vector_inventory_1_Synonym2,'')
  Synonym3 = ifelse(nchar(input$vector_inventory_1_Synonym3)>0, input$vector_inventory_1_Synonym3,'')
  Synonym4 = ifelse(nchar(input$vector_inventory_1_Synonym4)>0, input$vector_inventory_1_Synonym4,'')
  Synonym5 = ifelse(nchar(input$vector_inventory_1_Synonym5)>0, input$vector_inventory_1_Synonym5,'')
  Backbone = ifelse(nchar(input$vector_inventory_1_Backbone)>0, input$vector_inventory_1_Backbone,'')
  Cassette1Promoter = ifelse(nchar(input$vector_inventory_2_Cassette1Promoter)>0, input$vector_inventory_2_Cassette1Promoter,'')
  Cassette1Gene = ifelse(nchar(input$vector_inventory_2_Cassette1Gene)>0, input$vector_inventory_2_Cassette1Gene,'')
  Cassette1Terminator = ifelse(nchar(input$vector_inventory_2_Cassette1Terminator)>0, input$vector_inventory_2_Cassette1Terminator,'')
  Cassette1Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette1Feature1)>0, input$vector_inventory_2_Cassette1Feature1,'')
  Cassette1Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette1Feature1Desc)>0, input$vector_inventory_2_Cassette1Feature1Desc,'')
  Cassette1Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette1Feature2)>0, input$vector_inventory_2_Cassette1Feature2,'')
  Cassette1Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette1Feature2Desc)>0, input$vector_inventory_2_Cassette1Feature2Desc,'')
  Cassette1Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette1Feature3)>0, input$vector_inventory_2_Cassette1Feature3,'')
  Cassette1Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette1Feature3Desc)>0, input$vector_inventory_2_Cassette1Feature3Desc,'')
  Cassette2Promoter = ifelse(nchar(input$vector_inventory_2_Cassette2Promoter)>0, input$vector_inventory_2_Cassette2Promoter,'')
  Cassette2Gene = ifelse(nchar(input$vector_inventory_2_Cassette2Gene)>0, input$vector_inventory_2_Cassette2Gene,'')
  Cassette2Terminator = ifelse(nchar(input$vector_inventory_2_Cassette2Terminator)>0, input$vector_inventory_2_Cassette2Terminator,'')
  Cassette2Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette2Feature1)>0, input$vector_inventory_2_Cassette2Feature1,'')
  Cassette2Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette2Feature1Desc)>0, input$vector_inventory_2_Cassette2Feature1Desc,'')
  Cassette2Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette2Feature2)>0, input$vector_inventory_2_Cassette2Feature2,'')
  Cassette2Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette2Feature2Desc)>0, input$vector_inventory_2_Cassette2Feature2Desc,'')
  Cassette2Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette2Feature3)>0, input$vector_inventory_2_Cassette2Feature3,'')
  Cassette2Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette2Feature3Desc)>0, input$vector_inventory_2_Cassette2Feature3Desc,'')
  Cassette3Promoter = ifelse(nchar(input$vector_inventory_2_Cassette3Promoter)>0, input$vector_inventory_2_Cassette3Promoter,'')
  Cassette3Gene = ifelse(nchar(input$vector_inventory_2_Cassette3Gene)>0, input$vector_inventory_2_Cassette3Gene,'')
  Cassette3Terminator = ifelse(nchar(input$vector_inventory_2_Cassette3Terminator)>0, input$vector_inventory_2_Cassette3Terminator,'')
  Cassette3Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette3Feature1)>0, input$vector_inventory_2_Cassette3Feature1,'')
  Cassette3Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette3Feature1Desc)>0, input$vector_inventory_2_Cassette3Feature1Desc,'')
  Cassette3Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette3Feature2)>0, input$vector_inventory_2_Cassette3Feature2,'')
  Cassette3Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette3Feature2Desc)>0, input$vector_inventory_2_Cassette3Feature2Desc,'')
  Cassette3Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette3Feature3)>0, input$vector_inventory_2_Cassette3Feature3,'')
  Cassette3Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette3Feature3Desc)>0, input$vector_inventory_2_Cassette3Feature3Desc,'')
  Cassette4Promoter = ifelse(nchar(input$vector_inventory_2_Cassette4Promoter)>0, input$vector_inventory_2_Cassette4Promoter,'')
  Cassette4Gene = ifelse(nchar(input$vector_inventory_2_Cassette4Gene)>0, input$vector_inventory_2_Cassette4Gene,'')
  Cassette4Terminator = ifelse(nchar(input$vector_inventory_2_Cassette4Terminator)>0, input$vector_inventory_2_Cassette4Terminator,'')
  Cassette4Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette4Feature1)>0, input$vector_inventory_2_Cassette4Feature1,'')
  Cassette4Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette4Feature1Desc)>0, input$vector_inventory_2_Cassette4Feature1Desc,'')
  Cassette4Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette4Feature2)>0, input$vector_inventory_2_Cassette4Feature2,'')
  Cassette4Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette4Feature2Desc)>0, input$vector_inventory_2_Cassette4Feature2Desc,'')
  Cassette4Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette4Feature3)>0, input$vector_inventory_2_Cassette4Feature3,'')
  Cassette4Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette4Feature3Desc)>0, input$vector_inventory_2_Cassette4Feature3Desc,'')
  Cassette5Promoter = ifelse(nchar(input$vector_inventory_2_Cassette5Promoter)>0, input$vector_inventory_2_Cassette5Promoter,'')
  Cassette5Gene = ifelse(nchar(input$vector_inventory_2_Cassette5Gene)>0, input$vector_inventory_2_Cassette5Gene,'')
  Cassette5Terminator = ifelse(nchar(input$vector_inventory_2_Cassette5Terminator)>0, input$vector_inventory_2_Cassette5Terminator,'')
  Cassette5Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette5Feature1)>0, input$vector_inventory_2_Cassette5Feature1,'')
  Cassette5Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette5Feature1Desc)>0, input$vector_inventory_2_Cassette5Feature1Desc,'')
  Cassette5Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette5Feature2)>0, input$vector_inventory_2_Cassette5Feature2,'')
  Cassette5Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette5Feature2Desc)>0, input$vector_inventory_2_Cassette5Feature2Desc,'')
  Cassette5Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette5Feature3)>0, input$vector_inventory_2_Cassette5Feature3,'')
  Cassette5Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette5Feature3Desc)>0, input$vector_inventory_2_Cassette5Feature3Desc,'')
  VNTI_Map_Location = ifelse(!is.null(input$vector_inventory_1_VNTILocate), input$vector_inventory_1_VNTILocate,'')
  ClonedBy = ifelse(nchar(input$vector_inventory_1_ClonedBy)>0, input$vector_inventory_1_ClonedBy,'')
  ClonedDate = as.Date(input$vector_inventory_1_DateOfCloning, origin="1970-01-01")
  LabBookNumber = input$vector_inventory_1_LabBookNumber
  PageNumber = input$vector_inventory_1_PageNumber
  Cassette1ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette1ForwardName)>0, input$vector_inventory_3_Cassette1ForwardName,'')
  Cassette1ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette1ForwardCode)>0, input$vector_inventory_3_Cassette1ForwardCode,'')
  Cassette1ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette1ForwardSequence)>0, input$vector_inventory_3_Cassette1ForwardSequence,'')
  Cassette1ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette1ReverseName)>0, input$vector_inventory_3_Cassette1ReverseName,'')
  Cassette1ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette1ReverseCode)>0, input$vector_inventory_3_Cassette1ReverseCode,'')
  Cassette1ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette1ReverseSequence)>0, input$vector_inventory_3_Cassette1ReverseSequence,'')
  Cassette2ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette2ForwardName)>0, input$vector_inventory_3_Cassette2ForwardName,'')
  Cassette2ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette2ForwardCode)>0, input$vector_inventory_3_Cassette2ForwardCode,'')
  Cassette2ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette2ForwardSequence)>0, input$vector_inventory_3_Cassette2ForwardSequence,'')
  Cassette2ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette2ReverseName)>0, input$vector_inventory_3_Cassette2ReverseName,'')
  Cassette2ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette2ReverseCode)>0, input$vector_inventory_3_Cassette2ReverseCode,'')
  Cassette2ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette2ReverseSequence)>0, input$vector_inventory_3_Cassette2ReverseSequence,'')
  Cassette3ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette3ForwardName)>0, input$vector_inventory_3_Cassette3ForwardName,'')
  Cassette3ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette3ForwardCode)>0, input$vector_inventory_3_Cassette3ForwardCode,'')
  Cassette3ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette3ForwardSequence)>0, input$vector_inventory_3_Cassette3ForwardSequence,'')
  Cassette3ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette3ReverseName)>0, input$vector_inventory_3_Cassette3ReverseName,'')
  Cassette3ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette3ReverseCode)>0, input$vector_inventory_3_Cassette3ReverseCode,'')
  Cassette3ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette3ReverseSequence)>0, input$vector_inventory_3_Cassette3ReverseSequence,'')
  Cassette4ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette4ForwardName)>0, input$vector_inventory_3_Cassette4ForwardName,'')
  Cassette4ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette4ForwardCode)>0, input$vector_inventory_3_Cassette4ForwardCode,'')
  Cassette4ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette4ForwardSequence)>0, input$vector_inventory_3_Cassette4ForwardSequence,'')
  Cassette4ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette4ReverseName)>0, input$vector_inventory_3_Cassette4ReverseName,'')
  Cassette4ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette4ReverseCode)>0, input$vector_inventory_3_Cassette4ReverseCode,'')
  Cassette4ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette4ReverseSequence)>0, input$vector_inventory_3_Cassette4ReverseSequence,'')
  Cassette5ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette5ForwardName)>0, input$vector_inventory_3_Cassette5ForwardName,'')
  Cassette5ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette5ForwardCode)>0, input$vector_inventory_3_Cassette5ForwardCode,'')
  Cassette5ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette5ForwardSequence)>0, input$vector_inventory_3_Cassette5ForwardSequence,'')
  Cassette5ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette5ReverseName)>0, input$vector_inventory_3_Cassette5ReverseName,'')
  Cassette5ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette5ReverseCode)>0, input$vector_inventory_3_Cassette5ReverseCode,'')
  Cassette5ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette5ReverseSequence)>0, input$vector_inventory_3_Cassette5ReverseSequence,'')
  SequencingPrimers = ifelse(nchar(input$vector_inventory_4_SequencingPrimers)>0, input$vector_inventory_4_SequencingPrimers,'')
  SequencingCompleted = ifelse(nchar(input$vector_inventory_4_SequencingCompleted)>0, input$vector_inventory_4_SequencingCompleted,'')
  DateOfSequencing = as.Date(input$vector_inventory_4_DateOfSequencing, origin="1970-01-01")
  SeqPrimersLabBookNumber = input$vector_inventory_4_SeqPrimersLabBookNumber
  SeqPrimersPageNumber = input$vector_inventory_4_SeqPrimersPageNumber
  ContigExpressSequencingAlignment = ifelse(!is.null(input$vector_inventory_4_ContigExpressSequencingAlignment), input$vector_inventory_4_ContigExpressSequencingAlignment,'')
  SequencingFiles = ifelse(!is.null(input$vector_inventory_4_SequencingFiles), input$vector_inventory_4_SequencingFiles,'')
  CheckedBy = ifelse(nchar(input$vector_inventory_4_CheckedBy)>0, input$vector_inventory_4_CheckedBy,'')
  CheckedDate = as.Date(input$vector_inventory_4_CheckedDate, origin="1970-01-01")
  VerifiedBy = ifelse(nchar(input$vector_inventory_4_VerifiedBy)>0, input$vector_inventory_4_VerifiedBy,'')
  VerifiedDate = as.Date(input$vector_inventory_4_VerifiedDate, origin="1970-01-01")
  TranformedIntoAgro = ifelse(nchar(input$vector_inventory_4_TranformedIntoAgro)>0, input$vector_inventory_4_TranformedIntoAgro,'')
  Strain = ifelse(nchar(input$vector_inventory_4_Strain)>0, input$vector_inventory_4_Strain,'')
  ConfirmedByPCR = ifelse(nchar(input$vector_inventory_4_ConfirmedByPCR)>0, input$vector_inventory_4_ConfirmedByPCR,'')
  ConfirmedByPCRDate = as.Date(input$vector_inventory_4_ConfirmedByPCRDate, origin="1970-01-01")
  DNAStorageLocation = ifelse(nchar(input$vector_inventory_5_DNAStorageLocation)>0, input$vector_inventory_5_DNAStorageLocation,'')
  DNAStorageBox = ifelse(nchar(input$vector_inventory_5_DNAStorageBox)>0, input$vector_inventory_5_DNAStorageBox,'')
  DNAStoredBy = ifelse(nchar(input$vector_inventory_5_DNAStoredBy)>0, input$vector_inventory_5_DNAStoredBy,'')
  DNAStorageDate = as.Date(input$vector_inventory_5_DNAStorageDate, origin="1970-01-01")
  EColiGlycerolStorageLocation = ifelse(nchar(input$vector_inventory_5_EcoliGlycerolStorageLocation)>0, input$vector_inventory_5_EcoliGlycerolStorageLocation,'')
  EColiGlycerolStorageBox = ifelse(nchar(input$vector_inventory_5_EcoliGlycerolStorageBox)>0, input$vector_inventory_5_EcoliGlycerolStorageBox,'')
  EColiGlycerolStoredBy = ifelse(nchar(input$vector_inventory_5_EcoliGlycerolStoredBy)>0, input$vector_inventory_5_EcoliGlycerolStoredBy,'')
  EColiGlycerolStorageDate = as.Date(input$vector_inventory_5_EcoliGlycerolStorageDate, origin="1970-01-01")
  AgroGlycerolStorageLocation = ifelse(nchar(input$vector_inventory_5_AgroGlycerolStorageLocation)>0, input$vector_inventory_5_AgroGlycerolStorageLocation,'')
  AgroGlycerolStorageBox = ifelse(nchar(input$vector_inventory_5_AgroGlycerolStorageBox)>0, input$vector_inventory_5_AgroGlycerolStorageBox,'')
  AgroGlycerolStoredBy = ifelse(nchar(input$vector_inventory_5_AgroGlycerolStoredBy)>0, input$vector_inventory_5_AgroGlycerolStoredBy,'')
  AgroGlycerolStorageDate = as.Date(input$vector_inventory_5_AgroGlycerolStorageDate, origin="1970-01-01")
  Cassette6Promoter = ifelse(nchar(input$vector_inventory_2_Cassette6Promoter)>0, input$vector_inventory_2_Cassette6Promoter,'')
  Cassette6Gene = ifelse(nchar(input$vector_inventory_2_Cassette6Gene)>0, input$vector_inventory_2_Cassette6Gene,'')
  Cassette6Terminator = ifelse(nchar(input$vector_inventory_2_Cassette6Terminator)>0, input$vector_inventory_2_Cassette6Terminator,'')
  Cassette6Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette6Feature1)>0, input$vector_inventory_2_Cassette6Feature1,'')
  Cassette6Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette6Feature1Desc)>0, input$vector_inventory_2_Cassette6Feature1Desc,'')
  Cassette6Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette6Feature2)>0, input$vector_inventory_2_Cassette6Feature2,'')
  Cassette6Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette6Feature2Desc)>0, input$vector_inventory_2_Cassette6Feature2Desc,'')
  Cassette6Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette6Feature3)>0, input$vector_inventory_2_Cassette6Feature3,'')
  Cassette6Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette6Feature3Desc)>0, input$vector_inventory_2_Cassette6Feature3Desc,'')
  Cassette7Promoter = ifelse(nchar(input$vector_inventory_2_Cassette7Promoter)>0, input$vector_inventory_2_Cassette7Promoter,'')
  Cassette7Gene = ifelse(nchar(input$vector_inventory_2_Cassette7Gene)>0, input$vector_inventory_2_Cassette7Gene,'')
  Cassette7Terminator = ifelse(nchar(input$vector_inventory_2_Cassette7Terminator)>0, input$vector_inventory_2_Cassette7Terminator,'')
  Cassette7Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette7Feature1)>0, input$vector_inventory_2_Cassette7Feature1,'')
  Cassette7Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette7Feature1Desc)>0, input$vector_inventory_2_Cassette7Feature1Desc,'')
  Cassette7Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette7Feature2)>0, input$vector_inventory_2_Cassette7Feature2,'')
  Cassette7Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette7Feature2Desc)>0, input$vector_inventory_2_Cassette7Feature2Desc,'')
  Cassette7Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette7Feature3)>0, input$vector_inventory_2_Cassette7Feature3,'')
  Cassette7Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette7Feature3Desc)>0, input$vector_inventory_2_Cassette7Feature3Desc,'')
  Cassette8Promoter = ifelse(nchar(input$vector_inventory_2_Cassette8Promoter)>0, input$vector_inventory_2_Cassette8Promoter,'')
  Cassette8Gene = ifelse(nchar(input$vector_inventory_2_Cassette8Gene)>0, input$vector_inventory_2_Cassette8Gene,'')
  Cassette8Terminator = ifelse(nchar(input$vector_inventory_2_Cassette8Terminator)>0, input$vector_inventory_2_Cassette8Terminator,'')
  Cassette8Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette8Feature1)>0, input$vector_inventory_2_Cassette8Feature1,'')
  Cassette8Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette8Feature1Desc)>0, input$vector_inventory_2_Cassette8Feature1Desc,'')
  Cassette8Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette8Feature2)>0, input$vector_inventory_2_Cassette8Feature2,'')
  Cassette8Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette8Feature2Desc)>0, input$vector_inventory_2_Cassette8Feature2Desc,'')
  Cassette8Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette8Feature3)>0, input$vector_inventory_2_Cassette8Feature3,'')
  Cassette8Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette8Feature3Desc)>0, input$vector_inventory_2_Cassette8Feature3Desc,'')
  Cassette9Promoter = ifelse(nchar(input$vector_inventory_2_Cassette9Promoter)>0, input$vector_inventory_2_Cassette9Promoter,'')
  Cassette9Gene = ifelse(nchar(input$vector_inventory_2_Cassette9Gene)>0, input$vector_inventory_2_Cassette9Gene,'')
  Cassette9Terminator = ifelse(nchar(input$vector_inventory_2_Cassette9Terminator)>0, input$vector_inventory_2_Cassette9Terminator,'')
  Cassette9Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette9Feature1)>0, input$vector_inventory_2_Cassette9Feature1,'')
  Cassette9Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette9Feature1Desc)>0, input$vector_inventory_2_Cassette9Feature1Desc,'')
  Cassette9Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette9Feature2)>0, input$vector_inventory_2_Cassette9Feature2,'')
  Cassette9Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette9Feature2Desc)>0, input$vector_inventory_2_Cassette9Feature2Desc,'')
  Cassette9Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette9Feature3)>0, input$vector_inventory_2_Cassette9Feature3,'')
  Cassette9Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette9Feature3Desc)>0, input$vector_inventory_2_Cassette9Feature3Desc,'')
  Cassette10Promoter = ifelse(nchar(input$vector_inventory_2_Cassette10Promoter)>0, input$vector_inventory_2_Cassette10Promoter,'')
  Cassette10Gene = ifelse(nchar(input$vector_inventory_2_Cassette10Gene)>0, input$vector_inventory_2_Cassette10Gene,'')
  Cassette10Terminator = ifelse(nchar(input$vector_inventory_2_Cassette10Terminator)>0, input$vector_inventory_2_Cassette10Terminator,'')
  Cassette10Feature1 = ifelse(nchar(input$vector_inventory_2_Cassette10Feature1)>0, input$vector_inventory_2_Cassette10Feature1,'')
  Cassette10Feature1Desc = ifelse(nchar(input$vector_inventory_2_Cassette10Feature1Desc)>0, input$vector_inventory_2_Cassette10Feature1Desc,'')
  Cassette10Feature2 = ifelse(nchar(input$vector_inventory_2_Cassette10Feature2)>0, input$vector_inventory_2_Cassette10Feature2,'')
  Cassette10Feature2Desc = ifelse(nchar(input$vector_inventory_2_Cassette10Feature2Desc)>0, input$vector_inventory_2_Cassette10Feature2Desc,'')
  Cassette10Feature3 = ifelse(nchar(input$vector_inventory_2_Cassette10Feature3)>0, input$vector_inventory_2_Cassette10Feature3,'')
  Cassette10Feature3Desc = ifelse(nchar(input$vector_inventory_2_Cassette10Feature3Desc)>0, input$vector_inventory_2_Cassette10Feature3Desc,'')
  Cassette6ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette6ForwardName)>0, input$vector_inventory_3_Cassette6ForwardName,'')
  Cassette6ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette6ForwardCode)>0, input$vector_inventory_3_Cassette6ForwardCode,'')
  Cassette6ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette6ForwardSequence)>0, input$vector_inventory_3_Cassette6ForwardSequence,'')
  Cassette6ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette6ReverseName)>0, input$vector_inventory_3_Cassette6ReverseName,'')
  Cassette6ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette6ReverseCode)>0, input$vector_inventory_3_Cassette6ReverseCode,'')
  Cassette6ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette6ReverseSequence)>0, input$vector_inventory_3_Cassette6ReverseSequence,'')
  Cassette7ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette7ForwardName)>0, input$vector_inventory_3_Cassette7ForwardName,'')
  Cassette7ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette7ForwardCode)>0, input$vector_inventory_3_Cassette7ForwardCode,'')
  Cassette7ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette7ForwardSequence)>0, input$vector_inventory_3_Cassette7ForwardSequence,'')
  Cassette7ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette7ReverseName)>0, input$vector_inventory_3_Cassette7ReverseName,'')
  Cassette7ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette7ReverseCode)>0, input$vector_inventory_3_Cassette7ReverseCode,'')
  Cassette7ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette7ReverseSequence)>0, input$vector_inventory_3_Cassette7ReverseSequence,'')
  Cassette8ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette8ForwardName)>0, input$vector_inventory_3_Cassette8ForwardName,'')
  Cassette8ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette8ForwardCode)>0, input$vector_inventory_3_Cassette8ForwardCode,'')
  Cassette8ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette8ForwardSequence)>0, input$vector_inventory_3_Cassette8ForwardSequence,'')
  Cassette8ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette8ReverseName)>0, input$vector_inventory_3_Cassette8ReverseName,'')
  Cassette8ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette8ReverseCode)>0, input$vector_inventory_3_Cassette8ReverseCode,'')
  Cassette8ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette8ReverseSequence)>0, input$vector_inventory_3_Cassette8ReverseSequence,'')
  Cassette9ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette9ForwardName)>0, input$vector_inventory_3_Cassette9ForwardName,'')
  Cassette9ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette9ForwardCode)>0, input$vector_inventory_3_Cassette9ForwardCode,'')
  Cassette9ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette9ForwardSequence)>0, input$vector_inventory_3_Cassette9ForwardSequence,'')
  Cassette9ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette9ReverseName)>0, input$vector_inventory_3_Cassette9ReverseName,'')
  Cassette9ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette9ReverseCode)>0, input$vector_inventory_3_Cassette9ReverseCode,'')
  Cassette9ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette9ReverseSequence)>0, input$vector_inventory_3_Cassette9ReverseSequence,'')
  Cassette10ForwardName = ifelse(nchar(input$vector_inventory_3_Cassette10ForwardName)>0, input$vector_inventory_3_Cassette10ForwardName,'')
  Cassette10ForwardCode = ifelse(nchar(input$vector_inventory_3_Cassette10ForwardCode)>0, input$vector_inventory_3_Cassette10ForwardCode,'')
  Cassette10ForwardSequence = ifelse(nchar(input$vector_inventory_3_Cassette10ForwardSequence)>0, input$vector_inventory_3_Cassette10ForwardSequence,'')
  Cassette10ReverseName = ifelse(nchar(input$vector_inventory_3_Cassette10ReverseName)>0, input$vector_inventory_3_Cassette10ReverseName,'')
  Cassette10ReverseCode = ifelse(nchar(input$vector_inventory_3_Cassette10ReverseCode)>0, input$vector_inventory_3_Cassette10ReverseCode,'')
  Cassette10ReverseSequence = ifelse(nchar(input$vector_inventory_3_Cassette10ReverseSequence)>0, input$vector_inventory_3_Cassette10ReverseSequence,'')
  
  
  tb <- paste0(input$project_selected, "_tblVectorInventory")
  
  sql <- paste("UPDATE",tb,"SET
                VectorPrefix = ?VectorPrefix,
                VectorCode = ?VectorCode,
                VectorSuffix = ?VectorSuffix,
                BacterialSelection = ?BacterialSelection,
                PlantSelection = ?PlantSelection,
                Synonym1 = ?Synonym1,
                Synonym2 = ?Synonym2,
                Synonym3 = ?Synonym3,
                Synonym4 = ?Synonym4,
                Synonym5 = ?Synonym5,
                Backbone = ?Backbone,
                Cassette1Promoter = ?Cassette1Promoter,
                Cassette1Gene = ?Cassette1Gene,
                Cassette1Terminator = ?Cassette1Terminator,
                Cassette1Feature1 = ?Cassette1Feature1,
                Cassette1Feature1Desc = ?Cassette1Feature1Desc,
                Cassette1Feature2 = ?Cassette1Feature2,
                Cassette1Feature2Desc = ?Cassette1Feature2Desc,
                Cassette1Feature3 = ?Cassette1Feature3,
                Cassette1Feature3Desc = ?Cassette1Feature3Desc,
                Cassette2Promoter = ?Cassette2Promoter,
                Cassette2Gene = ?Cassette2Gene,
                Cassette2Terminator = ?Cassette2Terminator,
                Cassette2Feature1 = ?Cassette2Feature1,
                Cassette2Feature1Desc = ?Cassette2Feature1Desc,
                Cassette2Feature2 = ?Cassette2Feature2,
                Cassette2Feature2Desc = ?Cassette2Feature2Desc,
                Cassette2Feature3 = ?Cassette2Feature3,
                Cassette2Feature3Desc = ?Cassette2Feature3Desc,
                Cassette3Promoter = ?Cassette3Promoter,
                Cassette3Gene = ?Cassette3Gene,
                Cassette3Terminator = ?Cassette3Terminator,
                Cassette3Feature1 = ?Cassette3Feature1,
                Cassette3Feature1Desc = ?Cassette3Feature1Desc,
                Cassette3Feature2 = ?Cassette3Feature2,
                Cassette3Feature2Desc = ?Cassette3Feature2Desc,
                Cassette3Feature3 = ?Cassette3Feature3,
                Cassette3Feature3Desc = ?Cassette3Feature3Desc,
                Cassette4Promoter = ?Cassette4Promoter,
                Cassette4Gene = ?Cassette4Gene,
                Cassette4Terminator = ?Cassette4Terminator,
                Cassette4Feature1 = ?Cassette4Feature1,
                Cassette4Feature1Desc = ?Cassette4Feature1Desc,
                Cassette4Feature2 = ?Cassette4Feature2,
                Cassette4Feature2Desc = ?Cassette4Feature2Desc,
                Cassette4Feature3 = ?Cassette4Feature3,
                Cassette4Feature3Desc = ?Cassette4Feature3Desc,
                Cassette5Promoter = ?Cassette5Promoter,
                Cassette5Gene = ?Cassette5Gene,
                Cassette5Terminator = ?Cassette5Terminator,
                Cassette5Feature1 = ?Cassette5Feature1,
                Cassette5Feature1Desc = ?Cassette5Feature1Desc,
                Cassette5Feature2 = ?Cassette5Feature2,
                Cassette5Feature2Desc = ?Cassette5Feature2Desc,
                Cassette5Feature3 = ?Cassette5Feature3,
                Cassette5Feature3Desc = ?Cassette5Feature3Desc,
                VNTI_Map_Location = ?VNTI_Map_Location,
                ClonedBy = ?ClonedBy,
                ClonedDate = ?ClonedDate,
                LabBookNumber = ?LabBookNumber,
                PageNumber = ?PageNumber,
                Cassette1ForwardName = ?Cassette1ForwardName,
                Cassette1ForwardCode = ?Cassette1ForwardCode,
                Cassette1ForwardSequence = ?Cassette1ForwardSequence,
                Cassette1ReverseName = ?Cassette1ReverseName,
                Cassette1ReverseCode = ?Cassette1ReverseCode,
                Cassette1ReverseSequence = ?Cassette1ReverseSequence,
                Cassette2ForwardName = ?Cassette2ForwardName,
                Cassette2ForwardCode = ?Cassette2ForwardCode,
                Cassette2ForwardSequence = ?Cassette2ForwardSequence,
                Cassette2ReverseName = ?Cassette2ReverseName,
                Cassette2ReverseCode = ?Cassette2ReverseCode,
                Cassette2ReverseSequence = ?Cassette2ReverseSequence,
                Cassette3ForwardName = ?Cassette3ForwardName,
                Cassette3ForwardCode = ?Cassette3ForwardCode,
                Cassette3ForwardSequence = ?Cassette3ForwardSequence,
                Cassette3ReverseName = ?Cassette3ReverseName,
                Cassette3ReverseCode = ?Cassette3ReverseCode,
                Cassette3ReverseSequence = ?Cassette3ReverseSequence,
                Cassette4ForwardName = ?Cassette4ForwardName,
                Cassette4ForwardCode = ?Cassette4ForwardCode,
                Cassette4ForwardSequence = ?Cassette4ForwardSequence,
                Cassette4ReverseName = ?Cassette4ReverseName,
                Cassette4ReverseCode = ?Cassette4ReverseCode,
                Cassette4ReverseSequence = ?Cassette4ReverseSequence,
                Cassette5ForwardName = ?Cassette5ForwardName,
                Cassette5ForwardCode = ?Cassette5ForwardCode,
                Cassette5ForwardSequence = ?Cassette5ForwardSequence,
                Cassette5ReverseName = ?Cassette5ReverseName,
                Cassette5ReverseCode = ?Cassette5ReverseCode,
                Cassette5ReverseSequence = ?Cassette5ReverseSequence,
                SequencingPrimers = ?SequencingPrimers,
                SequencingCompleted = ?SequencingCompleted,
                DateOfSequencing = ?DateOfSequencing,
                SeqPrimersLabBookNumber = ?SeqPrimersLabBookNumber,
                SeqPrimersPageNumber = ?SeqPrimersPageNumber,
                ContigExpressSequencingAlignment = ?ContigExpressSequencingAlignment,
                SequencingFiles = ?SequencingFiles,
                CheckedBy = ?CheckedBy,
                CheckedDate = ?CheckedDate,
                VerifiedBy = ?VerifiedBy,
                VerifiedDate = ?VerifiedDate,
                TranformedIntoAgro = ?TranformedIntoAgro,
                Strain = ?Strain,
                ConfirmedByPCR = ?ConfirmedByPCR,
                ConfirmedByPCRDate = ?ConfirmedByPCRDate,
                DNAStorageLocation = ?DNAStorageLocation,
                DNAStorageBox = ?DNAStorageBox,
                DNAStoredBy = ?DNAStoredBy,
                DNAStorageDate = ?DNAStorageDate,
                EColiGlycerolStorageLocation = ?EColiGlycerolStorageLocation,
                EColiGlycerolStorageBox = ?EColiGlycerolStorageBox,
                EColiGlycerolStoredBy = ?EColiGlycerolStoredBy,
                EColiGlycerolStorageDate = ?EColiGlycerolStorageDate,
                AgroGlycerolStorageLocation = ?AgroGlycerolStorageLocation,
                AgroGlycerolStorageBox = ?AgroGlycerolStorageBox,
                AgroGlycerolStoredBy = ?AgroGlycerolStoredBy,
                AgroGlycerolStorageDate = ?AgroGlycerolStorageDate,
                Cassette6Promoter = ?Cassette6Promoter,
                Cassette6Gene = ?Cassette6Gene,
                Cassette6Terminator = ?Cassette6Terminator,
                Cassette6Feature1 = ?Cassette6Feature1,
                Cassette6Feature1Desc = ?Cassette6Feature1Desc,
                Cassette6Feature2 = ?Cassette6Feature2,
                Cassette6Feature2Desc = ?Cassette6Feature2Desc,
                Cassette6Feature3 = ?Cassette6Feature3,
                Cassette6Feature3Desc = ?Cassette6Feature3Desc,
                Cassette7Promoter = ?Cassette7Promoter,
                Cassette7Gene = ?Cassette7Gene,
                Cassette7Terminator = ?Cassette7Terminator,
                Cassette7Feature1 = ?Cassette7Feature1,
                Cassette7Feature1Desc = ?Cassette7Feature1Desc,
                Cassette7Feature2 = ?Cassette7Feature2,
                Cassette7Feature2Desc = ?Cassette7Feature2Desc,
                Cassette7Feature3 = ?Cassette7Feature3,
                Cassette7Feature3Desc = ?Cassette7Feature3Desc,
                Cassette8Promoter = ?Cassette8Promoter,
                Cassette8Gene = ?Cassette8Gene,
                Cassette8Terminator = ?Cassette8Terminator,
                Cassette8Feature1 = ?Cassette8Feature1,
                Cassette8Feature1Desc = ?Cassette8Feature1Desc,
                Cassette8Feature2 = ?Cassette8Feature2,
                Cassette8Feature2Desc = ?Cassette8Feature2Desc,
                Cassette8Feature3 = ?Cassette8Feature3,
                Cassette8Feature3Desc = ?Cassette8Feature3Desc,
                Cassette9Promoter = ?Cassette9Promoter,
                Cassette9Gene = ?Cassette9Gene,
                Cassette9Terminator = ?Cassette9Terminator,
                Cassette9Feature1 = ?Cassette9Feature1,
                Cassette9Feature1Desc = ?Cassette9Feature1Desc,
                Cassette9Feature2 = ?Cassette9Feature2,
                Cassette9Feature2Desc = ?Cassette9Feature2Desc,
                Cassette9Feature3 = ?Cassette9Feature3,
                Cassette9Feature3Desc = ?Cassette9Feature3Desc,
                Cassette10Promoter = ?Cassette10Promoter,
                Cassette10Gene = ?Cassette10Gene,
                Cassette10Terminator = ?Cassette10Terminator,
                Cassette10Feature1 = ?Cassette10Feature1,
                Cassette10Feature1Desc = ?Cassette10Feature1Desc,
                Cassette10Feature2 = ?Cassette10Feature2,
                Cassette10Feature2Desc = ?Cassette10Feature2Desc,
                Cassette10Feature3 = ?Cassette10Feature3,
                Cassette10Feature3Desc = ?Cassette10Feature3Desc,
                Cassette6ForwardName = ?Cassette6ForwardName,
                Cassette6ForwardCode = ?Cassette6ForwardCode,
                Cassette6ForwardSequence = ?Cassette6ForwardSequence,
                Cassette6ReverseName = ?Cassette6ReverseName,
                Cassette6ReverseCode = ?Cassette6ReverseCode,
                Cassette6ReverseSequence = ?Cassette6ReverseSequence,
                Cassette7ForwardName = ?Cassette7ForwardName,
                Cassette7ForwardCode = ?Cassette7ForwardCode,
                Cassette7ForwardSequence = ?Cassette7ForwardSequence,
                Cassette7ReverseName = ?Cassette7ReverseName,
                Cassette7ReverseCode = ?Cassette7ReverseCode,
                Cassette7ReverseSequence = ?Cassette7ReverseSequence,
                Cassette8ForwardName = ?Cassette8ForwardName,
                Cassette8ForwardCode = ?Cassette8ForwardCode,
                Cassette8ForwardSequence = ?Cassette8ForwardSequence,
                Cassette8ReverseName = ?Cassette8ReverseName,
                Cassette8ReverseCode = ?Cassette8ReverseCode,
                Cassette8ReverseSequence = ?Cassette8ReverseSequence,
                Cassette9ForwardName = ?Cassette9ForwardName,
                Cassette9ForwardCode = ?Cassette9ForwardCode,
                Cassette9ForwardSequence = ?Cassette9ForwardSequence,
                Cassette9ReverseName = ?Cassette9ReverseName,
                Cassette9ReverseCode = ?Cassette9ReverseCode,
                Cassette9ReverseSequence = ?Cassette9ReverseSequence,
                Cassette10ForwardName = ?Cassette10ForwardName,
                Cassette10ForwardCode = ?Cassette10ForwardCode,
                Cassette10ForwardSequence = ?Cassette10ForwardSequence,
                Cassette10ReverseName = ?Cassette10ReverseName,
                Cassette10ReverseCode = ?Cassette10ReverseCode,
                Cassette10ReverseSequence = ?Cassette10ReverseSequence
                WHERE VectorID = ?VectorID;")
  query <- sqlInterpolate(pool, sql,
                          VectorPrefix = VectorPrefix,
                          VectorCode = VectorCode,
                          VectorSuffix = VectorSuffix,
                          BacterialSelection = BacterialSelection,
                          PlantSelection = PlantSelection,
                          Synonym1 = Synonym1,
                          Synonym2 = Synonym2,
                          Synonym3 = Synonym3,
                          Synonym4 = Synonym4,
                          Synonym5 = Synonym5,
                          Backbone = Backbone,
                          Cassette1Promoter = Cassette1Promoter,
                          Cassette1Gene = Cassette1Gene,
                          Cassette1Terminator = Cassette1Terminator,
                          Cassette1Feature1 = Cassette1Feature1,
                          Cassette1Feature1Desc = Cassette1Feature1Desc,
                          Cassette1Feature2 = Cassette1Feature2,
                          Cassette1Feature2Desc = Cassette1Feature2Desc,
                          Cassette1Feature3 = Cassette1Feature3,
                          Cassette1Feature3Desc = Cassette1Feature3Desc,
                          Cassette2Promoter = Cassette2Promoter,
                          Cassette2Gene = Cassette2Gene,
                          Cassette2Terminator = Cassette2Terminator,
                          Cassette2Feature1 = Cassette2Feature1,
                          Cassette2Feature1Desc = Cassette2Feature1Desc,
                          Cassette2Feature2 = Cassette2Feature2,
                          Cassette2Feature2Desc = Cassette2Feature2Desc,
                          Cassette2Feature3 = Cassette2Feature3,
                          Cassette2Feature3Desc = Cassette2Feature3Desc,
                          Cassette3Promoter = Cassette3Promoter,
                          Cassette3Gene = Cassette3Gene,
                          Cassette3Terminator = Cassette3Terminator,
                          Cassette3Feature1 = Cassette3Feature1,
                          Cassette3Feature1Desc = Cassette3Feature1Desc,
                          Cassette3Feature2 = Cassette3Feature2,
                          Cassette3Feature2Desc = Cassette3Feature2Desc,
                          Cassette3Feature3 = Cassette3Feature3,
                          Cassette3Feature3Desc = Cassette3Feature3Desc,
                          Cassette4Promoter = Cassette4Promoter,
                          Cassette4Gene = Cassette4Gene,
                          Cassette4Terminator = Cassette4Terminator,
                          Cassette4Feature1 = Cassette4Feature1,
                          Cassette4Feature1Desc = Cassette4Feature1Desc,
                          Cassette4Feature2 = Cassette4Feature2,
                          Cassette4Feature2Desc = Cassette4Feature2Desc,
                          Cassette4Feature3 = Cassette4Feature3,
                          Cassette4Feature3Desc = Cassette4Feature3Desc,
                          Cassette5Promoter = Cassette5Promoter,
                          Cassette5Gene = Cassette5Gene,
                          Cassette5Terminator = Cassette5Terminator,
                          Cassette5Feature1 = Cassette5Feature1,
                          Cassette5Feature1Desc = Cassette5Feature1Desc,
                          Cassette5Feature2 = Cassette5Feature2,
                          Cassette5Feature2Desc = Cassette5Feature2Desc,
                          Cassette5Feature3 = Cassette5Feature3,
                          Cassette5Feature3Desc = Cassette5Feature3Desc,
                          VNTI_Map_Location = VNTI_Map_Location,
                          ClonedBy = ClonedBy,
                          ClonedDate = ClonedDate,
                          LabBookNumber = LabBookNumber,
                          PageNumber = PageNumber,
                          Cassette1ForwardName = Cassette1ForwardName,
                          Cassette1ForwardCode = Cassette1ForwardCode,
                          Cassette1ForwardSequence = Cassette1ForwardSequence,
                          Cassette1ReverseName = Cassette1ReverseName,
                          Cassette1ReverseCode = Cassette1ReverseCode,
                          Cassette1ReverseSequence = Cassette1ReverseSequence,
                          Cassette2ForwardName = Cassette2ForwardName,
                          Cassette2ForwardCode = Cassette2ForwardCode,
                          Cassette2ForwardSequence = Cassette2ForwardSequence,
                          Cassette2ReverseName = Cassette2ReverseName,
                          Cassette2ReverseCode = Cassette2ReverseCode,
                          Cassette2ReverseSequence = Cassette2ReverseSequence,
                          Cassette3ForwardName = Cassette3ForwardName,
                          Cassette3ForwardCode = Cassette3ForwardCode,
                          Cassette3ForwardSequence = Cassette3ForwardSequence,
                          Cassette3ReverseName = Cassette3ReverseName,
                          Cassette3ReverseCode = Cassette3ReverseCode,
                          Cassette3ReverseSequence = Cassette3ReverseSequence,
                          Cassette4ForwardName = Cassette4ForwardName,
                          Cassette4ForwardCode = Cassette4ForwardCode,
                          Cassette4ForwardSequence = Cassette4ForwardSequence,
                          Cassette4ReverseName = Cassette4ReverseName,
                          Cassette4ReverseCode = Cassette4ReverseCode,
                          Cassette4ReverseSequence = Cassette4ReverseSequence,
                          Cassette5ForwardName = Cassette5ForwardName,
                          Cassette5ForwardCode = Cassette5ForwardCode,
                          Cassette5ForwardSequence = Cassette5ForwardSequence,
                          Cassette5ReverseName = Cassette5ReverseName,
                          Cassette5ReverseCode = Cassette5ReverseCode,
                          Cassette5ReverseSequence = Cassette5ReverseSequence,
                          SequencingPrimers = SequencingPrimers,
                          SequencingCompleted = SequencingCompleted,
                          DateOfSequencing = DateOfSequencing,
                          SeqPrimersLabBookNumber = SeqPrimersLabBookNumber,
                          SeqPrimersPageNumber = SeqPrimersPageNumber,
                          ContigExpressSequencingAlignment = ContigExpressSequencingAlignment,
                          SequencingFiles = SequencingFiles,
                          CheckedBy = CheckedBy,
                          CheckedDate = CheckedDate,
                          VerifiedBy = VerifiedBy,
                          VerifiedDate = VerifiedDate,
                          TranformedIntoAgro = TranformedIntoAgro,
                          Strain = Strain,
                          ConfirmedByPCR = ConfirmedByPCR,
                          ConfirmedByPCRDate = ConfirmedByPCRDate,
                          DNAStorageLocation = DNAStorageLocation,
                          DNAStorageBox = DNAStorageBox,
                          DNAStoredBy = DNAStoredBy,
                          DNAStorageDate = DNAStorageDate,
                          EColiGlycerolStorageLocation = EColiGlycerolStorageLocation,
                          EColiGlycerolStorageBox = EColiGlycerolStorageBox,
                          EColiGlycerolStoredBy = EColiGlycerolStoredBy,
                          EColiGlycerolStorageDate = EColiGlycerolStorageDate,
                          AgroGlycerolStorageLocation = AgroGlycerolStorageLocation,
                          AgroGlycerolStorageBox = AgroGlycerolStorageBox,
                          AgroGlycerolStoredBy = AgroGlycerolStoredBy,
                          AgroGlycerolStorageDate = AgroGlycerolStorageDate,
                          Cassette6Promoter = Cassette6Promoter,
                          Cassette6Gene = Cassette6Gene,
                          Cassette6Terminator = Cassette6Terminator,
                          Cassette6Feature1 = Cassette6Feature1,
                          Cassette6Feature1Desc = Cassette6Feature1Desc,
                          Cassette6Feature2 = Cassette6Feature2,
                          Cassette6Feature2Desc = Cassette6Feature2Desc,
                          Cassette6Feature3 = Cassette6Feature3,
                          Cassette6Feature3Desc = Cassette6Feature3Desc,
                          Cassette7Promoter = Cassette7Promoter,
                          Cassette7Gene = Cassette7Gene,
                          Cassette7Terminator = Cassette7Terminator,
                          Cassette7Feature1 = Cassette7Feature1,
                          Cassette7Feature1Desc = Cassette7Feature1Desc,
                          Cassette7Feature2 = Cassette7Feature2,
                          Cassette7Feature2Desc = Cassette7Feature2Desc,
                          Cassette7Feature3 = Cassette7Feature3,
                          Cassette7Feature3Desc = Cassette7Feature3Desc,
                          Cassette8Promoter = Cassette8Promoter,
                          Cassette8Gene = Cassette8Gene,
                          Cassette8Terminator = Cassette8Terminator,
                          Cassette8Feature1 = Cassette8Feature1,
                          Cassette8Feature1Desc = Cassette8Feature1Desc,
                          Cassette8Feature2 = Cassette8Feature2,
                          Cassette8Feature2Desc = Cassette8Feature2Desc,
                          Cassette8Feature3 = Cassette8Feature3,
                          Cassette8Feature3Desc = Cassette8Feature3Desc,
                          Cassette9Promoter = Cassette9Promoter,
                          Cassette9Gene = Cassette9Gene,
                          Cassette9Terminator = Cassette9Terminator,
                          Cassette9Feature1 = Cassette9Feature1,
                          Cassette9Feature1Desc = Cassette9Feature1Desc,
                          Cassette9Feature2 = Cassette9Feature2,
                          Cassette9Feature2Desc = Cassette9Feature2Desc,
                          Cassette9Feature3 = Cassette9Feature3,
                          Cassette9Feature3Desc = Cassette9Feature3Desc,
                          Cassette10Promoter = Cassette10Promoter,
                          Cassette10Gene = Cassette10Gene,
                          Cassette10Terminator = Cassette10Terminator,
                          Cassette10Feature1 = Cassette10Feature1,
                          Cassette10Feature1Desc = Cassette10Feature1Desc,
                          Cassette10Feature2 = Cassette10Feature2,
                          Cassette10Feature2Desc = Cassette10Feature2Desc,
                          Cassette10Feature3 = Cassette10Feature3,
                          Cassette10Feature3Desc = Cassette10Feature3Desc,
                          Cassette6ForwardName = Cassette6ForwardName,
                          Cassette6ForwardCode = Cassette6ForwardCode,
                          Cassette6ForwardSequence = Cassette6ForwardSequence,
                          Cassette6ReverseName = Cassette6ReverseName,
                          Cassette6ReverseCode = Cassette6ReverseCode,
                          Cassette6ReverseSequence = Cassette6ReverseSequence,
                          Cassette7ForwardName = Cassette7ForwardName,
                          Cassette7ForwardCode = Cassette7ForwardCode,
                          Cassette7ForwardSequence = Cassette7ForwardSequence,
                          Cassette7ReverseName = Cassette7ReverseName,
                          Cassette7ReverseCode = Cassette7ReverseCode,
                          Cassette7ReverseSequence = Cassette7ReverseSequence,
                          Cassette8ForwardName = Cassette8ForwardName,
                          Cassette8ForwardCode = Cassette8ForwardCode,
                          Cassette8ForwardSequence = Cassette8ForwardSequence,
                          Cassette8ReverseName = Cassette8ReverseName,
                          Cassette8ReverseCode = Cassette8ReverseCode,
                          Cassette8ReverseSequence = Cassette8ReverseSequence,
                          Cassette9ForwardName = Cassette9ForwardName,
                          Cassette9ForwardCode = Cassette9ForwardCode,
                          Cassette9ForwardSequence = Cassette9ForwardSequence,
                          Cassette9ReverseName = Cassette9ReverseName,
                          Cassette9ReverseCode = Cassette9ReverseCode,
                          Cassette9ReverseSequence = Cassette9ReverseSequence,
                          Cassette10ForwardName = Cassette10ForwardName,
                          Cassette10ForwardCode = Cassette10ForwardCode,
                          Cassette10ForwardSequence = Cassette10ForwardSequence,
                          Cassette10ReverseName = Cassette10ReverseName,
                          Cassette10ReverseCode = Cassette10ReverseCode,
                          Cassette10ReverseSequence = Cassette10ReverseSequence,
                          VectorID = VectorID)
  dbExecute(pool, query)
  shinyalert("", "Vector Inventory Updated", type = "success")
  
  vector_inventory_values$Data <- vector_inventory()
  vector_inventory_values$Data
})


# Delete the Selected Vector

observeEvent(input$vector_inventory_search_DeleteTheSelectedVector,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_search_DeleteTheSelectedVector_Confirm",
    type = "warning",
    title = "",
    text = "Do you really want to DELETE the record?",
    btn_labels = c("Cancel", "Yes, Delete!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})

observeEvent(input$vector_inventory_search_DeleteTheSelectedVector_Confirm, {
  if(input$vector_inventory_search_DeleteTheSelectedVector_Confirm == TRUE){
    
    tb <- paste0(input$project_selected, "_tblVectorInventory")
    vector_inventory <- vector_inventory_values$Data <- vector_inventory()
    dt <- vector_inventory %>%
      dplyr::filter(VectorID == input$vector_inventory_search_SelectedVectorID)
    
    sql <- paste("DELETE FROM",tb,"WHERE VectorID = ?VectorID;")
    query <- sqlInterpolate(pool, sql, VectorID = dt$VectorID)
    dbExecute(pool, query) # delete
    
    # reload data
    vector_inventory_values$Data <- vector_inventory()
    vector_inventory_values$Data
  }
}, ignoreInit = TRUE)


# Back to Control Form

observeEvent(input$vector_inventory_search_ControlForm,{

})


## Exit
observeEvent(input$vector_inventory_search_Exit,{
  confirmSweetAlert(
    session = session,
    inputId = "vector_inventory_search_Exit_Confirm",
    type = "warning",
    title = "Do you really want to EXIT the application?",
    btn_labels = c("Cancel", "Yes, Exit!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})

observeEvent(input$vector_inventory_search_Exit_Confirm, {

  if(input$vector_inventory_search_Exit_Confirm == TRUE){
    js$closewindow();
    stopApp()
  }

}, ignoreInit = TRUE)
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------


