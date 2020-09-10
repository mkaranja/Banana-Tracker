tab_files <- list.files(path = "tabs/server/project_specific/data_form/transformation_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

observeEvent(input$transformation_module, {
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Transformation Module - ",input$project_selected)),
        
        tabsetPanel(type = "pills", id = "transformation_module_tabs", 
                    search_transformation,
                    new_transformation,
                    updating_last_transformation,
                    # batch_subculturing,
                    labels_Transformation       
                    ),
        easyClose = F, size = "l"
  ))
})
# # SEARCH TRANSFORMATIONS ------------------------------------------------------------------------------------------------------

observeEvent(input$transformation_module,{

   dt <- transformation()

    updateSelectInput(session, "search_transformation_Identity", "Identity", choices = c('', dt$TransformationID))
    updateSelectInput(session, "search_transformation_Cultivar", "Cultivar", choices = c('', dt$Cultivar))
    updateSelectInput(session, "search_transformation_VectorID", "Vector ID", choices = c('', dt$VectorID1))
    updateDateRangeInput(session, "search_transformation_DateOfStarterCulture", "Date of Starter Culture",
                         min = min(dt$DateOfStarterCulture), max = max(dt$DateOfStarterCulture),
                         start = min(dt$DateOfStarterCulture), end = max(dt$DateOfStarterCulture))

})

observeEvent(input$search_DeletedTransformations,{
  if(input$search_DeletedTransformations==TRUE){
  dt <- deletedTransformation()

  updateSelectInput(session, "search_transformation_Identity", "Identity", choices = c('', dt$TransformationID))
  updateSelectInput(session, "search_transformation_Cultivar", "Cultivar", choices = c('', dt$Cultivar))
  updateSelectInput(session, "search_transformation_VectorID", "Vector ID", choices = c('', dt$VectorID1))
  updateDateRangeInput(session, "search_transformation_DateOfStarterCulture", "Date of Starter Culture",
                       min = min(dt$DateOfStarterCulture), max = max(dt$DateOfStarterCulture),
                       start = min(dt$DateOfStarterCulture), end = max(dt$DateOfStarterCulture))
  }
})

# search tables
output$search_Transformation_Table_Output <- renderUI({
  if(input$search_DeletedTransformations == TRUE){
    rHandsontableOutput("search_deletedTransformation_Table")
  }else {
    rHandsontableOutput("search_Transformation_Table")
  }
})

transformationInput <- reactive({
  dt <- transformation()
  if(input$search_transformation_Identity!=''){
    dt <- dt %>%
      dplyr::filter(trimws(TransformationID) == trimws(input$search_transformation_Identity))
  }
  if(input$search_transformation_Cultivar!=''){
    dt <- dt %>%
      dplyr::filter(trimws(Cultivar) == trimws(input$search_transformation_Cultivar))
  }
  if(input$search_transformation_VectorID!=''){
    dt <- dt %>%
      dplyr::filter(trimws(VectorID1) == trimws(input$search_transformation_VectorID))
  }
  # dt %<>%
  #   dplyr::filter(between(trimws(DateOfStarterCulture), dt$DateOfStarterCulture[1], dt$DateOfStarterCulture[2]))
  dt
})

observeEvent(input$search_transformation_ActionSearch,{
  #if(input$search_DeletedTransformations==FALSE){
    output$search_Transformation_Table <- renderRHandsontable({
      rhandsontable(transformationInput(), selectCallback = T, readOnly = T, rowHeaders=F) %>%
        hot_table(stretchH = "all")
    })
  #}
})
# 
# deleted table
deletedTransformationsInput <- reactive({
  dt <- deletedTransformation()
  if(input$search_transformation_Identity!=''){
    dt <- dt %>%
      dplyr::filter(trimws(TransformationID) == trimws(input$search_transformation_Identity))
  }
  if(input$search_transformation_Cultivar!=''){
    dt <- dt %>%
      dplyr::filter(trimws(Cultivar) == trimws(input$search_transformation_Cultivar))
  }
  if(input$search_transformation_VectorID!=''){
    dt <- dt %>%
      dplyr::filter(trimws(VectorID1) == trimws(input$search_transformation_VectorID))
  }
  # dt %<>%
  #   dplyr::filter(between(trimws(DateOfStarterCulture), dt$DateOfStarterCulture[1], dt$DateOfStarterCulture[2]))
  dt
})

observeEvent(input$search_transformation_ActionSearch,{
 # if(input$search_DeletedTransformations==TRUE){
    if(!is.null(deletedTransformationsInput())){
      output$search_deletedTransformation_Table <- renderRHandsontable({
        rhandsontable(deletedTransformationsInput(), selectCallback = T, readOnly = T, rowHeaders=F) %>%
          hot_table(stretchH = "all")
      })
    }else{
      showNotification("Not data to display.", type = "warning")
    }
  #}
})
# 
# # observeEvent(input$search_transformation_ActionSearch,{
# #   output$search_Transformation_Table <- renderRHandsontable({
# #     rhandsontable(transformationInput(), selectCallback = T, readOnly = T, rowHeaders=F) %>%
# #       hot_table(stretchH = "all")
# #   })
# # })
# 
# 
# deleted transformations
observeEvent(input$search_transformation_SearchDeletedTransformations,{
  dt <- deletedTransformation_values$Data <- deletedTransformation()

  updateSelectInput(session, "search_transformation_Identity", "Identity", choices = c('', dt$TransformationID))
  updateSelectInput(session, "search_transformation_Cultivar", "Cultivar", choices = c('', dt$Cultivar))
  updateSelectInput(session, "search_transformation_VectorID", "Vector ID", choices = c('', dt$VectorID1))
  updateDateRangeInput(session, "search_transformation_DateOfStarterCulture", "Date of Starter Culture",
                       min = min(dt$DateOfStarterCulture), max = max(dt$DateOfStarterCulture),
                       start = min(dt$DateOfStarterCulture), end = max(dt$DateOfStarterCulture))
})
deletedTransformationInput <- reactive({
  dt <- deletedTransformation_values$Data <- deletedTransformation()

  if(input$search_transformation_Identity!=''){
    dt <- dt %>%
      dplyr::filter(TransformationID == input$search_transformation_Identity)
  }
  if(input$search_transformation_Cultivar!=''){
    dt <- dt %>%
      dplyr::filter(Cultivar == input$search_transformation_Cultivar)
  }
  if(input$search_transformation_VectorID!=''){
    dt <- dt %>%
      dplyr::filter(VectorID1 == input$search_transformation_VectorID)
  }
  dt %<>%
    dplyr::filter(between(DateOfStarterCulture, dt$DateOfStarterCulture[1], dt$DateOfStarterCulture[2]))
})
observeEvent(input$search_transformation_ActionSearch,{
    output$search_deletedTransformation_Table <- renderRHandsontable({
      rhandsontable(deletedTransformationInput(), selectCallback = T, readOnly = T, rowHeaders=F) %>%
        hot_table(stretchH = "all")
    })
})
 
 
# Clear Fields
observeEvent(input$search_transformation_ActionClearForm,{
  confirmSweetAlert(
    session = session,
    inputId = "search_transformation_ActionClearForm_confirm",
    type = "warning",
    title = "",
    text = "Do you really want to clear the fields?",
    btn_labels = c("Cancel", "Yes, Clear!"),
    btn_colors = c("#DD6B55", "#04B404")
  )
})
observeEvent(input$search_transformation_ActionClearForm_confirm, {

    reset("search_transformation_Form")
    updateDateRangeInput(session, "search_transformation_DateOfStarterCulture", "Date of Starter Culture",
                         min = NULL, max = NULL, start = NULL, end = NULL)

    shinyjs::reset("search_transformation_Form")
    shinyjs::hide("search_Transformation_Table")
    shinyjs::hide("search_deletedTransformation_Table")
    shinyjs::hide("search_transformation_CultureTable")
    shinyjs::hide("search_transformation_Culture_Form")

}, ignoreInit = TRUE)

## update Cultures
# culture table
observeEvent(input$search_Transformation_Table_select$select$r,{
  r <- isolate(input$search_Transformation_Table_select$select$r)
  id <- as.character(transformationInput()[r,'TransformationID'])
  updateTextInput(session, "search_transformation_Identity", "Identity", value = id)

  dt <- loadData(paste0(input$project_selected, "_tblCulturesTransformation")) %>%
    dplyr::filter(trimws(TransformationID) == trimws(id))

  output$search_transformation_CultureTable <- renderRHandsontable({
    rhandsontable(dt, selectCallback = T, readOnly = T, rowHeaders=F) %>%
      hot_table(stretchH = "all")
  })
})
# # culture fields
# # output$tbx <- renderPrint({
# #   input$search_transformation_CultureTable_select$select
# # })
observeEvent(input$search_transformation_ActionCulture,{
  r1 <- isolate(input$search_Transformation_Table_select$select$r)
  id1 <- as.character(transformationInput()[r1,'TransformationID'])

  r2 <- input$search_transformation_CultureTable_select$select$r

  dt <- loadData(paste0(input$project_selected, "_tblCulturesTransformation")) %>%
    dplyr::filter(trimws(TransformationID) == trimws(id1))
  id <- as.character(dt[r2,'TransformationID'])

  if(length(r2)>0){
    output$search_transformation_Culture_Output <- renderUI({

              div(id = "search_transformation_Culture_Form",
              column(4,
                   disabled(textInput("search_transformation_SelectedIdentity","Selected Identity", value = id, width = "100%")),
                   numericInput("search_transformation_NumberOfCultures",labelMandatory("Number of Cultures"), min = 0, value = NULL, width = "100%"),
                   dateInput("search_transformation_DateOfCulture",labelMandatory("Date of Cultures"), width = "100%"),
                   selectInput("search_transformation_CulturedBy",labelMandatory("Cultured By"), choices = c('', loadData("tblCulturedBy")$CulturedBy), width = "100%"),
                   selectInput("search_transformation_MediaForCultures",labelMandatory("Media"), choices = c('', loadData("tblMedia")$Media), width = "100%"),
                   numericInput("search_transformation_LabBookNumberForCultures",labelMandatory("Lab Book Number"), min = 0, value = NULL, width = "100%"),
                   numericInput("search_transformation_PageNumberForCultures",labelMandatory("Page Number"), min = 0, value = NULL, width = "100%"),
                   textInput("search_transformation_Comments", "Comments", width = "100%")
              ),
              column(3, br(), br(),br(),br(), br(),br(),br(),br(),
                     panel_div(class_type = "default",
                               content = tags$div(
                                 tags$b("Additives"),
                                 awesomeCheckboxGroup(inputId = "search_transformation_AdditivesForCultures",
                                                      label = "", choices = c(loadData("tblAdditives")$Additives), selected = NULL, status = "info")
                               )),br(),br(),br(), br(),br(),br(),br(),br(), br(),br(),
                    actionBttn("search_transformation_SaveRecord", "Save Record", style = "fill", size = "xs", color = "primary", block=T)
                )
              )
        })
    }else {
      showNotification("Select an Identity from the table", type = "error")
    }
})

search_Transformation_MandatoryFields <-
  c("search_transformation_SelectedIdentity","search_transformation_NumberOfCultures","search_transformation_CulturedBy",
    "search_transformation_MediaForCultures", "search_transformation_LabBookNumberForCultures", "search_transformation_PageNumberForCultures")
observe({
  # check if all mandatory fields have a value
  mandatoryFilled <-
    vapply(search_Transformation_MandatoryFields,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != ""
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  # enable/disable the submit button
  shinyjs::toggleState(id = "search_transformation_SaveRecord", "Save Record", condition = mandatoryFilled)
})

observeEvent(input$search_transformation_SaveRecord,{
  tb <- paste0(input$project_selected, "_tblCulturesTransformation")
  dt <- data.frame(
    TransformationID = input$search_transformation_SelectedIdentity,
    NumberOfCultures = input$search_transformation_NumberOfCultures,
    DateOfCulture = input$search_transformation_DateOfCulture,
    CulturedBy = input$search_transformation_CulturedBy,
    Comments = ifelse(nchar(input$search_transformation_Comments)>0,input$search_transformation_Comments,''),
    MediaForCultures  = input$search_transformation_MediaForCultures,
    AdditivesForCultures = ifelse(!is.null(input$search_transformation_AdditivesForCultures),input$search_transformation_AdditivesForCultures,''),
    LabBookNumberForCultures = input$search_transformation_LabBookNumberForCultures,
    PageNumberForCultures = input$search_transformation_PageNumberForCultures
  )
  try(expr = dbWriteTable(conn = pool,   name = tb, value = dt, overwrite = F, append = T))
  #saveData(dt, tb)
  shinyalert("Success!", "Record Saved", type = "success")

  shinyjs::reset("search_transformation_Form")
  shinyjs::hide("search_Transformation_Table")
  shinyjs::hide("search_deletedTransformation_Table")
  shinyjs::hide("search_transformation_CultureTable")
  shinyjs::hide("search_transformation_Culture_Form")

})
# delete culture
observeEvent(input$search_transformation_ActionDelete,{
  r <- input$search_transformation_ResultsTable_select$select$r
  v <- as.character(transformationInput()[r,'Identity'])

  updateTextInput(session, "search_transformation_Identity", "Identity", value = v)

  admin <- tbl(pool, "tblUserInformation") %>%
    dplyr::filter(UserName == input$userName) %>%
    collect()
  tb <- paste0(input$project_selected, "_tblTransformation")
  td <- paste0(input$project_selected, "_tblDeletedTransformation")
  if(grepl("Admin", admin$PrivilegeLevel)==TRUE){
    sql <- paste("DELETE FROM",tb,"WHERE TransformationID = ?id;")
    query <- sqlInterpolate(pool, sql, TransformationID = input$search_transformation_Identity)
    dbExecute(pool, query)
  }else {
    shinyalert("", "Sorry! You don't have the rights to delete the record", type = "error")
  }
})

# NEW TRANSFORMATIONS -------------------------------------------------------------------------------------------------------------

observeEvent(input$transformation_module,{
  agrobacterium <- paste0(input$project_selected, "_tblAgrobacteriumStrains")
  vectorInventory <- paste0(input$project_selected, "_tblVectorInventory")
 
  updateSelectInput(session,"new_transformation_AgrobacteriumStrains", "Agrobacterium Strains", choices = c("", loadData(agrobacterium)$AgrobacteriumStrains))
  
})

# enable button
# observe({
#   if(input$new_transformation_ParentIDType !=""){
#     shinyjs::toggleState(id = "new_transformation_ParentIDType_GetID", "", condition = mandatoryFilled)
#   }
# })


observeEvent(input$new_transformation_ParentIDType_GetID,{
  if(input$new_transformation_ParentIDType == "Cell Suspension Culture (CSC)"){
  #  dt <- 
  } else if(input$new_transformation_ParentIDType == "Explant") {
    
  }
  dt <- 
  updateSelectInput(session, "new_transformation_ParentID","", choices = c("", transformation()$CSCIdentity))
})
# load data
new_transformation_input <- reactive({
  
})
observeEvent(input$new_transformation_GetData,{
  req("new_transformation_ParentID")
  dt <- transformation() %>%
    dplyr::filter(trimws(CSCIdentity) == trimws(input$new_transformation_ParentID))
  
  
  updateSelectInput(session,"new_transformation_PermitType1", "Permit Type", choices = c("", loadData("tblPermitType")$PermitType))
  updateSelectInput(session,"new_transformation_PermitType2", "Permit Type", choices = c("", loadData("tblPermitType")$PermitType))
  updateSelectInput(session,"new_transformation_PermitType3", "Permit Type", choices = c("", loadData("tblPermitType")$PermitType))
  updateSelectInput(session,"new_transformation_PermitType4", "Permit Type", choices = c("", loadData("tblPermitType")$PermitType))
  updateSelectInput(session,"new_transformation_PermitType5", "Permit Type", choices = c("", loadData("tblPermitType")$PermitType))
  updateSelectInput(session,"new_transformation_PermitType6", "Permit Type", choices = c("", loadData("tblPermitType")$PermitType))
  
  updateSelectInput(session,"new_transformation_Media", "Media", choices = c("", loadData("tblMedia")$Media))
  updateSelectInput(session,"new_transformation_Additives", "Additives", choices = c(loadData("tblAdditives")$Additives))
  updateSelectInput(session,"new_transformation_CulturedBy","Cultured By", choices = c("",loadData("tblCulturedBy")$CulturedBy))
  updateSelectInput(session,"new_transformation_ExplantIdentity", "Explant Identity", choices = c('', transformation()$ExplantIdentity))
  
  #    updateSelectInput(session,"new_transformation_PlantSelection_Vector1", "", choices = c('', loadData(vectorInventory)$VectorID))
  #    updateSelectInput(session,"new_transformation_PlantSelection_Vector2", "", choices = c('', loadData(vectorInventory)$VectorID))
  
})

# Save new record
## mandatory fields

new_transformation_MandatoryFields <- c("new_transformation_TransformationType", "new_transformation_AgrobacteriumStrains", "new_transformation_ParentIDType",
                           "new_transformation_Media", "new_transformation_LabBookNumber", "new_transformation_PageNumber")

observe({
  # check if all mandatory fields have a value
  mandatoryFilled <-
    vapply(new_transformation_MandatoryFields,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != ""
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  # enable/disable the submit button
  shinyjs::toggleState(id = "new_transformation_SaveStarterCultureAndSubCulture", "Save Starter Culture and Sub Culture", condition = mandatoryFilled)
})


observeEvent(input$new_transformation_SaveStarterCultureAndSubCulture,{
  tb1 <- paste0(input$project_selected,"_tblTransformation")
  tb2 <- paste0(input$project_selected,"_tblCulturesTransformation")

  dt1 <- data.frame(
    TransformationID = input$new_transformation_TransformationID,
    TransformationType = input$new_transformation_TransformationType,
    AgrobacteriumStrains = input$new_transformation_AgrobacteriumStrains,
    ExplantIdentity = input$new_transformation_ExplantIdentity,
    CSCIdentity = "",
    VectorID1 = input$new_transformation_PlantSelection_Vector1,
    VectorID2 = input$new_transformation_PlantSelection_Vector2,
    IdentityType = input$new_transformation_IdentityType,
    PermitType1 = input$new_transformation_PermitType1,
    PermitType2 = input$new_transformation_PermitType2,
    PermitType3 = input$new_transformation_PermitType3,
    PermitType4 = input$new_transformation_PermitType4,
    PermitType5 = input$new_transformation_PermitType5,
    PermitType6 = input$new_transformation_PermitType6,
    PermitNumber1 = input$new_transformation_PermitNumber1,
    PermitNumber2 = input$new_transformation_PermitNumber2,
    PermitNumber3 = input$new_transformation_PermitNumber3,
    PermitNumber4 = input$new_transformation_PermitNumber4,
    PermitNumber5 = input$new_transformation_PermitNumber5,
    PermitNumber6 = input$new_transformation_PermitNumber6,
    DateOfStarterCulture = as.Date(NA),
    Media = input$new_transformation_Media,
    Additives = input$new_transformation_Additives,
    LabBookNumber = input$new_transformation_LabBookNumber,
    PageNumber = input$new_transformation_PageNumber,
    Cultivar = input$new_transformation_Cultivar
  )

  dbWriteTable(pool, tb1, dt1, append = T)

  if(input$new_transformation_ReadyToCulture=="Yes"){
    dt2 <- data.frame(
      TransformationID = input$new_transformation_TransformationID,
      NumberOfCultures = input$new_transformation_NumberOfCultures,
      DateOfCulture = input$new_transformation_DateOfCultures,
      CulturedBy = input$new_transformation_CulturedBy,
      Comments = input$new_transformation_Comments,
      MediaForCultures = input$new_transformation_Media,
      AdditivesForCultures = input$new_transformation_Additives,
      LabBookNumberForCultures = input$new_transformation_LabBookNumber,
      PageNumberForCultures = input$new_transformation_PageNumber
    )

    dbWriteTable(pool, tb2, dt2, append = T)
    shinyalert("", "Record Added", type = "success")
  }

})
# # Refresh
# 
# observeEvent(input$new_transformation_Refresh,{
# 
# })

# Load Data
observeEvent(input$new_transformation_LoadData,{
  transformation <- tbl(pool, paste0(input$project_selected,"_tblTransformation")) %>%
    dplyr::filter(ExplantIdentity == input$new_transformation_ExplantIdentity) %>%
    collect()
  cultures <- tbl(pool, paste0(input$project_selected,"_tblCulturesTransformation")) %>%
    dplyr::filter(TransformationID == transformation$TransformationID[1])
    collect()

  # fill the fields
  updateSelectInput(session, "new_transformation_TransformationType", "Transformation Type", choices = c("", transformation$TransformationType))
  updateSelectInput(session,"new_transformation_AgrobacteriumStrains", "Agrobacterium Strains", choices = c("", transformation$AgrobacteriumStrains))
  updateSelectInput(session,"new_transformation_PermitType1", "Permit Type", choices = c("", transformation$PermitType))
  updateSelectInput(session,"new_transformation_PermitType2", "Permit Type", choices = c("", transformation$PermitType))
  updateSelectInput(session,"new_transformation_PermitType3", "Permit Type", choices = c("", transformation$PermitType))
  updateSelectInput(session,"new_transformation_PermitType4", "Permit Type", choices = c("", transformation$PermitType))
  updateSelectInput(session,"new_transformation_PermitType5", "Permit Type", choices = c("", transformation$PermitType))
  updateSelectInput(session,"new_transformation_PermitType6", "Permit Type", choices = c("", transformation$PermitType))
  updateSelectInput(session,"new_transformation_Media", "Media", choices = c("", transformation$Media))
  updateSelectInput(session,"new_transformation_Additives", "Additives", choices = c("", transformation$Additives))
  updateTextInput(session,"new_transformation_CulturedBy","Cultured By", value = transformation$CulturedBy)
  updateSelectInput(session,"new_transformation_ExplantIdentity", "Explant Identity", choices = c('', transformation$ExplantIdentity))

  updateNumericInput(session, "new_transformation_NumberOfCultures", "Number of Cultures", value = cultures$NumberOfCultures)
  updateDateInput(session,"new_transformation_DateOfCultures", "Date of Cultures", value = cultures$DateOfCulture)
  updateTextInput(session, "new_transformation_CulturedBy","Cultured By", value = cultures$CulturedBy)
  updateTextInput(session, "new_transformation_Comments", "Comments", value = cultures$Comments)
})
# # Update

observeEvent(input$new_transformation_Update,{
    tb1 <- paste0(input$project_selected,"_tblTransformation")
    tb2 <- paste0(input$project_selected,"_tblCulturesTransformation")

    TransformationID = input$new_transformation_TransformationID
    TransformationType = input$new_transformation_TransformationType
    AgrobacteriumStrains = input$new_transformation_AgrobacteriumStrains
    ExplantIdentity = input$new_transformation_ExplantIdentity
    CSCIdentity = ""
    VectorID1 = input$new_transformation_PlantSelection_Vector1
    VectorID2 = input$new_transformation_PlantSelection_Vector2
    IdentityType = input$new_transformation_IdentityType
    PermitType1 = input$new_transformation_PermitType1
    PermitType2 = input$new_transformation_PermitType2
    PermitType3 = input$new_transformation_PermitType3
    PermitType4 = input$new_transformation_PermitType4
    PermitType5 = input$new_transformation_PermitType5
    PermitType6 = input$new_transformation_PermitType6
    PermitNumber1 = input$new_transformation_PermitNumber1
    PermitNumber2 = input$new_transformation_PermitNumber2
    PermitNumber3 = input$new_transformation_PermitNumber3
    PermitNumber4 = input$new_transformation_PermitNumber4
    PermitNumber5 = input$new_transformation_PermitNumber5
    PermitNumber6 = input$new_transformation_PermitNumber6
    DateOfStarterCulture = as.Date(NA)
    Media = input$new_transformation_Media
    Additives = input$new_transformation_Additives
    LabBookNumber = input$new_transformation_LabBookNumber
    PageNumber = input$new_transformation_PageNumber
    Cultivar = input$new_transformation_Cultivar

    sql1 <- paste("UPDATE",tb1,"SET
                  TransformationType=?TransformationType,
                  AgrobacteriumStrains=?AgrobacteriumStrains,
                  ExplantIdentity=?ExplantIdentity,
                  CSCIdentity=?CSCIdentity,
                  VectorID1=?VectorID1,
                  VectorID2=?VectorID2,
                  IdentityType=?IdentityType,
                  PermitType1=?PermitType1,
                  PermitType2=?PermitType2,
                  PermitType3=?PermitType3,
                  PermitType4=?PermitType4,
                  PermitType5=?PermitType5,
                  PermitType6=?PermitType6,
                  PermitNumber1=?PermitNumber1,
                  PermitNumber2=?PermitNumber2,
                  PermitNumber3=?PermitNumber3,
                  PermitNumber4=?PermitNumber4,
                  PermitNumber5=?PermitNumber5,
                  PermitNumber6=?PermitNumber6,
                  DateOfStarterCulture=?DateOfStarterCulture,
                  Media=?Media,
                  Additives=?Additives,
                  LabBookNumber=?LabBookNumber,
                  PageNumber=?PageNumber,
                  Cultivar=?Cultivar,
                  WHERE TransformationID = ?TransformationID;")
    query1 <- sqlInterpolate(pool, sql1,
                            TransformationType=TransformationType,
                            AgrobacteriumStrains=AgrobacteriumStrains,
                            ExplantIdentity=ExplantIdentity,
                            CSCIdentity=CSCIdentity,
                            VectorID1=VectorID1,
                            VectorID2=VectorID2,
                            IdentityType=IdentityType,
                            PermitType1=PermitType1,
                            PermitType2=PermitType2,
                            PermitType3=PermitType3,
                            PermitType4=PermitType4,
                            PermitType5=PermitType5,
                            PermitType6=PermitType6,
                            PermitNumber1=PermitNumber1,
                            PermitNumber2=PermitNumber2,
                            PermitNumber3=PermitNumber3,
                            PermitNumber4=PermitNumber4,
                            PermitNumber5=PermitNumber5,
                            PermitNumber6=PermitNumber6,
                            DateOfStarterCulture=DateOfStarterCulture,
                            Media=Media,
                            Additives=Additives,
                            LabBookNumber=LabBookNumber,
                            PageNumber=PageNumber,
                            Cultivar=Cultivar,
                            TransformationID = TransformationID)
    dbExecute(pool, query1)

  if(input$new_transformation_ReadyToCulture=="Yes"){
    NumberOfCultures = input$new_transformation_NumberOfCultures
    DateOfCulture = input$new_transformation_DateOfCultures
    CulturedBy = input$new_transformation_CulturedBy
    Comments = input$new_transformation_Comments
    MediaForCultures = input$new_transformation_Media
    AdditivesForCultures = input$new_transformation_Additives
    LabBookNumberForCultures = input$new_transformation_LabBookNumber
    PageNumberForCultures = input$new_transformation_PageNumber

    sql2 <- paste("UPDATE",tb2,"SET
              NumberOfCultures=?NumberOfCultures,
              DateOfCulture=?DateOfCulture,
              CulturedBy=?CulturedBy,
              Comments=?Comments,
              MediaForCultures=?MediaForCultures,
              AdditivesForCultures=?AdditivesForCultures,
              LabBookNumberForCultures=?LabBookNumberForCultures,
              PageNumberForCultures=?PageNumberForCultures,
            WHERE  TransformationID = ?TransformationID;")
    query2 <- sqlInterpolate(pool, sql2,
                            NumberOfCultures=NumberOfCultures,
                            DateOfCulture=DateOfCulture,
                            CulturedBy=CulturedBy,
                            Comments=Comments,
                            MediaForCultures=MediaForCultures,
                            AdditivesForCultures=AdditivesForCultures,
                            LabBookNumberForCultures=LabBookNumberForCultures,
                            PageNumberForCultures=PageNumberForCultures,
                            TransformationID = TransformationID)
    dbExecute(pool, query2)
  }

    shinyalert("", "Transformation Updated", type = "success")

})

# # --------------------- UPDATING LAST SUBCULTURE -----------------------------------------------------------

## UPDATING LAST SUBCULTURE
observeEvent(input$transformation_module,{
  updateSelectInput(session, "updating_last_TransformationIdentity", "Transformation Identity", choices = c('', cultureTransformation()$TransformationID))
})


observeEvent(input$updating_last_transformation_LoadData,{
  dt <- cultureTransformation() %>%
    dplyr::filter(trimws(TransformationID) == trimws(input$updating_last_TransformationIdentity)) %>%
    dplyr::arrange(desc(lubridate::ydm(DateOfCulture))) %>%
    .[1,] # sort in descending order and select the most recent

  updateNumericInput(session, "updating_last_transformation_NumberOfCultures", "Number of Cultures", value = dt$NumberOfCultures)
  updateDateInput(session, "updating_last_transformation_DateOfCulture", "Date of Culture", value = dt$DateOfCulture)
  updateSelectInput(session, 'updating_last_transformation_CulturedBy', "Cultured By", choices = c(dt$CulturedBy), selected = dt$CulturedBy)
  updateSelectInput(session, "updating_last_transformation_Media", "Media", choices = c(dt$Media), selected = dt$Media)
  updateTextInput(session, "updating_last_transformation_Additives", "Additives", value = dt$Additives)
  updateNumericInput(session, "updating_last_transformation_LabBookNumber", "Lab Book Number", value = dt$LabBookNumber)
  updateNumericInput(session, "updating_last_transformation_PageNumber", "Page Number", value = dt$PageNumber)
  updateTextAreaInput(session, "updating_last_transformation_Comments", "Comments", value = dt$Comments)
})

# Update
observeEvent(input$updating_last_transformation_Update,{
  id <- trimws(input$updating_last_TransformationIdentity)
  NumberOfCultures = input$updating_last_transformation_NumberOfCultures
  Comments = input$updating_last_transformation_Comments

  tb <- paste0(input$project_selected, "_tblCulturesTransformation")
  sql <- paste("UPDATE ", tb, "SET NumberOfCultures = ?val1, Comments = ?val2 WHERE TransformationID = ?id1;")
  query <- sqlInterpolate(pool, sql, val1 = NumberOfCultures, val2 = Comments, id1 = id)
  dbExecute(pool, query)

  shinyalert("Success!", "Record updated", type = "success")
  reset("updating_last_transformation_Form")
})


# Clear
observeEvent(input$updating_last_transformation_Clear,{
  confirmSweetAlert(
    session = session,
    inputId = "updating_last_transformation_Clear_Confirm",
    type = "",
    title = "",
    text = "Do you really want to clear the fields?",
    btn_labels = c("Cancel", "Yes, Clear!"),
    btn_colors = c("#D3D3D3", "#DD6B55")
  )
})
observeEvent(input$updating_last_transformation_Clear_Confirm, {
  if(input$updating_last_transformation_Clear_Confirm == TRUE){
    reset("updating_last_transformation_Form")
  }
}, ignoreInit = TRUE)


# ## BATCH SUBCULTURING --------------------------------------------------------------------

observeEvent(input$transformation_module,{
  tb <- paste0(input$project_selected, "_tblTransformation")
  updateDateRangeInput(session, "batch_subculturingDateOfStarterCulture", "Date of Starter Culture",
                  start = min(lubridate::ymd(loadData(tb)$DateOfStarterCulture)), end = max(lubridate::ymd(loadData(tb)$DateOfStarterCulture)),
                  min = min(lubridate::ymd(loadData(tb)$DateOfStarterCulture)), max = max(lubridate::ymd(loadData(tb)$DateOfStarterCulture)))
})

# search

hot_values <- reactiveValues()

search_batch_subculturing_Input <- reactive({
  req(input$project_selected)
  
  tb <- paste0(input$project_selected, "_tblTransformation")
  dt <- loadData(tb) %>%
    dplyr::filter(between(lubridate::ymd(trimws(DateOfStarterCulture)), input$batch_subculturingDateOfStarterCulture[1],
                          input$batch_subculturingDateOfStarterCulture[2]))
  dt$Select <- FALSE
  dt$NumberOfCultures <- as.integer(NA)
  dt <- dt %>%
    dplyr::select(Select, NumberOfCultures, TransformationID, ExplantIdentity, CSCIdentity, VectorID1, VectorID2, TransformationType, DateOfStarterCulture, everything())
  dt
})

observe({
  if (!is.null(input$batch_subculturing_Table)) {
    hot_values[["previous"]] <- isolate(hot_values[["DF"]])
    DF = hot_to_r(input$batch_subculturing_Table)
  } else {
    if (is.null(hot_values[["DF"]]))
      DF <-  isolate(search_batch_subculturing_Input())
    else
      DF <- hot_values[["DF"]]
  }
  hot_values[["DF"]] <- DF
})

# table
observeEvent(input$batch_subculturingSearch,{
  DF <- hot_values[["DF"]]
  if(!is.null(DF)){
    output$batch_subculturing_Table <- renderRHandsontable({
      rhandsontable(DF, selectCallback = TRUE, readOnly = F, rowHeaders=F) %>%
        hot_table(stretchH = "all", columnSorting = TRUE)
    })
  }
})

## mandatory fields

batch_MandatoryFields <- c("batch_subculturing_CulturedBy",  "batch_subculturing_MediaForCultures", "batch_subculturing_LabBookNumberForCultures", "batch_subculturing_PageNumberForCultures")

observe({
  # check if all mandatory fields have a value
  mandatoryFilled <-
    vapply(batch_MandatoryFields,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != ""
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  # enable/disable the submit button
  shinyjs::toggleState(id = "batch_subculturing_Save", "Save Culture", condition = mandatoryFilled)
})

## save

 observeEvent(input$batch_subculturing_Save, {
  tb <- paste0(input$project_selected, "_tblTransformation")

  finalDF <- isolate(values[["DF"]]) %>%
    dplyr::select(TransformationID, NumberOfCultures)
  newDT <- data.frame(
    DateOfCulture = input$batch_subculturing_DateOfCulture,
    CulturedBy = input$batch_subculturing_CulturedBy,
    MediaForCultures = input$batch_subculturing_MediaForCultures,
    LabBookNumberForCultures = input$batch_subculturing_LabBookNumberForCultures,
    PageNumberForCultures = input$batch_subculturing_PageNumberForCultures,
    Comments = ifelse(nchar(input$batch_subculturing_Comments)>0,input$batch_subculturing_Comments,''),
    AdditivesForCultures = ifelse(!is.null(input$batch_subculturing_AdditivesForCultures), input$batch_subculturing_AdditivesForCultures,'')
    )
  dt <- merge.data.frame(finalDF, newDT)

  try(expr = dbWriteTable(conn = pool,   name = tb, value = dt, overwrite = F, append = T))
  #saveData(dt, tb)
  shinyalert("Success!", "Record Saved", type = "success")

})
# 
# # fname = tempfile(fileext = ".csv")
# # observe({
# #   # remove button and isolate to update file automatically
# #   # after each table change
# #   input$batch_subculturing_Save
# #   hot = isolate(input$batch_subculturing_Table)
# #   if (!is.null(batch_subculturing_Table)) {
# #     write.csv(hot_to_r(input$batch_subculturing_Table), fname)
# #     print(fname)
# #   }
# # })
# #
# # output$batch_subculturing_Table <- renderRHandsontable({
# #   if (!is.null(input$batch_subculturing_Table)) {
# #     DF = hot_to_r(input$batch_subculturing_Table)
# #   } else {
# #     DF = read.csv("mtcars.csv", stringsAsFactors = FALSE)
# #   }
# # 
# #   rhandsontable(DF) %>%
# #     hot_table(highlightCol = TRUE, highlightRow = TRUE)
# # })
# 
# #
# #
# ## LABELS ------------------------------------------------------------------------------

observeEvent(input$transformation_module,{
  tb <- paste0(input$project_selected, "_tblCulturesTransformation")
    updateDateInput(session, "labelsTransformation_SelectDate", "", value = min(lubridate::ymd(loadData(tb)$DateOfCulture)),
              min = min(lubridate::ymd(loadData(tb)$DateOfCulture)), max = max(lubridate::ymd(loadData(tb)$DateOfCulture)))
})

labelsTransformation_Input <- reactive({
  #req(input$labelsTransformation_SelectDate)
  dt <- loadData(paste0(input$project_selected, "_tblCulturesTransformation")) %>%
    dplyr::filter(lubridate::ymd(DateOfCulture) == input$labelsTransformation_SelectDate)
  dt
})

observeEvent(input$labelsTransformation_LoadData,{
  output$labelsTransformation_Table <- renderRHandsontable({
    rhandsontable(labelsTransformation_Input(), selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
      hot_table(stretchH = "all", columnSorting = TRUE)
  })
})

# Export to Excel
output$labelsTransformation_ExportToExcel <- downloadHandler(
  filename = function(){paste("Transformation Labels-", Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(labelsTransformation_Input(), path = file, col_names = T, format_headers = T )
  }
)

# Barcode

labelsTransformation_ExcelLabels <- reactive({
  dt <- labelsTransformation_Input() %>%
    dplyr::select(TransformationID, NumberOfCultures)
  dt = data.frame(dt)
  dt = dt[rep(row.names(dt), dt$NumberOfCultures),]
  dt <- dt %>%
    dplyr::select(TransformationID) %>%
    dplyr::mutate(label = TransformationID)
  dt
})

# pdf barcodes
output$labelsTransformation_barcodeLabels <- downloadHandler(
  filename = function(){paste("Transformation Labels-", Sys.time(), '.pdf')},

  content = function(file) {
    pdf(file, width=8.0, height = 11, paper = 'letter', pagecentre=T) # right align width=6.0 # left width=2.0,
    par(mfrow=c(10, 4),mar=c(2.0,2.0,2.0,2.0), oma=c(2,2,2,2)) # right align mar=c(0,30,3,0)
    for(i in 1:(nrow(labelsTransformation_ExcelLabels()))){
      image(qrencode_raster(as.character(labelsTransformation_ExcelLabels()[i,1])), # QRcode
            cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F,
            xlab="", ylab="", subtitle = mtext(paste(as.character(labelsTransformation_ExcelLabels()[i,1])), side = 1, line = 0,
                                               outer = F, at = NA, adj = 0, padj = 0.5, cex = 1, col = 1, las=1, font = 10)
      )

    }
    dev.off()
  }
)

# Excel barcodes
output$labelsTransformation_excelLabels <- downloadHandler(
  filename = function(){paste("Labels-", Sys.time(), '.xls')},
  content = function(file) {
    writexl::write_xlsx(labelsTransformation_ExcelLabels(), path = file, col_names = T, format_headers = T )
  }
)