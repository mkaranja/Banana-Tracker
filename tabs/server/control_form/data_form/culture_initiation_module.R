
tab_files <- list.files(path = "tabs/server/control_form/data_form/culture_initiation_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}




observeEvent(input$culture_initiation_module,{

 showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;","Culture Initiation Module"),
   
   tabsetPanel( type = "pills", selected = "new_culture_initiation",
     search_culture_initiation,
     new_culture_initiation,
     updating_last_subculture
     
   ),easyClose = F, size = "l"
 ))
 
})
 

###################################################### SERVER ######################################################

#***************************************************** 1. New Culture Initiation ************************************

load_mfc <- reactive({
   pool %>% tbl("tblMFC") %>% collect()
})

mfcV <- reactiveValues()

observeEvent(input$new_culture_initiation_GenerateIdentity, {
   req(input$new_culture_initiation_CultureType)
   mfc <- tbl(pool,"tblMFC") %>%
      collect() %>%
      filter(substr(ExplantIdentity, 1,3) == input$new_culture_initiation_CultureType, lubridate::year(DateOfStarterCulture) == as.integer(input$new_culture_initiation_Year)) %>%
      arrange(desc(ExplantIdentity)) %>%
      select(ExplantIdentity)
   
   
   val <- ifelse(nrow(mfc)>0, stringr::str_pad((as.integer(substr(readr::parse_number(mfc$ExplantIdentity[1]),3,8))+1), 5, side = "left", pad = "0"),
                 stringr::str_pad("1", 5, side = "left", pad = "0")
                )
   
   validate(need(nchar(input$new_culture_initiation_Year) == 4, "Please add the year"))
   year <- as.numeric(strsplit(as.character(input$new_culture_initiation_Year), "")[[1]])
   yy <- paste0(year[3],year[4])
   value <- paste0(input$new_culture_initiation_CultureType, yy, val)
   updateTextInput(session, inputId = "new_culture_initiation_ExplantIdentity", "", value = value)
   
   updateTextInput(session, "new_culture_initiation_ExplantIdentityType", "", value = input$new_culture_initiation_CultureType)
   
})



new_culture_initiation_MandatoryFields <- c("new_culture_initiation_ExplantIdentity","new_culture_initiation_ExplantIdentityType","new_culture_initiation_Cultivar","new_culture_initiation_CultivarConfirmed",
                                            "new_culture_initiation_Source","new_culture_initiation_VirusIndexed","new_culture_initiation_Media","new_culture_initiation_LabBookNumber",
                                            "new_culture_initiation_PageNumber", "new_culture_initiation_PermitNumber1")

 observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
       vapply(new_culture_initiation_MandatoryFields,
              function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
              },
              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "new_culture_initiation_SaveStarterCulture", "Save Starter Culture", condition = mandatoryFilled)
 })


 new_culture_initiation_Form_Data <- reactive({
    req(input$new_culture_initiation_ExplantIdentity)
    req(input$new_culture_initiation_ExplantIdentityType)
    req(input$new_culture_initiation_Cultivar)
    req(input$new_culture_initiation_CultivarConfirmed)
    req(input$new_culture_initiation_Source)
    req(input$new_culture_initiation_VirusIndexed)
    req(input$new_culture_initiation_DateOfStarterCulture)
    req(input$new_culture_initiation_Media)
    req(input$new_culture_initiation_LabBookNumber)
    req(input$new_culture_initiation_PageNumber)
    
    df <- data.frame(
       ExplantIdentity = input$new_culture_initiation_ExplantIdentity, 
       ExplantIdentityType = input$new_culture_initiation_ExplantIdentityType, 
       Cultivar = input$new_culture_initiation_Cultivar,
       CultivarConfirmed = input$new_culture_initiation_CultivarConfirmed, 
       CultivarConfirmedComments = ifelse(nchar(input$new_culture_initiation_CultivarConfirmedComments)>0, input$new_culture_initiation_CultivarConfirmedComments,''),
       Source  = input$new_culture_initiation_Source,
       VirusIndexed = input$new_culture_initiation_VirusIndexed,
       PermitType1 = ifelse(nchar(input$new_culture_initiation_PermitType1)>0, input$new_culture_initiation_PermitType1,''),
       PermitType2 = ifelse(nchar(input$new_culture_initiation_PermitType2)>0, input$new_culture_initiation_PermitType2,''),
       PermitType3 = ifelse(nchar(input$new_culture_initiation_PermitType3)>0, input$new_culture_initiation_PermitType3,''),
       PermitType4 = ifelse(nchar(input$new_culture_initiation_PermitType4)>0, input$new_culture_initiation_PermitType4,''),
       PermitType5 = ifelse(nchar(input$new_culture_initiation_PermitType5)>0, input$new_culture_initiation_PermitType5,''),
       PermitType6 = ifelse(nchar(input$new_culture_initiation_PermitType6)>0, input$new_culture_initiation_PermitType6,''),
       PermitNumber1 = ifelse(nchar(input$new_culture_initiation_PermitNumber1)>0, input$new_culture_initiation_PermitNumber1,''),
       PermitNumber2 = ifelse(nchar(input$new_culture_initiation_PermitNumber2)>0, input$new_culture_initiation_PermitNumber2,''),
       PermitNumber3 = ifelse(nchar(input$new_culture_initiation_PermitNumber3)>0, input$new_culture_initiation_PermitNumber3,''),
       PermitNumber4 = ifelse(nchar(input$new_culture_initiation_PermitNumber4)>0, input$new_culture_initiation_PermitNumber4,''),
       PermitNumber5 = ifelse(nchar(input$new_culture_initiation_PermitNumber5)>0, input$new_culture_initiation_PermitNumber5,''),
       PermitNumber6 = ifelse(nchar(input$new_culture_initiation_PermitNumber6)>0, input$new_culture_initiation_PermitNumber6,''),
       DateOfStarterCulture = input$new_culture_initiation_DateOfStarterCulture,
       Media = input$new_culture_initiation_Media,
       Additives = ifelse(!is.null(input$new_culture_initiation_Additives), input$new_culture_initiation_Additives,''), 
       LabBookNumber = input$new_culture_initiation_LabBookNumber, 
       PageNumber = input$new_culture_initiation_PageNumber,
       VirusIndexedDate = input$new_culture_initiation_VirusIndexedDate, 
       VirusIndexedBy = ifelse(nchar(input$new_culture_initiation_VirusIndexedBy)>0, input$new_culture_initiation_VirusIndexedBy, '')
    )
    df
 })
 
observeEvent(input$new_culture_initiation_SaveStarterCulture,{
   df <- new_culture_initiation_Form_Data()
   dt <- mfcV$Data <- load_mfc()
   
    if((df$ExplantIdentity %in% dt$ExplantIdentity)==FALSE){
       dbWriteTable(pool, 'tblMFC', df, append = T)
       #showNotification("New Record Added Successfully")
       shinyalert("Success!", "Record Added", type = "success")
    }
   ## Reset form if ExplantID changes
   # reset("new_culture_initiation_Form")
   
}, ignoreInit = TRUE, once = TRUE)


## Sub Culture
new_culture_initiation_SubCulture_MandatoryFields <- c(new_culture_initiation_MandatoryFields, "new_culture_initiation_NumberOfCultures", 
                                                       "new_culture_initiation_CulturedBy")

observe({
   # check if all mandatory fields have a value
   mandatoryFilled <-
      vapply(new_culture_initiation_SubCulture_MandatoryFields,
             function(x) {
                !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
   mandatoryFilled <- all(mandatoryFilled)
   
   # enable/disable the submit button
   shinyjs::toggleState(id = "new_culture_initiation_SaveStarterCultureAndSubCulture", "Save Starter Culture and Sub Culture", condition = mandatoryFilled)
})

observeEvent(input$new_culture_initiation_SaveStarterCultureAndSubCulture,{
   
   req(input$new_culture_initiation_ExplantIdentity)
   req(input$new_culture_initiation_ExplantIdentityType)
   req(input$new_culture_initiation_Cultivar)
   req(input$new_culture_initiation_CultivarConfirmed)
   req(input$new_culture_initiation_Source)
   req(input$new_culture_initiation_VirusIndexed)
   req(input$new_culture_initiation_DateOfStarterCulture)
   req(input$new_culture_initiation_Media)
   req(input$new_culture_initiation_LabBookNumber)
   req(input$new_culture_initiation_PageNumber)
   req(input$new_culture_initiation_NumberOfCultures)
   req(input$new_culture_initiation_DateOfCultures)
   req(input$new_culture_initiation_CulturedBy)
   df <- data.frame(
      ExplantIdentity = input$new_culture_initiation_ExplantIdentity, 
      ExplantIdentityType = input$new_culture_initiation_ExplantIdentityType, 
      Cultivar = input$new_culture_initiation_Cultivar,
      CultivarConfirmed = input$new_culture_initiation_CultivarConfirmed, 
      CultivarConfirmedComments = ifelse(nchar(input$new_culture_initiation_CultivarConfirmedComments)>0, input$new_culture_initiation_CultivarConfirmedComments,''),
      Source  = input$new_culture_initiation_Source,
      VirusIndexed = input$new_culture_initiation_VirusIndexed,
      PermitType1 = ifelse(nchar(input$new_culture_initiation_PermitType1)>0, input$new_culture_initiation_PermitType1,''),
      PermitType2 = ifelse(nchar(input$new_culture_initiation_PermitType2)>0, input$new_culture_initiation_PermitType2,''),
      PermitType3 = ifelse(nchar(input$new_culture_initiation_PermitType3)>0, input$new_culture_initiation_PermitType3,''),
      PermitType4 = ifelse(nchar(input$new_culture_initiation_PermitType4)>0, input$new_culture_initiation_PermitType4,''),
      PermitType5 = ifelse(nchar(input$new_culture_initiation_PermitType5)>0, input$new_culture_initiation_PermitType5,''),
      PermitType6 = ifelse(nchar(input$new_culture_initiation_PermitType6)>0, input$new_culture_initiation_PermitType6,''),
      PermitNumber1 = ifelse(nchar(input$new_culture_initiation_PermitNumber1)>0, input$new_culture_initiation_PermitNumber1,''),
      PermitNumber2 = ifelse(nchar(input$new_culture_initiation_PermitNumber2)>0, input$new_culture_initiation_PermitNumber2,''),
      PermitNumber3 = ifelse(nchar(input$new_culture_initiation_PermitNumber3)>0, input$new_culture_initiation_PermitNumber3,''),
      PermitNumber4 = ifelse(nchar(input$new_culture_initiation_PermitNumber4)>0, input$new_culture_initiation_PermitNumber4,''),
      PermitNumber5 = ifelse(nchar(input$new_culture_initiation_PermitNumber5)>0, input$new_culture_initiation_PermitNumber5,''),
      PermitNumber6 = ifelse(nchar(input$new_culture_initiation_PermitNumber6)>0, input$new_culture_initiation_PermitNumber6,''),
      DateOfStarterCulture = input$new_culture_initiation_DateOfStarterCulture,
      Media = input$new_culture_initiation_Media,
      Additives = ifelse(!is.null(input$new_culture_initiation_Additives), input$new_culture_initiation_Additives,''), 
      LabBookNumber = input$new_culture_initiation_LabBookNumber, 
      PageNumber = input$new_culture_initiation_PageNumber,
      VirusIndexedDate = input$new_culture_initiation_VirusIndexedDate, 
      VirusIndexedBy = ifelse(nchar(input$new_culture_initiation_VirusIndexedBy)>0, input$new_culture_initiation_VirusIndexedBy, ''),
      NumberOfCultures = input$new_culture_initiation_NumberOfCultures,
      DateOfCultures = input$new_culture_initiation_DateOfCultures,
      CulturedBy = input$new_culture_initiation_CulturedBy,
      Comments = ifelse(!is.null(input$new_culture_initiation_Comments1), input$new_culture_initiation_Comments1,'')
   )
   
   df1 <- df[,c(1:26)]
   df2 <- df[,c(1,27:30, 21:24)]
   colnames(df2[,6:9]) <- c("MediaForCultures", "AdditivesForCultures","LabBookNumberForCultures","PageNumberForCultures")
   dt <- mfcV$Data <- load_mfc()
   
   if((df$ExplantIdentity %in% dt$ExplantIdentity)==FALSE){
      dbWriteTable(pool, 'tblMFC', df1, append = T)
      dbWriteTable(pool, 'tblCultures', df2, append = T)
      shinyalert("Success!", "Record Added", type = "success")
      # showNotification("New Record Added Successfully")
   }
   
   # Fields to Export to Excel
   f <- c("ExplantIdentity","ExplantIdentityType","Cultivar","Source","DateOfStarterCulture","Media","Additives","LabBookNumber","PageNumber","NumberOfCultures","DateOfCultures","CulturedBy")
   output$select_fields_to_be_exported_to_excel_Output <- renderUI({
      awesomeCheckboxGroup(inputId = "select_fields_to_be_exported_to_excel",label = "",
                           choices = c(f), selected = f[c(1:5,11,12)], inline = FALSE)
   })
   
   ## Reset form if ExplantID changes
   # reset("new_culture_initiation_Form")
   
}, ignoreInit = TRUE, once = TRUE)

## Export to excel
new_culture_initiation_SelectFieldToExportToExcel_Input <- reactive({
   dt <- new_culture_initiation_Form_Data() %>%
      data.frame()
   # mfcV$Data <- load_mfc() %>%
   # dplyr::filter(ExplantIdentity == input$new_culture_initiation_ExplantIdentity)
   
   if (length(input$select_fields_to_be_exported_to_excel) > 0){
      dt = dt %>% 
         dplyr::select(!!!input$select_fields_to_be_exported_to_excel)
   }
   
   dt[rep(row.names(dt), input$new_culture_initiation_NumberOfCultures),]
   
})

output$new_culture_initiation_SelectFieldToExportToExcel <- downloadHandler(
   filename = function(){paste(input$new_culture_initiation_ExplantIdentity, Sys.time(), '.xls')},
   content = function(file) {
      writexl::write_xlsx(new_culture_initiation_SelectFieldToExportToExcel_Input(), path = file, col_names = T, format_headers = T )
   }
)


## Clear
observeEvent(input$new_culture_initiation_ClearForm,{
   shinyalert(
      title = "",
      callbackR = function(x) {
         if(x != FALSE)
         reset("new_culture_initiation_Form")
      },
      text = "Do you really want to clear the form?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonCol = '#DD6B55',
      confirmButtonText = 'Yes, clear!'
   )
})

## Refresh
observeEvent(input$new_culture_initiation_Refresh,{
   reset("new_culture_initiation_Form")
})

observeEvent(input$new_culture_initiation_FormToPicture, {
   js$winprint()
})


observeEvent(input$new_culture_initiation_MFC_SCP_CSC_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})


observeEvent(input$new_culture_initiation_Exit, {
   
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

#****************************************************** 2. Search Culture Initiation ****************************************************************************

search_culture_initiation_ResultsTable_Input <- reactive({
   if(input$search_culture_initiation_Identify !=''){
      dt <- mfc %>%
         dplyr::filter(ExplantIdentity == input$search_culture_initiation_Identify)
   }else if(input$search_culture_initiation_Source!=''){
      dt <- mfc %>%
         dplyr::filter(Source == input$search_culture_initiation_Source)
   }else if(input$search_culture_initiation_Cultivar !=''){
      dt <- mfc %>%
         dplyr::filter(Cultivar == input$search_culture_initiation_Cultivar)
   } else if(input$search_culture_initiation_CultivarConfirmed !=''){
      dt <- mfc %>%
         dplyr::filter(CultivarConfirmed == input$search_culture_initiation_CultivarConfirmed)
   }else if(input$search_culture_initiation_VirusIndexed !=''){
      dt <- mfc %>%
         dplyr::filter(VirusIndexed == input$search_culture_initiation_VirusIndexed)
   }else if(!is.null(input$search_culture_initiation_DateOfStarterCulture)){
      dt <- mfc %>%
         dplyr::filter(between(lubridate::ymd(DateOfStarterCulture), input$search_culture_initiation_DateOfStarterCulture[1], input$search_culture_initiation_DateOfStarterCulture[2]))
   }
   else {
      dt <- data.frame(t(names(mfc)))
      colnames(dt) <- names(mfc)
      dt <- dt[-1,]
   }
   dt
})


# Table results
output$search_culture_initiation_ResultsTable_Output <- renderUI({
    # Search Results Table
    if(input$search_culture_initiation_SearchDeletedMFC == FALSE){
       rHandsontableOutput("search_culture_initiation_ResultsTable")
    } else {
       # Search Deleted MFC 
       rHandsontableOutput("search_culture_initiation_SearchDeletedMFC_ResultsTable")
    }
})
 output$txt11 <- renderPrint({
    input$search_culture_initiation_SearchDeletedMFC
 })
# Hide ExplantIdentity

# observe({
#    if(input$search_culture_initiation_Identify == '' & input$search_culture_initiation_Source =='' & input$search_culture_initiation_Cultivar =='' & 
#       input$search_culture_initiation_CultivarConfirmed =='' & input$search_culture_initiation_VirusIndexed =='' & is.null(input$search_culture_initiation_DateOfStarterCulture)){
#       
#       shinyjs::hide("search_culture_initiation_ResultsTable_Output")
#       shinyjs::hide("search_culture_initiation_Culture_Form1")
#    } 
# })

# Search Results
observeEvent(input$search_culture_initiation_ActionSearch,{
   
    if(input$search_culture_initiation_Identify == '' & input$search_culture_initiation_Source =='' & input$search_culture_initiation_Cultivar =='' & 
       input$search_culture_initiation_CultivarConfirmed =='' & input$search_culture_initiation_VirusIndexed =='' & is.null(input$search_culture_initiation_DateOfStarterCulture)){
          shinyalert("Oops!", "Select one of the search criteria", type = "warning")
    } else {
      tryCatch({
         output$search_culture_initiation_ResultsTable <- renderRHandsontable({
            # dt <- search_culture_initiation_ResultsTable_Input() 
            dt <- data.frame(t(names(mfc)))
            colnames(dt) <- names(mfc)
            dt <- dt[-1,]
            rhandsontable(dt, selectCallback = T, readOnly = T, rowHeaders=F) %>%
               hot_table(stretchH = "all")
         })
      })
   }
})


 observeEvent(input$search_culture_initiation_ResultsTable_select$select$r,{
    dv <- search_culture_initiation_ResultsTable_Input()
    df <- cultures
    r <- input$search_culture_initiation_ResultsTable_select$select$r
    c <- dv[r,'ExplantIdentity']
    if(length(c)>0){
    dt = df[df$ExplantIdentity == c$ExplantIdentity, ]
    
      output$search_culture_initiation_ExplantIdentiyResultsTable <- renderRHandsontable({
         rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
            hot_table(stretchH = "all", columnSorting = TRUE)
       })
    }
    ## update Explant Identity
    # updateSelectInput(session, "search_culture_initiation_Identify", "Identify", choices = c('', mfc$ExplantIdentity), selected = c$ExplantIdentity)
})


observeEvent(input$search_culture_initiation_ActionClearForm,{
   shinyalert(
      title = "",
      callbackR = function(x) {
         if(x != FALSE)
            reset("search_culture_initiation_Form")
         
         # Hide ExplantIdentity
         shinyjs::hide("search_culture_initiation_ResultsTable_Output")
         shinyjs::hide("search_culture_initiation_Culture_Form1")
      },
      text = "Do you really want to clear the form?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonCol = '#DD6B55',
      confirmButtonText = 'Yes, clear!'
   )
  # input$search_culture_initiation_ResultsTable_select$select$r <- NULL
   
})

observeEvent(input$search_culture_initiation_ActionCulture,{
   if(input$search_culture_initiation_Identify !=''){
      id <- input$search_culture_initiation_Identify
      
   output$search_culture_initiation_ActionCulture_Output <- renderUI({
        
      div(
         panel_div(class_type = "default",
            content = tags$div(
               
                column(3, 
                   div(id = "search_culture_initiation_Culture_Form",
                       disabled(textInput("search_culture_initiation_SelectedIdentity","Selected Identity", value = id, width = "100%")),
                       numericInput("search_culture_initiation_NumberOfCultures",labelMandatory("Number of Cultures"), value = NULL, width = "100%"),
                       dateInput("search_culture_initiation_DateOfCulture",labelMandatory("Date of Cultures"), width = "100%"),
                       selectInput("search_culture_initiation_CulturedBy",labelMandatory("Cultured By"), choices = c('', cultured_by$CulturedBy), width = "100%"),
                       selectInput("search_culture_initiation_MediaForCultures",labelMandatory("Media"), choices = c('', media$Media), width = "100%"),
                       numericInput("search_culture_initiation_LabBookNumberForCultures",labelMandatory("Lab Book Number"), value = NULL, width = "100%"),
                       numericInput("search_culture_initiation_PageNumberForCultures",labelMandatory("Page Number"), value = NULL, width = "100%"),
                       textInput("search_culture_initiation_Comments", "Comments", width = "100%")
                    )
                  ), 
                  column(1, br(), br(),br(),br(), br(),br(), 
                         awesomeCheckboxGroup(inputId = "search_culture_initiation_AdditivesForCultures", 
                                              label = "Additives", choices = c(additives$Additives), selected = NULL, status = "info")),
                  column(3, 
                         conditionalPanel(
                            condition = "input.search_culture_initiation_SaveRecord",
                            awesomeCheckboxGroup(inputId = "search_culture_initiation_SelectTheFields", label = "Select the Fields",
                                                 choices = c(names(mfc)), selected = names(mfc), status = "info"), br(), br(),
                            downloadBttn("search_culture_initiation_ExportToExcel", "Export to Excel", style = "simple", size = "sm", color = "primary")
                         )
                  ),
                  column(12, offset = 6, actionBttn("search_culture_initiation_SaveRecord", "Save Record", style = "fill", size = "sm", color = "primary"))
            ))
      )
   })
   }else {
      showNotification("Select an Identity", type = "error")
   }
})

observeEvent(input$search_culture_initiation_ActionDelete,{
   dv <- search_culture_initiation_ResultsTable_Input()
   r <- input$search_culture_initiation_ResultsTable_select$select$r
   c <- dv[r,'ExplantIdentity']
   id <- c$ExplantIdentity
   sql <- "DELETE FROM tblMFC WHERE ExplantIdentity = ?id1;"
   query <- sqlInterpolate(pool, sql, id1 = id)
   dbExecute(pool, query)
   
   # Save deleted MFC details
   try(expr = dbWriteTable( conn = pool,   name = "tblDeletedMFC", value = c, overwrite = F, append = T))
})

# Search deleted MFC

output$search_culture_initiation_SearchDeletedMFC_ResultsTable <- renderRHandsontable({
   dt <- deletedMFC
   rhandsontable(dt, selectCallback = TRUE, readOnly = T, rowHeaders=F) %>%
      hot_table(stretchH = "all", columnSorting = TRUE)
})

 

 # Export to Excel
search_culture_initiation_Culture_Form_Data <- reactive({
  
   req(input$search_culture_initiation_SelectedIdentity)
   req(input$search_culture_initiation_NumberOfCultures)
   req(input$search_culture_initiation_DateOfCulture)
   req(input$search_culture_initiation_CulturedBy)
   req(input$search_culture_initiation_MediaForCultures)
   req(input$search_culture_initiation_LabBookNumberForCultures)
   req(input$search_culture_initiation_PageNumberForCultures)
   
   dt <-data.frame(
         ExplantIdentity = input$search_culture_initiation_SelectedIdentity,
         NumberOfCultures = input$search_culture_initiation_NumberOfCultures,
         DateOfCulture = input$search_culture_initiation_DateOfCulture,
         CulturedBy = input$search_culture_initiation_CulturedBy,
         MediaForCultures = input$search_culture_initiation_MediaForCultures,
         LabBookNumberForCultures = input$search_culture_initiation_LabBookNumberForCultures,
         PageNumberForCultures = input$search_culture_initiation_PageNumberForCultures,
         comments = ifelse(nchar(input$search_culture_initiation_Comments)>0,input$search_culture_initiation_Comments,''), 
         AdditivesForCultures = ifelse(nchar(input$search_culture_initiation_AdditivesForCultures)>0,input$search_culture_initiation_AdditivesForCultures,'')
      )#ifelse(nchar(input$new_culture_initiation_PermitNumber6)>0, 
      dt
})
 search_culture_initiation_ExportToExcel_Input <- reactive({
    dt <- search_culture_initiation_Culture_Form_Data() %>%
       data.frame()
    # mfcV$Data <- load_mfc() %>%
    # dplyr::filter(ExplantIdentity == input$new_culture_initiation_ExplantIdentity)
    
    if (length(input$search_culture_initiation_SelectTheFields) > 0){
       dt = dt %>% 
          dplyr::select(!!!input$search_culture_initiation_SelectTheFields)
    }
    
    dt[rep(row.names(dt), input$search_culture_initiation_NumberOfCultures),]
    
 })
 output$search_culture_initiation_ExportToExcel <- downloadHandler(
       filename = function(){paste(input$search_culture_initiation_SelectedIdentity, Sys.time(), '.xls')},
       content = function(file) {
          writexl::write_xlsx(search_culture_initiation_ExportToExcel_Input(), path = file, col_names = T, format_headers = T )
       }
   )
 
 
  # observeEvent(input$search_culture_initiation_SearchDeletedMFC,{
  #    # Hide Explant Identity 
  #    shinyjs::hide("search_culture_initiation_Culture_Form1")
  #    
  # })
  # 
 
 
 observeEvent(input$search_culture_initiation_FormToPicture, {
    js$winprint()
 })
 observeEvent(input$search_culture_initiation_MFC_SCP_CSC_ControlForm, {
    updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 observeEvent(input$search_culture_initiation_Exit, {
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
 
 #****************************************************** 3. Update Culture Initiation ****************************************************************************
 
 update_last_subculture_input <- reactive({
    mfc %>%
       filter(trimws(ExplantIdentity) == trimws(input$updating_last_subculture_CultureInitiationIdentity))
 })
 
 
 update_last_subculture_cultures_input <- reactive({
    cultures %>%
      filter(trimws(ExplantIdentity) == trimws(input$updating_last_subculture_CultureInitiationIdentity))
 })
 
 update_last_subculture_values <- reactiveValues()
 
 observeEvent(input$updating_last_subculture_LoadData,{
    
    update_last_subculture_values$Data <- update_last_subculture_cultures_input()
    dt <- update_last_subculture_values$Data
     
     updateNumericInput(session, "updating_last_subculture_NumberOfCultures", "Number of Cultures", value = dt$NumberOfCultures)
     updateDateInput(session, "updating_last_subculture_DateOfCulture", "Date of Culture", value = update_last_subculture_input()$DateOfStarterCulture)
     updateSelectInput(session, 'updating_last_subculture_CulturedBy', "Cultured By", choices = c(update_last_subculture_cultures_input()$CulturedBy), selected = update_last_subculture_cultures_input()$CulturedBy)
     updateSelectInput(session, "updating_last_subculture_Media", "Media", choices = c(update_last_subculture_input()$Media), selected = update_last_subculture_input()$Media)
     updateTextInput(session, "updating_last_subculture_Additives", "Additives", value = update_last_subculture_input()$Additives)
     updateNumericInput(session, "updating_last_subculture_LabBookNumber", "Lab Book Number", value = update_last_subculture_input()$LabBookNumber)
     updateNumericInput(session, "updating_last_subculture_PageNumber", "Page Number", value = update_last_subculture_input()$PageNumber)
     updateTextAreaInput(session, "updating_last_subculture_Comments","Comments", value = dt$Comments)
            
  })
  
 # Comments nchars
 observeEvent(input$updating_last_subculture_Comments,{
    if(nchar(input$updating_last_subculture_Comments)>1000){
       updateTextInput(session,'updating_last_subculture_Comments',value=substr(input$updating_last_subculture_Comments,1,1000))
       showNotification("Comments: Max length is 1000 characters", type = "error")
    }
 })
 
 
  observeEvent(input$updating_last_subculture_Update, {
     
     id <- trimws(input$updating_last_subculture_CultureInitiationIdentity)
     NumberOfCultures = input$updating_last_subculture_NumberOfCultures
     Comments = input$updating_last_subculture_Comments
     
     sql <- "UPDATE tblCultures SET NumberOfCultures = ?val1, Comments = ?val2 WHERE ExplantIdentity = ?id1;"
     query <- sqlInterpolate(pool, sql, val1 = NumberOfCultures, val2 = Comments, id1 = id)
     dbExecute(pool, query)
     
     update_last_subculture_values$Data <- update_last_subculture_cultures_input()
     update_last_subculture_values$Data
     
     shinyjs::js$refresh()
  })
  
  observeEvent(input$updating_last_subculture_Clear,{
     reset("updating_last_subculture_Form")
  })
  
  
  observeEvent(input$updating_last_subculture_FormToPicture, {
     js$winprint()
  })
  
  
  observeEvent(input$updating_last_subculture_MFC_SCP_CSC_ControlForm,{
     updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
  })
  
  
  observeEvent(input$updating_last_subculture_Exit, {
     
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
  
  
  