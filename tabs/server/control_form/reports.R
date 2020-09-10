
tab_files <- list.files(path = "tabs/server/control_form/reports", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))


## Culture Initiation Reports

observeEvent(input$culture_initiation_reports,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;","Culture Initiation Reports"),
                        
                        culture_initiation_reports,
                        easyClose = F, size = "l"
  ))
  
})

## Cell Suspension Culture Reports 
observeEvent(input$cell_suspension_culture_reports,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;","Cell Suspension Culture Reports"),
                        
                        cell_suspension_culture_reports,
                        easyClose = F, size = "l"
  ))
  
})

## ------------------------ 1. Culture Initiation Reports

output$culture_initiation_reports_Fields_Output <- renderUI({
  mfc <- tbl(pool, "tblMFC") %>% collect() 
  
  div(
    column(2, selectInput("culture_initiation_reports_ExplantIdentify", "Explant Identify", choices = c('', unique(mfc$ExplantIdentity)), width = "100%")),
    column(2, selectInput("culture_initiation_reports_Source", "Source", choices = c('', mfc$Source), width = "100%")),
    column(2, selectInput("culture_initiation_reports_Cultivar", "Cultivar", choices = c('', mfc$Cultivar), width = "100%")),
    column(2, selectInput("culture_initiation_reports_CultivarConfirmed", "Cultivar Confirmed", choices = c('','Yes','No'), width = "100%")),
    column(2, selectInput("culture_initiation_reports_VirusIndexed", "Virus Indexed", choices = c('','Yes','No'), width = "100%")),
    column(2, dateRangeInput("culture_initiation_reports_DateOfStarterCulture", "Date of Starter culture", 
                             start = min(mfc$DateOfStarterCulture), end = max(mfc$DateOfStarterCulture),
                             min = min(mfc$DateOfStarterCulture), max = max(mfc$DateOfStarterCulture)))
  )
})


output$culture_initiation_reports_Fields_Output2 <- renderUI({
  cultures <- tbl(pool, "tblCultures") %>% collect() 
  
 div(
   column(2, dateRangeInput("culture_initiation_reports_DateOfSubCulture", "Date of Sub Culture", width = "100%", 
                            start = min(cultures$DateOfCulture), end = max(cultures$DateOfCulture),
                            min = min(cultures$DateOfCulture), max = max(cultures$DateOfCulture))),
   column(2, selectInput(inputId = "culture_initiation_reports_CulturedBy",label = "Cultured By", choices = c('', cultures$CulturedBy), width = "100%"))
   
 )  
})

culture_initiation_reports_Input <- reactive({
  
  dt <- tbl(pool,"tblMFC") %>%
    dplyr::left_join(tbl(pool,"tblCultures")) %>%
    dplyr::select(ExplantIdentity, ExplantIdentityType, Cultivar, CultivarConfirmed, Source, DateOfStarterCulture,
           Media, Additives, LabBookNumber, PageNumber,VirusIndexed, VirusIndexedDate, VirusIndexedBy,
           NumberOfCultures, DateOfCulture, CulturedBy,  MediaForCultures, AdditivesForCultures,
           LabBookNumberForCultures, PageNumberForCultures, Comments) %>%
    collect()

  dt[,grep("Date", names(dt), value = T)] %<>% mutate_all(lubridate::date)

  # Date of Starter Culture
    dt <- dt %>%
      dplyr::filter(between(lubridate::ymd(DateOfStarterCulture),
                            lubridate::ymd(input$culture_initiation_reports_DateOfStarterCulture[1]),
                            lubridate::ymd(input$culture_initiation_reports_DateOfStarterCulture[2])))
  
  # Date of Sub Culture
    dt <- dt %>%
      dplyr::filter(between(lubridate::ymd(DateOfCulture),
                            lubridate::ymd(input$culture_initiation_reports_DateOfSubCulture[1]),
                            lubridate::ymd(input$culture_initiation_reports_DateOfSubCulture[2])))
  
  if(input$culture_initiation_reports_ExplantIdentify !=''){
    dt <- dt %>%
      dplyr::filter(ExplantIdentity == input$culture_initiation_reports_ExplantIdentify)
  }

  if(input$culture_initiation_reports_Source !=''){
    dt <- dt %>%
      dplyr::filter(Source == input$culture_initiation_reports_Source)
  }

  if(input$culture_initiation_reports_Cultivar !=''){
    dt <- dt %>%
      dplyr::filter(Cultivar == input$culture_initiation_reports_Cultivar)
  }

  if(input$culture_initiation_reports_CultivarConfirmed !=''){
    dt <- dt %>%
      dplyr::filter(CultivarConfirmed == input$culture_initiation_reports_CultivarConfirmed)
  }

  if(input$culture_initiation_reports_VirusIndexed !=''){
    dt <- dt %>%
      dplyr::filter(VirusIndexed == input$culture_initiation_reports_VirusIndexed)
  }

  if(input$culture_initiation_reports_CulturedBy !=''){
    dt <- dt %>%
      dplyr::filter(CulturedBy == input$culture_initiation_reports_CulturedBy)
  }
  dt
})

# Select Fields
output$culture_initiation_reports_SelectTheFields_Output <- renderUI({
  req(input$culture_initiation_reports_DateOfSubCulture)
  panel_div(class_type = "default",
            content = tags$div(
              prettyCheckboxGroup(inputId = "culture_initiation_reports_SelectTheFields", label = "Select the Fields",
                                  choices = c(names(culture_initiation_reports_Input())), selected = names(culture_initiation_reports_Input())[1], status = "info", icon = icon("check"))
            ))
})

# Filter based on selected fields
culture_initiation_reports_Fields_Selected_Input <- reactive({

  if (is.null(input$culture_initiation_reports_SelectTheFields)){
    data.frame()
  } else {
    culture_initiation_reports_Input() %>%
      dplyr::select(!!!input$culture_initiation_reports_SelectTheFields)
  }
})

# show data table
observeEvent(input$culture_initiation_reports_LoadData,{
  if(is.null(input$culture_initiation_reports_SelectTheFields)){
    showNotification("Select atleast one field on the left", type = "error")
  }else {
    output$culture_initiation_reports_ResultsTable <- renderRHandsontable({
      dt <- culture_initiation_reports_Fields_Selected_Input()
      rhandsontable(dt)
    })
  }
})



# Export To Excel

output$culture_initiation_reports_ExportToExcel <- downloadHandler(
  filename = function(){paste('Culture Initiation Report-', Sys.time(), '.csv')},
  content = function(file) {
    write.csv(culture_initiation_reports_Fields_Selected_Input(), file, row.names = F)
  }
)

# Clear Reports

observeEvent(input$culture_initiation_reports_Clear,{
  reset("culture_initiation_reports_Form")
  updatePrettyCheckboxGroup(session = session, inputId = "culture_initiation_reports_SelectTheFields", selected = NULL)
}, ignoreNULL = FALSE)



## -----------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------- 2. Cell Suspension Culture Report

output$cell_suspension_culture_reports_Fields_Output <- renderUI({
  csc <- tbl(pool, "tblCSC") %>% 
      dplyr::left_join(tbl(pool, "tblCulturesCSC")) %>% 
      collect()
  
  div(
    
    fluidRow(
      column(2, selectInput("cell_suspension_culture_reports_ExplantIdentify", "Explant Identify", choices = c('', unique(csc$ExplantIdentity)), width = "100%")),
      column(2, selectInput("cell_suspension_culture_reports_CSCIdentity", "CSC Identity", choices = c('',unique(csc$CSCIdentity)), width = "100%")),
      column(2, selectInput("cell_suspension_culture_reports_Source", "Source", choices = c('', csc$Source), width = "100%")),
      column(2, selectInput("cell_suspension_culture_reports_Cultivar", "Cultivar", choices = c('', csc$Cultivar), width = "100%")),
      column(2, dateRangeInput("cell_suspension_culture_reports_DateOfStarterCulture", "Date of Starter culture", 
                               start = min(csc$DateOfStarterCulture), end = max(csc$DateOfStarterCulture),
                               min = min(csc$DateOfStarterCulture), max = max(csc$DateOfStarterCulture)))
    ),
    fluidRow(
      column(2, dateRangeInput("cell_suspension_culture_reports_DateOfSubCulture", "Date of Sub Culture", width = "100%", 
                               start = min(na.omit(csc$DateOfCulture)), end = max(na.omit(csc$DateOfCulture)),
                               min = min(na.omit(csc$DateOfCulture)), max = max(na.omit(csc$DateOfCulture)))),
      column(2, selectInput(inputId = "cell_suspension_culture_reports_CulturedBy",label = "Cultured By", choices = c('', csc$CulturedBy), width = "100%"))
      
    ),
  )
})

# load cultures table

cell_suspension_culture_reports_Input <- reactive({

  dt <- tbl(pool,"tblCSC")%>%
    dplyr::left_join(tbl(pool,"tblCulturesCSC")) %>%
    dplyr::select(ExplantIdentity, CSCIdentity, IdentityType, Cultivar, Source, DateOfStarterCulture, Media, Additives,
           LabBookNumber, PageNumber, NumberOfCultures, DateOfCulture, CulturedBy, MediaForCultures, AdditivesForCultures,
           LabBookNumberForCultures, PageNumberForCultures, Comments) %>%
    collect()

  dt[,grep("Date", names(dt), value = T)] %<>% mutate_all(lubridate::date)

  if(input$cell_suspension_culture_reports_ExplantIdentify !=''){
    dt <- dt %>%
      dplyr::filter(ExplantIdentity == input$cell_suspension_culture_reports_ExplantIdentify)
  }

  if(input$cell_suspension_culture_reports_Source !=''){
    dt <- dt %>%
      dplyr::filter(Source == input$cell_suspension_culture_reports_Source)
  }

  if(input$cell_suspension_culture_reports_Cultivar !=''){
    dt <- dt %>%
      dplyr::filter(Cultivar == input$cell_suspension_culture_reports_Cultivar)
  }

  if(input$cell_suspension_culture_reports_CSCIdentity !=''){
    dt <- dt %>%
      dplyr::filter(CSCIdentity == input$cell_suspension_culture_reports_CSCIdentity)
  }

    dt <- dt %>%
      dplyr::filter(between(lubridate::ymd(DateOfStarterCulture),
                     lubridate::ymd(input$cell_suspension_culture_reports_DateOfStarterCulture[1]),
                     lubridate::ymd(input$cell_suspension_culture_reports_DateOfStarterCulture[2])))
    dt <- dt %>%
      dplyr::filter(between(lubridate::ymd(DateOfCulture),
                     lubridate::ymd(input$cell_suspension_culture_reports_DateOfSubCulture[1]),
                     lubridate::ymd(input$cell_suspension_culture_reports_DateOfSubCulture[2])))
  
  if(input$cell_suspension_culture_reports_CulturedBy !=''){
    dt <- dt %>%
      dplyr::filter(CulturedBy == input$cell_suspension_culture_reports_CulturedBy)
  }
  dt
})

# Select Fields
output$cell_suspension_culture_reports_SelectTheFields_Output <- renderUI({
  req(input$cell_suspension_culture_reports_DateOfStarterCulture)
  div(
  panel_div(class_type = "default",
            content = tags$div(
              prettyCheckboxGroup(inputId = "cell_suspension_culture_reports_SelectTheFields", label = "Select the Fields",
                                  choices = c(names(cell_suspension_culture_reports_Input())), selected = names(cell_suspension_culture_reports_Input())[1], status = "info", icon = icon("check"))
            ))
  )
})

# Filter based on selected fields
cell_suspension_culture_reports_Fields_Selected_Input <- reactive({

  if (is.null(input$cell_suspension_culture_reports_SelectTheFields)){
    data.frame()
  } else {
    cell_suspension_culture_reports_Input() %>%
      dplyr::select(!!!input$cell_suspension_culture_reports_SelectTheFields)
  }
})

# show data table
observeEvent(input$cell_suspension_culture_reports_LoadData,{
  if(is.null(input$cell_suspension_culture_reports_SelectTheFields)){
    showNotification("Select atleast one field on the left", type = "error")
  }else {
    output$cell_suspension_culture_reports_ResultsTable <- renderRHandsontable({
      dt <- cell_suspension_culture_reports_Fields_Selected_Input()
      rhandsontable(dt)
    })
  }
})



# Export To Excel

output$cell_suspension_culture_reports_ExportToExcel <- downloadHandler(
  filename = function(){paste('Cell Suspension Culture Report-', Sys.time(), '.csv')},
  content = function(file) {
    write.csv(cell_suspension_culture_reports_Fields_Selected_Input(), file, row.names = F)
  }
)

# Clear Reports

observeEvent(input$cell_suspension_culture_reports_Clear,{
  reset("cell_suspension_culture_reports_Form")
  updatePrettyCheckboxGroup(session = session, inputId = "cell_suspension_culture_reports_SelectTheFields", selected = NULL)
}, ignoreNULL = FALSE)


