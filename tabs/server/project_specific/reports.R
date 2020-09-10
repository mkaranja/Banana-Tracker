#source("tabs/server/project_specific/reports/transformation_reports.R")
#source("tabs/server/project_specific/reports/plant_tissue_culture_reports.R")
# source("tabs/server/project_specific/reports/FT_Number_Reports.R")
# source("tabs/server/project_specific/reports/explant_reports.R")
# source("tabs/server/project_specific/reports/plant_information.R")
# source("tabs/server/project_specific/reports/tracing_module.R")
# source("tabs/server/project_specific/reports/new_plant_tissue_culture.R")


# fluidRow(
#   column(12, br(), br(),
#          div(id = "transformation_reports_Form",
#              fluidRow(
#                uiOutput("transformation_reports_Fields_Output")
#              ),
#              fluidRow(
#                uiOutput("transformation_reports_Fields_Output2")
#              ),
#              fluidRow(
#                column(2, uiOutput("transformation_reports_SelectTheFields_Output")),
#                column(9, br(), rHandsontableOutput("transformation_reports_ResultsTable"))
#              )
#          ),
#          fluidRow(
#            column(1, actionBttn("transformation_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T)),
#            column(2, downloadBttn("transformation_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
#            column(1, actionBttn("transformation_reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
#            column(1, actionBttn("transformation_reports_MFC_SCP_CSC_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
#            column(2, actionBttn("transformation_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
#          )
#   )
# ),
observeEvent(input$transformation_reports,{
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Transformation Reports - ", input$project_selected)),
                          fluidRow(
                            column(12, hr(), br(),
                                   div(id = "transformation_reports_Form",
                                       fluidRow(
                                         column(2, selectInput("transformation_reports_Identity", "Identity", choices = c(''), width = "100%")),
                                         column(2, selectInput("transformation_reports_Cultivar", "Cultivar", choices = c(''), width = "100%")),
                                         column(2, selectInput("transformation_reports_VectorID", "Vector ID", choices = c(''), width = "100%")),
                                         column(1, br(), actionBttn("transformation_reports_VectorID_Get", "",icon=icon("angle-double-right", lib="font-awesome"), 
                                                                    size = "sm", style = "jelly", color = "primary")),
                                         column(2, textInput("transformation_reports_PlantSelection", "Plant Selection", width = "100%")),
                                         column(3, textInput("transformation_reports_PromoterGene", "Promoter - Gene", width = "100%"))
                                       ),
                                       fluidRow(
                                         column(2, br(), p("TPC/UPC Initial Culture Date")),
                                         column(3, dateRangeInput("transformation_reports_TPCUPCInitialCultureDate","", width = "100%")),
                                         column(1, offset = 2, br(), actionBttn("transformation_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T))
                                       ),
                                       fluidRow(
                                         column(11, rHandsontableOutput("transformation_reports_ResultsTable"))                                       )
                                   ), br(),
                                   fluidRow(
                                     column(2, downloadBttn("transformation_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
                                     column(1, actionBttn("transformation_reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
                                     column(2, actionBttn("transformation_reports_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
                                     column(2, actionBttn("transformation_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
                                   )
                                )
                          ),
          easyClose = F, size = "l"
  ))
})

observeEvent(input$transformation_reports,{
  tb <- paste0("IBBTV", "_tblTransformation")
  dt <- loadData(tb)
    updateSelectInput(session, "transformation_reports_Identity", "Identity", choices = c('', dt$TransformationID))
    updateSelectInput(session,"transformation_reports_Cultivar", "Cultivar", choices = c('', dt$Cultivar))
    updateSelectInput(session,"transformation_reports_VectorID", "Vector ID", choices = c('', dt$VectorID1))
})

transformation_reports_input <- reactive({
  tb <- paste0(input$project_selection, "_tblTransformation")
  dt <- loadData(tb)
  
  if(input$transformation_reports_Identity !=''){
    dt <- dt %>%
      dplyr::filter(trimws(TransformationID) == trimws(input$transformation_reports_Identity))
  }
  if(input$transformation_reports_Cultivar !=''){
    dt <- dt %>%
      dplyr::filter(trimws(Cultivar) == trimws(input$transformation_reports_Cultivar))
  }
  if(input$transformation_reports_VectorID !=''){
    dt <- dt %>%
      dplyr::filter(trimws(VectorID1) == trimws(input$transformation_reports_VectorID))
  }
  dt
})
observeEvent(input$transformation_reports_VectorID_Get,{
  dt <- loadData(paste0(input$project_selected, "_tblTransformation")) %>%
    dplyr::left_join(vector_inventory())
  
  updateTextInput(session, "transformation_reports_PlantSelection", "Plant Selection", value = dt$PlantSelection)
  updateTextInput(session,"transformation_reports_PromoterGene", "Promoter - Gene", value = "")
})

output$transformation_reports_Fields_Output <- renderUI({
  tb <- paste0(input$project_selection, "_tblTransformation")
  dt <- loadData(tb) %>%
    dplyr::left_join(loadData(paste0(input$project_selection, "_tblVectorInventory")))

  div(
    column(2, selectInput("transformation_reports_Identity", "Identify", choices = c('', unique(dt$TransformationID)), width = "100%")),
    column(2, selectInput("transformation_reports_Source", "Source", choices = c('', mfc$Source), width = "100%")),
    column(2, selectInput("transformation_reports_Cultivar", "Cultivar", choices = c('', mfc$Cultivar), width = "100%")),
    column(2, selectInput("transformation_reports_CultivarConfirmed", "Cultivar Confirmed", choices = c('','Yes','No'), width = "100%")),
    column(2, selectInput("transformation_reports_VirusIndexed", "Virus Indexed", choices = c('','Yes','No'), width = "100%")),
    column(2, dateRangeInput("transformation_reports_DateOfStarterCulture", "Date of Starter culture",
                             start = min(mfc$DateOfStarterCulture), end = max(mfc$DateOfStarterCulture),
                             min = min(mfc$DateOfStarterCulture), max = max(mfc$DateOfStarterCulture)))
  )
})


output$transformation_reports_Fields_Output2 <- renderUI({
  cultures <- tbl(pool, "tblCultures") %>% collect()

  div(
    column(2, dateRangeInput("transformation_reports_DateOfSubCulture", "Date of Sub Culture", width = "100%",
                             start = min(cultures$DateOfCulture), end = max(cultures$DateOfCulture),
                             min = min(cultures$DateOfCulture), max = max(cultures$DateOfCulture))),
    column(2, selectInput(inputId = "transformation_reports_CulturedBy",label = "Cultured By", choices = c('', cultures$CulturedBy), width = "100%"))

  )
})

transformation_reports_Input <- reactive({

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
                          lubridate::ymd(input$transformation_reports_DateOfStarterCulture[1]),
                          lubridate::ymd(input$transformation_reports_DateOfStarterCulture[2])))

  # Date of Sub Culture
  dt <- dt %>%
    dplyr::filter(between(lubridate::ymd(DateOfCulture),
                          lubridate::ymd(input$transformation_reports_DateOfSubCulture[1]),
                          lubridate::ymd(input$transformation_reports_DateOfSubCulture[2])))

  if(input$transformation_reports_ExplantIdentify !=''){
    dt <- dt %>%
      dplyr::filter(ExplantIdentity == input$transformation_reports_ExplantIdentify)
  }

  if(input$transformation_reports_Source !=''){
    dt <- dt %>%
      dplyr::filter(Source == input$transformation_reports_Source)
  }

  if(input$transformation_reports_Cultivar !=''){
    dt <- dt %>%
      dplyr::filter(Cultivar == input$transformation_reports_Cultivar)
  }

  if(input$transformation_reports_CultivarConfirmed !=''){
    dt <- dt %>%
      dplyr::filter(CultivarConfirmed == input$transformation_reports_CultivarConfirmed)
  }

  if(input$transformation_reports_VirusIndexed !=''){
    dt <- dt %>%
      dplyr::filter(VirusIndexed == input$transformation_reports_VirusIndexed)
  }

  if(input$transformation_reports_CulturedBy !=''){
    dt <- dt %>%
      dplyr::filter(CulturedBy == input$transformation_reports_CulturedBy)
  }
  dt
})

# Select Fields
output$transformation_reports_SelectTheFields_Output <- renderUI({
  req(input$transformation_reports_DateOfSubCulture)
  panel_div(class_type = "default",
            content = tags$div(
              prettyCheckboxGroup(inputId = "transformation_reports_SelectTheFields", label = "Select the Fields",
                                  choices = c(names(transformation_reports_Input())), selected = names(transformation_reports_Input())[1], status = "info", icon = icon("check"))
            ))
})

# Filter based on selected fields
transformation_reports_Fields_Selected_Input <- reactive({

  if (is.null(input$transformation_reports_SelectTheFields)){
    data.frame()
  } else {
    transformation_reports_Input() %>%
      dplyr::select(!!!input$transformation_reports_SelectTheFields)
  }
})

# show data table
observeEvent(input$transformation_reports_LoadData,{
  if(is.null(input$transformation_reports_SelectTheFields)){
    showNotification("Select atleast one field on the left", type = "error")
  }else {
    output$transformation_reports_ResultsTable <- renderRHandsontable({
      dt <- transformation_reports_Fields_Selected_Input()
      rhandsontable(dt)
    })
  }
})



# Export To Excel

output$transformation_reports_ExportToExcel <- downloadHandler(
  filename = function(){paste('Culture Initiation Report-', Sys.time(), '.csv')},
  content = function(file) {
    write.csv(transformation_reports_Fields_Selected_Input(), file, row.names = F)
  }
)

# Clear Reports

observeEvent(input$transformation_reports_Clear,{
  reset("transformation_reports_Form")
  updatePrettyCheckboxGroup(session = session, inputId = "transformation_reports_SelectTheFields", selected = NULL)
}, ignoreNULL = FALSE)

