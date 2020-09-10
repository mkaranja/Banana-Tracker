panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

plant_tissue_culture_reports <- 
  fluidRow(         
    column(12, hr(), br(),
           div(id = "plant_tissue_culture_reports_Form",                   
               fluidRow(
                 column(2, selectInput("plant_tissue_culture_reports_PTCIdentity", "PTC Identity", choices = NULL, width = "100%")),
                 column(2, selectInput("plant_tissue_culture_reports_Cultivar", "Cultivar", choices = NULL, width = "100%")),
                 column(2, selectInput("plant_tissue_culture_reports_VectorID", "Vector ID", choices = NULL, width = "100%")),
                 column(1, br(), actionBttn("plant_tissue_culture_reports_VectorID_Get", "", icon=icon("angle-double-right", lib="font-awesome"), size = "sm", style = "jelly", color = "primary")),
                 column(2, textInput("plant_tissue_culture_reports_PlantSelection", "Plant Selection", width = "100%")),
                 column(3, textInput("plant_tissue_culture_reports_PromoterGene", "Promoter - Gene", width = "100%"))
                 
               ),
               fluidRow(
                 column(2, br(), tags$p(style='text-align:right',"TPC Initial Culture Date")),
                 column(3, dateRangeInput("plant_tissue_culture_reports_TPCInitialCultureDate","", width = "100%"))
                 
               ),
               fluidRow(
                 column(2, 
                        panel_div(class_type = "default",
                                  content = tags$div(
                                    uiOutput("plant_tissue_culture_reports_Fields")
                                    )), 
                        actionBttn("plant_tissue_culture_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary")
                        ), br(),br(),br(),
                 column(9, rHandsontableOutput("plant_tissue_culture_reports_Table"))
               )
           ), br(),
           fluidRow(
             column(2, downloadBttn("plant_tissue_culture_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
             column(1, actionBttn("plant_tissue_culture_reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
             column(2, actionBttn("plant_tissue_culture_reports_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
             column(2, actionBttn("plant_tissue_culture_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
           )
    )
  )


# ------------SERVER-------------------------------

observeEvent(input$plant_tissue_culture_reports,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Plant Tissue Culture Reports - ", input$project_selected)),
                        plant_tissue_culture_reports,
                        easyClose = F, size = "l"
  ))
  
})

# get data
tblPlantTissueCulture <- reactive({
  tbl(pool, paste0(input$project_selected,"_tblPlantTissueCulture")) %>% 
     collect()
})

tblPlantTissueCulture_values <- reactiveValues()

plant_tissue_culture_reports_input <- reactive({
  tblPlantTissueCulture_values$Data <- tblPlantTissueCulture()
  dt <- tblPlantTissueCulture_values$Data
  dt[,grep("Date", names(dt), value = T)] %<>% mutate_all(lubridate::date)
  
  if(input$plant_tissue_culture_reports_PTCIdentity !=''){
    dt <- dt %>%
      dplyr::filter(trimws(PTCIdentity) == trimws(input$plant_tissue_culture_reports_PTCIdentity))
  }
  # if(input$plant_tissue_culture_reports_Cultivar!=''){
  #   dt <- dt %>%
  #     dplyr::filter(trimws(Cultivar) == trimws(input$plant_tissue_culture_reports_Cultivar))
  # }
  # if(input$plant_tissue_culture_reports_VectorID !=''){
  #   dt <- dt %>%
  #     dplyr::filter(trimws(VectorID1) == trimws(input$plant_tissue_culture_reports_VectorID))
  # }
  # if(!is.na(input$plant_tissue_culture_reports_TPCInitialCultureDate)){
  # dt <- dt %>%
  #   dplyr::filter(between(lubridate::ymd(DateOfStarterCulture),
  #                lubridate::ymd(input$plant_tissue_culture_reports_TPCInitialCultureDate[1]),
  #                lubridate::ymd(input$plant_tissue_culture_reports_TPCInitialCultureDate[2])))
  # }
  dt
})


# update fields

observeEvent(input$plant_tissue_culture_reports,{
  updateSelectInput(session, "plant_tissue_culture_reports_PTCIdentity", "PTC Identity", choices = c('', plant_tissue_culture_reports_input()$PTCIdentity))
  updateSelectInput(session, "plant_tissue_culture_reports_Cultivar", "Cultivar", choices = c('', plant_tissue_culture_reports_input()$Cultivar))
  updateSelectInput(session, "plant_tissue_culture_reports_VectorID", "Vector ID", choices = c('', plant_tissue_culture_reports_input()$VectorID1))
  updateDateRangeInput(session, "plant_tissue_culture_reports_TPCInitialCultureDate", start = min(plant_tissue_culture_reports_input()$DateOfStarterCulture),
                       end = max(plant_tissue_culture_reports_input()$DateOfStarterCulture))
})

# load vector ID details

observeEvent(input$plant_tissue_culture_reports_VectorID_Get, {
  dt <- plant_tissue_culture_reports_input() %>%
    dplyr::filter(VectorID1==input$plant_tissue_culture_reports_VectorID)
  updateTextInput(session, "plant_tissue_culture_reports_Cultivar", "Cultivar", value = plant_tissue_culture_reports_input()$Cultivar)
  updateTextInput(session, "plant_tissue_culture_reports_PromoterGene", "Promoter - Gene", value = 0)
})

# Select fields to show in table

output$plant_tissue_culture_reports_Fields <- renderUI({
  prettyCheckboxGroup("plant_tissue_culture_reports_Fields_Select", "Select the Fields", 
    choices = c(names(plant_tissue_culture_reports_input())), status = "info", icon = icon("check"))
})

# Filter based on selected fields
plant_tissue_culture_reports_selected_input <- reactive({
  
  if (is.null(input$plant_tissue_culture_reports_Fields_Select)){
    data.frame()
  } else {
    plant_tissue_culture_reports_input() %>%
      dplyr::select(!!!input$plant_tissue_culture_reports_Fields_Select)
  }
})
# Result Table 
observeEvent(input$plant_tissue_culture_reports_LoadData,{
  output$plant_tissue_culture_reports_Table <- renderRHandsontable({
    rhandsontable(plant_tissue_culture_reports_selected_input())
  })
})


# Export to Excel
output$plant_tissue_culture_reports_ExportToExcel <- downloadHandler(
  filename = function(){paste('Plant Tissue Culture Report-', input$project_selected,"-", Sys.time(), '.csv')},
  content = function(file) {
    write.csv(plant_tissue_culture_reports_selected_input(), file, row.names = F)
  }
)

# Clear Reports

observeEvent(input$plant_tissue_culture_reports_Clear,{
  reset("plant_tissue_culture_reports_Form")
  updatePrettyCheckboxGroup(session = session, inputId = "plant_tissue_culture_reports_Fields_Select", selected = NULL)
}, ignoreNULL = FALSE)