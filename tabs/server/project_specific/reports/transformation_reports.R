panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

transformation_reports <- 
  fluidRow(         
    column(12, hr(), br(),
           div(id = "transformation_reports_Form",                   
               fluidRow(
                 column(2, selectInput("transformation_reports_Identify", "Identify", choices = c(''), width = "100%")),
                 column(2, selectInput("transformation_reports_Cultivar", "Cultivar", choices = c(''), width = "100%")),
                 column(2, selectInput("transformation_reports_VectorID", "Vector ID", choices = c(''), width = "100%")),
                 column(1, br(), actionBttn("transformation_reports_VectorID_Get", "",icon=icon("angle-double-right", lib="font-awesome"), size = "sm", style = "jelly", color = "primary")),
                 column(2, textInput("transformation_reports_PlantSelection", "Plant Selection", width = "100%")),
                 column(3, textInput("transformation_reports_PromoterGene", "Promoter - Gene", width = "100%"))
               
               ),
               fluidRow(
                 column(2, br(), p("TPC/UPC Initial Culture Date")),
                 column(3, dateRangeInput("transformation_reports_TPCUPCInitialCultureDate","", width = "100%")),
                 column(1, offset = 2, br(), actionBttn("transformation_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T))
               ),
               fluidRow(
                 column(11, rHandsontableOutput("transformation_reports_ResultsTable"))
               )
           ), br(),
           fluidRow(
             column(2, downloadBttn("transformation_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
             column(1, actionBttn("transformation_reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
             column(2, actionBttn("transformation_reports_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
             column(2, actionBttn("transformation_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
           )
    )
  )


# ------------SERVER-------------------------------

observeEvent(input$transformation_reports,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Transformation Reports - ", input$project_selected)),
                        transformation_reports,
                        easyClose = F, size = "l"
  ))
  
})