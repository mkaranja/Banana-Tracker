panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

explant_reports <- 
  fluidRow(         
    column(12, hr(), br(),
           div(id = "explant_reports_Form",                   
               fluidRow(
                 column(2, selectInput("explant_reports_ExplantID", "Explant ID", choices = c(''), width = "100%")),
                 column(2, selectInput("explant_reports_Cultivar", "Cultivar", choices = c(''), width = "100%")),
                 column(2, selectInput("explant_reports_VectorID", "Vector ID", choices = c(''), width = "100%")),
                 column(1, br(), actionBttn("explant_reports_VectorID_Get", "", icon=icon("angle-double-right", lib="font-awesome"), size = "sm", style = "jelly", color = "primary")),
                 column(2, selectInput("explant_reports_PlantSelection", "Plant Selection", choices = c(''), width = "100%")),
                 column(3, selectInput("explant_reports_PromoterGene", "Promoter - Gene", choices = c(''), width = "100%"))
                 
               ),
               fluidRow(
                 column(2, br(), p("Date of Initial Culture")),
                 column(3, dateRangeInput("explant_reports_DateOfInitialCulture","")),
                 column(2, selectInput("explant_reports_TFPL_UFPL_ID", "TFPL/ UFPL ID", choices = c(""))),
                 column(1, offset = 1, br(), actionBttn("explant_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T))
               ),
               fluidRow(
                 column(11, rHandsontableOutput("explant_reports_ResultsTable"))
               )
           ),
           fluidRow(
             column(2, downloadBttn("explant_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
             column(1, actionBttn("explant_reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
             column(2, actionBttn("explant_reports_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
             column(2, actionBttn("explant_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
           )
    )
  )


# ------------SERVER-------------------------------

observeEvent(input$explant_reports,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Explant Reports - ", input$project_selected)),
                        explant_reports,
                        easyClose = F, size = "l"
  ))
  
})