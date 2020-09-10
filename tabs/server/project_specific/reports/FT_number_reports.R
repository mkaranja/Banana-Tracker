panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

FT_Number_Reports <- 
  fluidRow(         
    column(12, hr(), br(),
           div(id = "FT_Number_Reports_Form",                   
               fluidRow(
                 column(2, selectInput("FT_Number_Reports_Identify", "Identify", choices = c(''), width = "100%")),
                 column(2, selectInput("FT_Number_Reports_Cultivar", "Cultivar", choices = c(''), width = "100%")),
                 column(2, selectInput("FT_Number_Reports_VectorID", "Vector ID", choices = c(''), width = "100%")),
                 column(1, br(), actionBttn("FT_Number_Reports_VectorID_Get", "", icon=icon("angle-double-right", lib="font-awesome"), size = "sm", style = "jelly", color = "primary")),
                 column(2, selectInput("FT_Number_Reports_PlantSelection", "Plant Selection", choices = c(''), width = "100%")),
                 column(3, selectInput("FT_Number_Reports_PromoterGene", "Promoter - Gene", choices = c(''), width = "100%"))
                 
               ),
               fluidRow(
                 column(2, br(), p("TPC/UPC Initial Culture Date")),
                 column(4, dateRangeInput("TPC_UPC_Initial_Culture_Date","")),
                 column(3, selectInput("FT_Number_Reports_FieldTrialIdentity", "Field Trial Identity", choices = c(''), width = "100%"))
                 
               ),
               fluidRow(
                 column(3, uiOutput("FT_Number_Reports_fields"), br(), 
                        actionBttn("FT_Number_Reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary")), br(),
                 column(9, rHandsontableOutput("FT_Number_Reports_ResultsTable"))
               )
           ), br(),
           fluidRow(
             column(2, downloadBttn("FT_Number_Reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
             column(1, actionBttn("FT_Number_Reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
             column(2, actionBttn("FT_Number_Reports_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
             column(2, actionBttn("FT_Number_Reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
           )
    )
  )


# ------------SERVER-------------------------------

observeEvent(input$FT_Number_Reports,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("FT Number Reports - ", input$project_selected)),
                        FT_Number_Reports,
                        easyClose = F, size = "l"
  ))
  
})