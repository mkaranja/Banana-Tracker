panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}




cell_suspension_culture_reports <- 
  fluidRow(         
     column(12,br(), br(),
        div(id = "cell_suspension_culture_reports_Form",  
            uiOutput("cell_suspension_culture_reports_Fields_Output"),
            fluidRow(
              column(2, uiOutput("cell_suspension_culture_reports_SelectTheFields_Output")),
              column(9, br(), rHandsontableOutput("cell_suspension_culture_reports_ResultsTable"))
            )
            ),
            verbatimTextOutput("dd2"),
            fluidRow(
              column(1, actionBttn("cell_suspension_culture_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T)),
              column(2, downloadBttn("cell_suspension_culture_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
              column(1, actionBttn("cell_suspension_culture_reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
              column(1, actionBttn("cell_suspension_culture_reports_MFC_SCP_CSC_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary")),
              column(2, actionBttn("cell_suspension_culture_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
            )
     )
  )