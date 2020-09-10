panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


culture_initiation_reports <- 
  fluidRow(          
     column(12, br(), br(),
        div(id = "culture_initiation_reports_Form",                   
          fluidRow(
            uiOutput("culture_initiation_reports_Fields_Output")
          ),
          fluidRow(
            uiOutput("culture_initiation_reports_Fields_Output2")
          ),
          fluidRow(
              column(2, uiOutput("culture_initiation_reports_SelectTheFields_Output")),
              column(9, br(), rHandsontableOutput("culture_initiation_reports_ResultsTable"))
          )
          ),
          fluidRow(
              column(1, actionBttn("culture_initiation_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T)),
              column(2, downloadBttn("culture_initiation_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
              column(1, actionBttn("culture_initiation_reports_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
              column(1, actionBttn("culture_initiation_reports_MFC_SCP_CSC_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
              column(2, actionBttn("culture_initiation_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
          )
     )
)
