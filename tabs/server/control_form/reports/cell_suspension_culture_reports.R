panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


csc <- tbl(pool,"tblCSC") %>% 
  left_join(tbl(pool,"tblCulturesCSC")) %>%
  collect()

cell_suspension_culture_reports <- 
  tabPanel(tags$h2(style="color:#FF6347;text-align:center;","Cell Suspension Culture Reports"), value = "cell_suspension_culture_reports",
           div(id = "cell_suspension_culture_reports_Form",
               
               column(12,br(), br(),
                      
                      fluidRow(
                        column(2, selectInput("cell_suspension_culture_reports_ExplantIdentify", "Explant Identify", choices = c('', csc$ExplantIdentity), width = "100%")),
                        column(2, selectInput("cell_suspension_culture_reports_CSCIdentity", "CSC Identity", choices = c('',csc$CSCIdentity), width = "100%")),
                        column(2, selectInput("cell_suspension_culture_reports_Source", "Source", choices = c('', csc$Source), width = "100%")),
                        column(2, selectInput("cell_suspension_culture_reports_Cultivar", "Cultivar", choices = c('', csc$Cultivar), width = "100%")),
                        column(2, dateRangeInput("cell_suspension_culture_reports_DateOfStarterCulture", "Date of Starter culture", 
                                                 start = min(csc$DateOfStarterCulture), end = max(csc$DateOfStarterCulture),
                                                 min = min(csc$DateOfStarterCulture), max = max(csc$DateOfStarterCulture)))
                      ),
                      fluidRow(
                        column(2, dateRangeInput("cell_suspension_culture_reports_DateOfSubCulture", "Date of Sub Culture", width = "100%", 
                                                 start = min(csc$DateOfCulture), end = max(csc$DateOfCulture),
                                                 min = min(csc$DateOfCulture), max = max(csc$DateOfCulture))),
                        column(2, selectInput(inputId = "cell_suspension_culture_reports_CulturedBy",label = "Cultured By", choices = c('', csc$CulturedBy), width = "100%"))
                        
                      ),
                      fluidRow(
                        column(2, uiOutput("cell_suspension_culture_reports_SelectTheFields_Output")),
                        column(9, br(), rHandsontableOutput("cell_suspension_culture_reports_ResultsTable"))
                      ),
                      verbatimTextOutput("dd2"),
                      fluidRow(
                        column(2, actionBttn("cell_suspension_culture_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T)),
                        column(2, downloadBttn("cell_suspension_culture_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary", block=T)),
                        column(2, actionBttn("cell_suspension_culture_reports_Clear", "Clear", size = "xs", style = "jelly", color = "primary", block=T)),
                        column(2, actionBttn("cell_suspension_culture_reports_MFC_SCP_CSC_ControlForm", "MFC, SCP and CSC Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
                        column(2, offset = 1, actionBttn("cell_suspension_culture_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary", block=T))
                      )
               )
           )
  )