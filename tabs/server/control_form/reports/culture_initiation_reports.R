panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

mfc <- tbl(pool,"tblMFC") %>% collect()
cultures <- tbl(pool,"tblCultures") %>% collect()

culture_initiation_reports <- 
  tabPanel(tags$h2(style="color:#FF6347;text-align:center;","Culture Initiation Reports"), value = "culture_initiation_reports",
        div(id = "culture_initiation_reports_Form",
            
           column(12, br(), br(),
                                      
                fluidRow(
                    column(2, selectInput("culture_initiation_reports_ExplantIdentify", "Explant Identify", choices = c('', mfc$ExplantIdentity), width = "100%")),
                    column(2, selectInput("culture_initiation_reports_Source", "Source", choices = c('', mfc$Source), width = "100%")),
                    column(2, selectInput("culture_initiation_reports_Cultivar", "Cultivar", choices = c('', mfc$Cultivar), width = "100%")),
                    column(2, selectInput("culture_initiation_reports_CultivarConfirmed", "Cultivar Confirmed", choices = c('','Yes','No'), width = "100%")),
                    column(2, selectInput("culture_initiation_reports_VirusIndexed", "Virus Indexed", choices = c('','Yes','No'), width = "100%")),
                    column(2, dateRangeInput("culture_initiation_reports_DateOfStarterCulture", "Date of Starter culture", 
                                             start = min(mfc$DateOfStarterCulture), end = max(mfc$DateOfStarterCulture),
                                             min = min(mfc$DateOfStarterCulture), max = max(mfc$DateOfStarterCulture)))
                ),
                fluidRow(
                    column(2, dateRangeInput("culture_initiation_reports_DateOfSubCulture", "Date of Sub Culture", width = "100%", 
                                             start = min(cultures$DateOfCulture), end = max(cultures$DateOfCulture),
                                             min = min(cultures$DateOfCulture), max = max(cultures$DateOfCulture))),
                    column(2, selectInput(inputId = "culture_initiation_reports_CulturedBy",label = "Cultured By", choices = c('', cultures$CulturedBy), width = "100%"))
                         
                ),
                fluidRow(
                    column(2, uiOutput("culture_initiation_reports_SelectTheFields_Output")),
                    column(9, br(), rHandsontableOutput("culture_initiation_reports_ResultsTable"))
                ),
                verbatimTextOutput("dd"),
                fluidRow(
                    column(2, actionBttn("culture_initiation_reports_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T)),
                    column(2, downloadBttn("culture_initiation_reports_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary", block=T)),
                    column(2, actionBttn("culture_initiation_reports_Clear", "Clear", size = "xs", style = "jelly", color = "primary", block=T)),
                    column(2, actionBttn("culture_initiation_reports_MFC_SCP_CSC_ControlForm", "MFC, SCP and CSC Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
                    column(2, offset = 1, actionBttn("culture_initiation_reports_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary", block=T))
                )
           )
)
)