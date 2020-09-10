culture_initiation <- tabPanel("Culture Initiation", value = "tracing_culture_initiation", hr(),
  fluidRow(
    column(12,
           column(2, br(), tags$h5("Please Select the Year")),
           column(2, selectInput("tracing_culture_initiation_Year", "", choices = 2000:lubridate::year(Sys.Date()))),
           column(1, br(), actionBttn("tracing_culture_initiation_Get_Year", "", icon=icon("angle-double-right", lib="font-awesome"))),
           column(2, offset = 3, br(), actionBttn("tracing_culture_initiation_ClearTheForm", "Clear the Form", style = "jelly",  color = "primary", size="sm"))
    ),
    column(12,
           column(2, br(), tags$h5("Select or Enter the Explant Identity(ACTIVE)")),
           column(2, selectInput("tracing_culture_initiation_ExplantIdentityACTIVE", "", choices = NULL)),
           column(1, br(), actionBttn("tracing_culture_initiation_Get_ExplantIdentityACTIVE", "", icon=icon("angle-double-right", lib="font-awesome"))),
           column(2, br(), tags$h5("Select or Enter the Explant Identity(TRASHED)")),
           column(2, selectInput("tracing_culture_initiation_ExplantIdentityTRASHED", "", choices = NULL)),
           column(1, br(), actionBttn("tracing_culture_initiation_Get_ExplantIdentityTRASHED", "", icon=icon("angle-double-right", lib="font-awesome")))
    ),
    column(12,
           conditionalPanel(
             condition = "input.tracing_culture_initiation_Get_ExplantIdentityACTIVE",
           
                panel_div(class_type = "default",
                      content = tags$div(
                         tags$h4("Active Plants of the Selected Explant Identity"),
                         rHandsontableOutput("tracing_culture_initiation_ActivePLants"),
                         column(2, offset = 5, br(), actionBttn("tracing_culture_initiation_ActivePlants_ExportToExcel", "Export to Excel", size="xs"))
                  ))
           )
             
    ),
    column(12,
           conditionalPanel(
             condition = "input.tracing_culture_initiation_Get_ExplantIdentityTRASHED",
           
                panel_div(class_type = "default",
                    content = tags$div(
                         tags$h4("Trashed Plants of the Selected Explant Identity"),
                         rHandsontableOutput("tracing_culture_initiation_TrashedPLants"),
                         column(2, offset = 5, br(), actionBttn("tracing_culture_initiation_TrashedPlants_ExportToExcel", "Export to Excel", size="xs"))
                  ))
           )
          )
  ),hr(),
  fluidRow(
    column(6),
    column(6,
           column(4),
           column(3, actionBttn("tracing_culture_initiation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
           column(3, actionBttn("tracing_culture_initiation_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
           column(2, actionBttn('tracing_culture_initiation_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
    )
  )
  
)