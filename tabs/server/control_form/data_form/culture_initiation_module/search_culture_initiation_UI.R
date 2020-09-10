

search_culture_initiation <- 
  tabPanel("Search Culture Initiation", value = "search_culture_initiation",
              br(),br(),
              useShinyjs(),
              
          div(id = "search_culture_initiation_Form",     
              fluidRow(
                uiOutput("search_culture_initiation_Output")
              ),
              fluidRow(
                column(3, 
                       uiOutput("search_culture_initiation_DateOfStarterCulture_Output")
                ),
                column(8,
                       column(2, br(), actionBttn("search_culture_initiation_ActionSearch", "Search", style = "fill", size = "xs", color = "primary", block=T)),
                       column(2, br(), actionBttn("search_culture_initiation_ActionClearForm", "Clear Form", style = "fill", size = "xs", color = "primary", block=T)),
                       conditionalPanel(
                         condition = "input.search_culture_initiation_ActionSearch",
                             column(2, br(), actionBttn("search_culture_initiation_ActionCulture", "Culture", style = "fill", size = "xs", color = "primary", block=T)),
                             column(3, br(), actionBttn("search_culture_initiation_ActionDelete", "Delete Selected MFC", style = "fill", size = "xs", color = "primary", block=T))
                       )
                )
              ),br(), 
              fluidRow(
                column(10, 
                       rHandsontableOutput("search_culture_initiation_ResultsTable")
                       # uiOutput("search_culture_initiation_ResultsTable_Output")
                       ),
                column(2, shinyWidgets::awesomeCheckbox(inputId = "search_culture_initiation_SearchDeletedMFC", label = "Search Deleted MFC", status = "info", value = F))
              ),
              fluidRow(
                div(id = "search_culture_initiation_Culture_Form1",
                    column(5, br(), rHandsontableOutput("search_culture_initiation_ExplantIdentiyResultsTable")),
                    uiOutput("search_culture_initiation_ActionCulture_Output")
                 )
                ), hr(),
              fluidRow(
                     column(6),
                     column(6,
                            column(4),
                            column(3, actionBttn("search_culture_initiation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                            column(3, actionBttn("search_culture_initiation_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                            column(2, actionBttn('search_culture_initiation_Exit', "Exit",  style = "jelly", size = "xs", color = "danger", block=T))
                       )
              )
          )
)