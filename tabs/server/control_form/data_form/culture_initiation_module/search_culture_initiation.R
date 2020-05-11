

search_culture_initiation <- 
  tabPanel("Search Culture Initiation", value = "search_culture_initiation",
              br(),br(),
              useShinyjs(),
              
          div(id = "search_culture_initiation_Form",     
              fluidRow(
                column(12,
                       column(2, selectInput("search_culture_initiation_Identify", "Identify", choices = c('', mfc$ExplantIdentity), selected = '')),
                       column(2, selectInput("search_culture_initiation_Source", "Source", choices = c('', source$Source), selected = '')),
                       column(2, selectInput("search_culture_initiation_Cultivar", "Cultivar", choices = c('', cultivar$Cultivar), selected = '')),
                       column(2, selectInput("search_culture_initiation_CultivarConfirmed", "Cultivar Confirmed", choices = c("","Yes","No"))),
                       column(2, selectInput("search_culture_initiation_VirusIndexed", "Virus Indexed", choices = c("","Yes","No"))),
                       column(2, selectInput("search_culture_initiation_VirusIndexed", "Virus Indexed", choices = c("","Yes","No")))
                       
                       )
              ),
              fluidRow(
                column(4, 
                       dateRangeInput("search_culture_initiation_DateOfStarterCulture", "Date of Starter Culture", 
                                      min = min(mfc$DateOfStarterCulture), max = max(mfc$DateOfStarterCulture), 
                                      start = min(mfc$DateOfStarterCulture), end = max(mfc$DateOfStarterCulture), separator = "  TO  ")
                ),
                column(8,
                       column(2, actionBttn("search_culture_initiation_ActionSearch", "Search", style = "fill", size = "xs", color = "primary", block=T)),
                       column(2, actionBttn("search_culture_initiation_ActionClearForm", "Clear Form", style = "fill", size = "xs", color = "primary", block=T)),
                       conditionalPanel(
                         condition = "input.search_culture_initiation_ActionSearch",
                       
                             column(2, actionBttn("search_culture_initiation_ActionCulture", "Culture", style = "fill", size = "xs", color = "primary", block=T)),
                             column(2, actionBttn("search_culture_initiation_ActionDelete", "Delete Selected MFC", style = "fill", size = "xs", color = "primary", block=T))
                       )
                       # radioGroupButtons(inputId = "search_culture_initiation_Action",label = "Select Only One Condition Per Search. Clear the Form Before Every New Search",
                       #                   choices = c("Search","Clear Form", "Culture", "Delete Selected MFC"),individual = TRUE, size ="xs", status = "info",
                       #                   checkIcon = list(yes = icon("ok", lib = "glyphicon")))
                )
              ),
              fluidRow(
                column(8, br(), 
                       uiOutput("search_culture_initiation_ResultsTable_Output"), br()),
                
                column(4, br(), #checkboxInput("search_culture_initiation_SearchDeletedMFC11", "Some value", FALSE),
                       shinyWidgets::prettyCheckbox(inputId = "search_culture_initiation_SearchDeletedMFC", label = "Search Deleted MFC", status = "info", value = TRUE),
                       verbatimTextOutput("txt11"))
              ),
              fluidRow(
                div(id = "search_culture_initiation_Culture_Form1",
                    column(5, br(), rHandsontableOutput("search_culture_initiation_ExplantIdentiyResultsTable")),
                    uiOutput("search_culture_initiation_ActionCulture_Output")
                )
                ),
              fluidRow(br(),hr(),
                     column(6),
                     column(6,
                       column(3, actionBttn("search_culture_initiation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                       column(6, actionBttn("search_culture_initiation_MFC_SCP_CSC_ControlForm", "MFC, SCP and CSC Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                       column(2, 
                              tags$button(id = 'search_culture_initiation_Exit', type = "button", class = "btn action-button",
                                          onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                          "Exit", style="color: #fff; background-color: #ff0011; border-color: #ff0011; padding:4px; font-size:80%;")
                              )
                       )
              )
          )
)