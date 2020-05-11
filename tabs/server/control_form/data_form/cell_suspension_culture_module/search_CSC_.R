

search_CSC <- 
  tabPanel("Search CSC", value = "search_CSC",
           br(),br(),
           useShinyjs(),
           
           div(id = "search_CSC_Form",     
               fluidRow(
                 column(12,
                        uiOutput("search_CSC_Fields")
                 )
               ),
               fluidRow(
                 column(3, uiOutput("search_CSC_DateOfStarterCulture_output")),
                 column(8,
                        column(2, br(), actionBttn("search_CSC_Search", "Search", style = "fill", size = "xs", color = "primary", block=T)),
                        column(2, br(), actionBttn("search_CSC_ClearForm", "Clear Form", style = "fill", size = "xs", color = "primary", block=T)),
                        conditionalPanel(
                          condition = "input.search_CSC_Search",
                          
                          column(2, br(), actionBttn("search_CSC_Culture", "Culture", style = "fill", size = "xs", color = "primary", block=T)),
                          column(2, br(), actionBttn("search_CSC_Delete", "Delete Selected CSC", style = "fill", size = "xs", color = "primary", block=T))
                        )
                 )
               ),
               fluidRow(
                 column(8, br(), 
                        uiOutput("search_CSC_ResultsTable_Output"), br()),
                 
                 column(4, br(), #checkboxInput("search_CSC_SearchDeletedCSC11", "Search Deleted CSC", FALSE),
                        shinyWidgets::prettyCheckbox(inputId = "search_CSC_SearchDeletedCSC", label = "Search Deleted CSC", 
                                                     icon = icon("check"), status = "info", value = FALSE)
                        )
               ),
               fluidRow(
                 div(id = "search_CSC_Culture_Form1",
                     column(5, br(), rHandsontableOutput("search_CSC_ExplantIdentiyResultsTable")),
                     uiOutput("search_CSC_Culture_Output")
                 )
               ),
               fluidRow(br(),hr(),
                        column(8, offset = 2,
                               column(2, actionBttn("search_CSC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                               column(4, actionBttn("search_CSC_SCP_CSC_ControlForm", "MFC, SCP and CSC Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                               column(2, 
                                      tags$button(id = 'search_CSC_Exit', type = "button", class = "btn action-button",
                                                  onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                                  "Exit", style="color: #fff; background-color: #ff0011; border-color: #ff0011; padding:4px; font-size:80%;")
                                      #actionBttn("search_culture_initiation_Exit", "Exit", style = "jelly", size = "xs", color = "danger", block=T)
                               )
                        )
               )
           )
  )