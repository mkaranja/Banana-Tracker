

explant_search <- 
  tabPanel("Search Explant", value = "explant_search",
           br(),br(),
           useShinyjs(),
           
           div(id = "explant_search_Form",     
               fluidRow(
                 column(12,
                        uiOutput("explant_search_Fields")
                 )
               ),
               fluidRow(
                 column(3, uiOutput("explant_search_DateOfStarterCulture_output")),
                 column(8,
                        column(2, br(), actionBttn("explant_Search", "Search", style = "fill", size = "xs", color = "primary", block=T)),
                        column(2, br(), actionBttn("explant_search_ClearForm", "Clear Form", style = "fill", size = "xs", color = "primary", block=T)),
                        conditionalPanel(
                          condition = "input.explant_Search",
                          
                          column(2, br(), actionBttn("explant_search_Culture", "Culture", style = "fill", size = "xs", color = "primary", block=T)),
                          column(3, br(), actionBttn("explant_search_Delete", "Delete Selected Explant", style = "fill", size = "xs", color = "primary", block=T))
                        )
                 )
               ),
               fluidRow(
                 column(8, br(), 
                        uiOutput("explant_search_ResultsTable_Output"), br()),
                 
                 column(4, br(), #checkboxInput("explant_search_SearchDeletedExplant11", "Search Deleted Explant", FALSE),
                        shinyWidgets::prettyCheckbox(inputId = "explant_Search_Deleted_Explant", label = "Search Deleted Explant", 
                                                     icon = icon("check"), status = "info", value = FALSE)
                        )
               ),
               fluidRow(
                 div(id = "explant_search_Culture_Form1",
                     column(5, br(), rHandsontableOutput("explant_search_ExplantIdentiyResultsTable")),
                     uiOutput("explant_search_Culture_Output")
                 )
               ),hr(),
               
               fluidRow(
                        column(6),
                        column(6,
                               column(3, offset = 4, actionBttn("explant_search_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                               column(3, actionBttn("explant_search_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                               column(2, actionBttn('explant_search_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
                        )
               )
           )
  )