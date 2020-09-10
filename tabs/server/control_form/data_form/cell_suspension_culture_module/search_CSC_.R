## -------------------------------------------------------- Search CSC

search_CSC <- 
  tabPanel("Search CSC", value = "search_CSC",
           br(),br(),
           useShinyjs(),
           
           div(id = "search_CSC_Form",     
               fluidRow(
                 uiOutput("search_CSC_Fields_Output")
               ),
               fluidRow(
                 column(3, 
                        uiOutput("search_CSC_DateOfStarterCulture_Output")
                 ),
                 column(8,
                        column(2, br(), actionBttn("search_CSC_ActionSearch", "Search", style = "fill", size = "xs", color = "primary", block=T)),
                        column(2, br(), actionBttn("search_CSC_ActionClearForm", "Clear Form", style = "fill", size = "xs", color = "primary", block=T)),
                        conditionalPanel(
                          condition = "input.search_CSC_ActionSearch",
                          
                          column(2, br(), actionBttn("search_CSC_ActionCulture", "Culture", style = "fill", size = "xs", color = "primary", block=T)),
                          column(2, br(), actionBttn("search_CSC_ActionDelete", "Delete Selected CSC", style = "fill", size = "xs", color = "primary", block=T))
                        )
                 )
               ),br(), 
               fluidRow(
                 column(10, uiOutput("search_CSC_ResultsTable_Output")),
                 column(2, shinyWidgets::awesomeCheckbox(inputId = "search_CSC_SearchDeletedCSC", label = "Search Deleted CSC", status = "info", value = F))
               ),
               fluidRow(
                 div(id = "search_CSC_Culture_Form1",
                     column(5, br(), rHandsontableOutput("search_CSC_ExplantIdentiyResultsTable")),
                     uiOutput("search_CSC_ActionCulture_Output")
                 )
               ), hr(),
               fluidRow(
                 column(6),
                 column(6,
                        column(4),
                        column(3, actionBttn("search_CSC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                        column(3, actionBttn("search_CSC_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                        column(2, actionBttn('search_CSC_Exit', "Exit",  style = "jelly", size = "xs", color = "danger", block=T))
                 )
               )
           )
  )
