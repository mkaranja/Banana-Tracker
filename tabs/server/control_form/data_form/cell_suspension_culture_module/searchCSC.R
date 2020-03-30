search_CSC <- 
  tabPanel("Search CSC", value = "search_CSC",
           br(), br(),             
       fluidRow(
         column(2, selectInput("search_CSC_Identify", h5("Identify"), choices = NULL)),
         column(2, selectInput("search_CSC_Source", h5("Source"), choices = NULL)),
         column(2, selectInput("search_CSC_Cultivar", h5("Cultivar"), choices = NULL)),
         column(2, selectInput("search_CSC_CultivarConformed", h5("Cultivar Confirmed"), choices = NULL)),
         column(2, selectInput("search_CSC_VirusIndexed", h5("Virus Indexed"), choices = NULL)),
       ),
       fluidRow(
         column(4, dateRangeInput("search_CSC__DateOfStarterCultureForm", h5("Date of Starter Culture Form"))),
         column(8,
                radioGroupButtons(inputId = "search_CSC_Action",label = h5("Select Only One Condition Per "),
                                  choices = c("Search","Clear Form", "Culture", "Delete Selected MFC"),individual = TRUE, size ="sm",
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon")))
         )
       ),
       fluidRow(
         column(8, rHandsontableOutput("search_CSC_ResultsTable", height = "300px")),
         column(4, awesomeCheckbox(inputId = "search_CSC_SearchDeletedMFC", label = "Search Deleted MFC", value = TRUE, status = "primary"))
       ),
       fluidRow(
         column(4, rHandsontableOutput("search_CSC_ExplantIdentiyResultsTable", height = "200px")),
         column(4, 
                column(5, br(), h6("Selected Identity")),
                column(7, textInput("search_CSC_SelectedIdentity","", width = "100%")),
                
                column(5, br(), h6("Number of Culture")),
                column(7, textInput("search_CSC_NumberOfCulture","", width = "100%")),
                
                column(5, br(), h6("Date of Cultures")),
                column(7, textInput("search_CSC_DateOfCulture","", width = "100%")),
                
                column(5, br(), h6("Cultured By")),
                column(7, textInput("search_CSC_CulturedBy","", width = "100%")),
                
                column(5, br(), h6("Media")),
                column(7, textInput("search_CSC_Media","", width = "100%")),
                
                column(5, br(), h6("Lab Book Number")),
                column(7, textInput("search_CSC_LabBookNumber","", width = "100%")),
                
                column(5, br(), h6("Page Number")),
                column(7, textInput("search_CSC_PageNumber","", width = "100%")),
                
                column(5, br(), h6("Comments")),
                column(7, textInput("search_CSC_Comments", "", width = "100%"))
                
         )
       )
)
