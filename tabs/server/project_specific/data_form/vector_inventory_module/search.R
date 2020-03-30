search <- 
  tabPanel("Search", value = "vector_inventory_search", hr(),
           
        div(id = "vector_inventory_search_form",
           fluidRow(
             verbatimTextOutput("tc"),
             column(12,
                   # uiOutput("vector_inventory_search_Output")
                    column(2, selectInput("vector_inventory_search_VectorID", "Vector ID", choices = NULL)),
                    column(2, textInput("vector_inventory_search_Synonyms", "Synonyms")),
                    column(1, selectInput("vector_inventory_search_Promoter", "Promoter", choices = NULL)),
                    column(1, selectInput("vector_inventory_search_Gene", "Gene", choices = NULL)),
                    column(2, selectInput("vector_inventory_search_Terminator", "Terminator", choices = NULL)),
                    column(2, selectInput("vector_inventory_search_Backbone", "Backbone", choices = NULL)),
                    column(2, selectInput("vector_inventory_search_CloneddBy", "Cloned By", choices = NULL))
                    ),
             column(12,
                    column(2, dateRangeInput("vector_inventory_search_DateOfCloning", "Date of Cloning")),
                    column(3, br(), tags$p(style="text-align:right", "Clear the Form Before Every New Search")),
                    column(1, br(), actionBttn("vector_inventory_search_Search", "Search", size = "xs", style = "jelly", color = "primary", block = T)),
                    column(1, br(), actionBttn("vector_inventory_search_ClearForm", "Clear Form", size = "xs", style = "jelly", color = "primary", block = T))
                    ),
             column(8, rHandsontableOutput("vector_inventory_search_ResultsTable"))
           ),br(),
           fluidRow(
             column(2, textInput("vector_inventory_search_SelectedVectorID", "Selected Vecoer ID"))
           ),br(),
           fluidRow(
             column(2, actionBttn("vector_inventory_search_LoadDataToView", "Load Data to View", size = "xs", style = "jelly", color = "primary", block = T)),
             column(2, actionBttn("vector_inventory_search_LoadDataToUpdate", "Load Data to Update", size = "xs", style = "jelly", color = "primary", block = T))
           )
          ), hr(), br(),
         fluidRow(
           column(2, offset = 5, actionBttn("vector_inventory_search_FormToPicture", "Form To Picture", size = "xs", style = "jelly", color = "primary", block = T)),
           column(2, offset = 1, actionBttn("vector_inventory_search_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block = T)),
           column(1, actionBttn("vector_inventory_search_Exit", "Exit", size = "xs", style = "jelly", color = "primary", block = T))
         )
           
  )