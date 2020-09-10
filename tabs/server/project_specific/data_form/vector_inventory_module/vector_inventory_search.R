vector_inventory_search <- 
  tabPanel("Search", value = "vector_inventory_search", 
           useShinyalert(),
        div(id = "vector_inventory_search_form",
            panel_div(class_type = "default",
                content = tags$div(
                     fluidRow(
                       uiOutput("vector_inventory_search_Fields_Output"),
                       column(12,
                              column(2, 
                                    dateRangeInput("vector_inventory_search_ClonedDate", "Date of Cloning")
                                     ),
                              column(3, br(), tags$p(style="text-align:right", "Clear the Form Before Every New Search")),
                              column(1, br(), actionBttn("vector_inventory_search_Search", "Search", size = "xs", style = "fill", color = "primary", block=T),
                                     shinyBS::bsTooltip("vector_inventory_search_Search", "Press 'Search'",
                                                        "right", options = list(container = "body"))),
                              column(1, br(), actionBttn("vector_inventory_search_ClearForm", "Clear Form", size = "xs", style = "fill", color = "primary", block=T))
                              ),
                       column(8, br(), rHandsontableOutput("vector_inventory_search_ResultsTable"))
                     ),br(),
                     fluidRow(
                       column(2, textInput("vector_inventory_search_SelectedVectorID", "Selected Vector ID"))
                     ),br(),
                     fluidRow(
                       column(2, actionBttn("vector_inventory_search_LoadDataToView", "Load Data to View", size = "xs", style = "fill", color = "primary"),
                              shinyBS::bsTooltip("vector_inventory_search_LoadDataToView", "Press this button if you just want to view the data. Data will be loaded into the first five forms",
                                                 "right", options = list(container = "body"))
                              ),
                       column(2, actionBttn("vector_inventory_search_LoadDataToUpdate", "Load Data to Update", size = "xs", style = "fill", color = "primary"),
                              shinyBS::bsTooltip("vector_inventory_search_LoadDataToUpdate", "Press this for loading data to update the record. Data will be loaded to the first five forms.
                                                 Make the changes and press the 'save' button in 'Vector Inventory 5' Tab",
                                                 "right", options = list(container = "body"))),
                       column(2, actionBttn("vector_inventory_search_DeleteTheSelectedVector", "Delete the Selected Vector", size = "xs", style = "fill", color = "primary"))
                     )
                    )
                )), 
        fluidRow(
          tableOutput("tab")
        ),
        fluidRow(
          column(6),
          column(6,
                 column(4),
                 column(3, actionBttn("vector_inventory_search_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                 column(3, actionBttn("vector_inventory_search_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                 column(2, actionBttn('vector_inventory_search_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
          )
        )
           
  )