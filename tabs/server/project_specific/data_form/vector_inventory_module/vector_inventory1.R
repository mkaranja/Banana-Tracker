
vector_inventory1 <- 
  tabPanel("Vector Inventory 1", value = "vector_inventory_1", hr(),
      div(  
           fluidRow(
             column(2, disabled(textInput("vector_inventory_1_VectorID", "Vector ID", width = "100%"))),
             column(1),
             column(2, br(), actionBttn("vector_inventory_1_ClearAllTheTabs", "Clear All the Tabs", size = "xs", color = "primary",
                                  style = "jelly"))
           ),
           br(),
           panel_div(class_type = "default",
                     content = tags$div(id = "vector_inventory_1_form",
           
                       fluidRow(
                         
                         column(2, textInput("vector_inventory_1_VectorPrefix", "Vector Prefix", width = "100%")),
                         column(2, textInput("vector_inventory_1_VectorCode", labelMandatory("Vector Code"), width = "100%")),
                         column(1),
                         column(2, disabled(textInput("vector_inventory_1_VectorSuffix","Vector Suffix", value = NULL, width = "100%"))),
                         column(2, selectInput("vector_inventory_1_BacterialSelection","Bacterial Selection", choices = NULL, width = "100%"))
                       ),
                       fluidRow(
                         column(2, selectInput("vector_inventory_1_PlantSelection","Plant Selection", choices = NULL)),
                         column(2, textInput("vector_inventory_1_synonyms1", "Synonyms 1")),
                         column(2, textInput("vector_inventory_1_synonyms2", "Synonyms 2")),
                         column(2, textInput("vector_inventory_1_synonyms3", "Synonyms 3")),
                         column(2, textInput("vector_inventory_1_synonyms4", "Synonyms 4")),
                         column(2, textInput("vector_inventory_1_synonyms5", "Synonyms 5"))
                       ),
                       fluidRow(
                         column(2, br(), h4("Features")),
                         column(1, br(), h4("Backbone")),
                         column(2, selectInput("vector_inventory_1_Backbone","", choices = NULL))
                       ),
                       
                       fluidRow(
                         column(12,
                             column(2, br(), tags$h5(style="text-align:right;","VNTI Map")),
                             column(1, br(), actionBttn("vector_inventory_1_VNTILocate", "Locate", size = "xs", color = "primary", style = "jelly"), block=T),
                             column(3, verbatimTextOutput("vector_inventory_1_VNTIMap"))
                         ),
                         column(12,
                                column(2, br(), tags$h5(style="text-align:right;","Cloned By")),
                                column(2, selectInput("vector_inventory_1_ClonedBy","", choices = NULL, width = "100%")),
                                column(2, br(), tags$h5(style="text-align:right;","Date of Cloning")),
                                column(2, dateInput("vector_inventory_1_DateOfCloning","", width = "100%"))
                                ),
                         column(12,
                                column(2, br(), tags$h5(style="text-align:right;","Lab Book Number")),
                                column(2, numericInput("vector_inventory_1_LabBookNumber", "", value = 0, width = "100%"))
                                ),
                         column(12,
                                column(2, br(), tags$h5(style="text-align:right;","Page Number")),
                                column(2, numericInput("vector_inventory_1_PageNumber", "", value = 0, width = "100%")),
                                column(1),
                                column(1, br(), actionBttn("vector_inventory_1_Clear", "Clear", size = "xs", color = "primary", style = "jelly", block = T))
                         )
                         
                       )
                    )
                )
  
  ),hr(), br(),
  fluidRow(
    column(12,
           column(5),
           column(2, actionBttn("xvector_inventory_1_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary", block = T)),
           column(1),
           column(2, actionBttn("xvector_inventory_1_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block = T)),
           column(1, actionBttn("xvector_inventory_1_Exit", "Exit", size = "xs", style = "jelly", color = "primary", block = T))
    )
  )
)