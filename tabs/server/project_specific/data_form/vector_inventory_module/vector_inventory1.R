
vector_inventory1 <- 
  tabPanel("Vector Inventory 1", value = "vector_inventory_1", hr(),
           useShinyalert(),
      
           fluidRow(
             column(2, textInput("vector_inventory_1_VectorID", "Vector ID", width = "100%")),
             column(2),
             column(2, br(), actionBttn("vector_inventory_1_ClearAllTheTabs", "Clear All the Tabs", size = "xs", color = "primary",
                                  style = "fill", block=T))
           ),
           panel_div(class_type = "default",
                     content = tags$div(
                       fluidRow(
                         column(2, textInput("vector_inventory_1_VectorPrefix", "Vector Prefix", width = "100%"),
                                shinyBS::bsTooltip("vector_inventory_1_VectorPrefix", "This is generated based on the project",
                                                   "right", options = list(container = "body"))),
                         div(id = "vector_inventory_1_form_a",
                               column(2, textInput("vector_inventory_1_VectorCode", labelMandatory("Vector Code"), width = "100%"),
                                      shinyBS::bsTooltip("vector_inventory_1_VectorCode", "A compulsory field to enter",
                                                         "right", options = list(container = "body"))),
                               column(2),
                               column(2, uiOutput("vector_inventory_1_VectorSuffix_output")),
                               column(2, selectInput("vector_inventory_1_BacterialSelection","Bacterial Selection", choices = NULL, width = "100%"))
                         )
                       ),
                div(id = "vector_inventory_1_form_b",       
                       fluidRow(
                         column(2, selectInput("vector_inventory_1_PlantSelection","Plant Selection", choices = NULL, width = "100%")),
                         column(2, textInput("vector_inventory_1_Synonym1", "Synonyms 1")),
                         column(2, textInput("vector_inventory_1_Synonym2", "Synonyms 2")),
                         column(2, textInput("vector_inventory_1_Synonym3", "Synonyms 3")),
                         column(2, textInput("vector_inventory_1_Synonym4", "Synonyms 4")),
                         column(2, textInput("vector_inventory_1_Synonym5", "Synonyms 5"))
                       ),
                       fluidRow(
                         column(1, br(), tags$b("Backbone")),
                         column(2, selectInput("vector_inventory_1_Backbone","", choices = NULL, width = "100%"))
                       ),
                       
                       fluidRow(
                         column(12,
                             column(1, br(), tags$h5(style="text-align:right;","VNTI Map")),
                             column(4, fileInput("vector_inventory_1_VNTILocate", "", multiple = F)), #Locate
                             column(3, verbatimTextOutput("vector_inventory_1_VNTIMap"))#vector_inventory_1_VNTI_Map_Location
                         ),
                         column(12,
                                column(1, br(), tags$h5(style="text-align:right;","Cloned By")),
                                column(2, selectInput("vector_inventory_1_ClonedBy","", choices = NULL, width = "100%")),
                                column(2, br(), tags$h5(style="text-align:right;","Date of Cloning")),
                                column(2, dateInput("vector_inventory_1_DateOfCloning","", width = "100%", value = NULL, min = NULL, max = NULL))
                                ), # vector_inventory_1_ClonedDate
                         column(12,
                                column(1, br(), tags$h5(style="text-align:right;","Lab Book Number")),
                                column(2, numericInput("vector_inventory_1_LabBookNumber", "", value = 0, width = "100%"))
                                ),
                         column(12,
                                column(1, br(), tags$h5(style="text-align:right;","Page Number")),
                                column(2, numericInput("vector_inventory_1_PageNumber", "", value = 0, width = "100%")),
                                column(1),
                                column(1, br(), tags$h5(style="text-align:right;","MTA File")),
                                column(4, fileInput("vector_inventory_1_MTAFileLocate", "", multiple = F)),
                                column(1, offset = 1, br(), actionBttn("vector_inventory_1_Clear", "Clear", size = "xs", color = "primary", style = "fill", block=T))
                         )
                       )
                    )
                )
  
            ), hr(),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("vector_inventory_1_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("vector_inventory_1_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('vector_inventory_1_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
)