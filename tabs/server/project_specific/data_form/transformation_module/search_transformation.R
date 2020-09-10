

search_transformation <- tabPanel("Search Transformation", value = "search_transformation",
                                      br(),br(),
                                      useShinyjs(),
                                      
                                  div(id = "search_transformation_Form",     
                                      fluidRow(
                                        column(12,
                                               column(2, selectInput("search_transformation_Identity", "Identity", choices = NULL)),
                                               column(2, selectInput("search_transformation_Cultivar", "Cultivar", choices = NULL)),
                                               column(2, selectInput("search_transformation_VectorID", "Vector ID", choices = NULL)),
                                               column(1),
                                               column(2, selectInput("search_transformation_PlantSelection", "Plant Selection", choices = NULL)),
                                               column(3, selectInput("search_transformation_PromoterGene", "Promoter - Gene", choices = NULL))
                                               
                                               )
                                      ),
                                      fluidRow(
                                        column(4,  dateRangeInput("search_transformation_DateOfStarterCulture", "Date of Starter Culture")),
                                        column(8,
                                               column(2, actionBttn("search_transformation_ActionSearch", "Search", style = "fill", size = "xs", color = "primary", block=T)),
                                               column(2, actionBttn("search_transformation_ActionClearForm", "Clear Form", style = "fill", size = "xs", color = "primary", block=T)),
                                               conditionalPanel(
                                                 condition = "input.search_transformation_ActionSearch",
                                                     column(2, actionBttn("search_transformation_ActionCulture", "Culture", style = "fill", size = "xs", color = "primary", block=T)),
                                                     column(2, actionBttn("search_transformation_ActionDelete", "Delete Selected MFC", style = "fill", size = "xs", color = "primary", block=T))
                                               )
                                        )
                                      ),
                                      fluidRow(
                                        column(8, br(), uiOutput("search_Transformation_Table_Output"), br()),
                                        column(4, br(), awesomeCheckbox(inputId = "search_DeletedTransformations", label = "Search Deleted Transformations", status = "info", value = FALSE))
                                      ),
                                      fluidRow(
                                        div(id = "search_transformation_Culture_Form1",
                                            column(7, br(), rHandsontableOutput("search_transformation_CultureTable"), br(), br(),
                                                   uiOutput("search_transformation_Culture_Output")
                                                   )
                                        )
                                      ), hr(),
                                      fluidRow(
                                        column(6),
                                        column(6,
                                               column(4),
                                               column(3, actionBttn("search_transformation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                                               column(3, actionBttn("search_transformation_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                                               column(2, actionBttn('search_transformation_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
                                        )
                                      )
                                  )
)