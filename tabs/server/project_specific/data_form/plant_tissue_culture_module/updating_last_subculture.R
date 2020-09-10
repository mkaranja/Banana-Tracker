updating_last_subculture <- tabPanel("Updating Last Subculture", value = "updating_last_subculture_PTC",
                                         br(),br(),
                                         div(id = "updating_last_subculture_PTCForm",       
                                             fluidRow(
                                               column(10,offset = 1,
                                                      column(4,
                                                             selectizeInput("updating_last_subculture_PTCIdentity", "Plant Tissue Culture Identity",
                                                                            choices = NULL, width = "100%")
                                                      ),
                                                      column(2, br(), actionBttn("updating_last_subculture_PTCLoadData", "Load Data", style = "fill", size = "xs", color = "primary")),
                                                      column(4, br(), "Only the last subculture will be loaded for updating")
                                               )
                                             ), br(), br(),
                                             fluidRow(
                                               column(10,offset = 1,
                                                      column(4,
                                                             numericInput("updating_last_subculture_PTCNumberOfCultures", "Number of Cultures", value = NULL, min = 0, max = 255, width = "100%"),
                                                             disabled(dateInput("updating_last_subculture_PTCDateOfCulture", "Date of Culture",value = NULL, width = "100%")),
                                                             disabled(selectInput('updating_last_subculture_PTCCulturedBy', "Cultured By", choices = NULL, width = "100%")),
                                                             disabled(selectInput("updating_last_subculture_PTCMedia", "Media", choices = NULL, width = "100%")),
                                                             disabled(textInput("updating_last_subculture_PTCAdditives", "Additives", width = "100%")),
                                                             disabled(numericInput("updating_last_subculture_PTCLabBookNumber", "Lab Book Number", value = NULL,min = 0, max = 767, width = "100%")),
                                                             disabled(numericInput("updating_last_subculture_PTCPageNumber", "Page Number", value = NULL,min = 0, max = 767, width = "100%")),
                                                             textAreaInput("updating_last_subculture_PTCComments", "Comments", width = "100%")
                                                      )
                                               )
                                             )
                                         ),
                                         fluidRow(
                                           
                                         ),
                                         fluidRow(
                                           column(10, offset = 1,
                                                  column(2, actionBttn("updating_last_subculture_PTCUpdate", "Update", style = "fill", size = "xs", color = "primary")),
                                                  column(2, actionBttn("updating_last_subculture_PTCClear", "Clear", style = "fill", size = "xs", color = "primary"))
                                           )),hr(),
                                         fluidRow(
                                           column(6),
                                           column(6,
                                                  column(4),
                                                  column(3, actionBttn("updating_last_subculture_PTCFormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                                                  column(3, actionBttn("updating_last_subculture_PTCControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                                                  column(2, actionBttn('updating_last_subculture_PTCExit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
                                           )
                                         )
)