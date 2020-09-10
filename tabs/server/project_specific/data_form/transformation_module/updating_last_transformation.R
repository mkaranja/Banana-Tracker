updating_last_transformation <- tabPanel("Updating Last Transformation", value = "updating_last_transformation",
                                     br(),br(),
                              div(id = "updating_last_transformation_Form",       
                                     fluidRow(
                                       column(10,offset = 1,
                                           column(4,
                                                  selectizeInput("updating_last_TransformationIdentity", "Transformation Identity",choices = c(""), width = "100%")
                                                 ),
                                           column(1, br(), actionBttn("updating_last_transformation_LoadData", "Load Data", style = "fill", size = "xs", color = "primary", block = T)),
                                           column(4, br(), "Only the last transformation will be loaded for updating")
                                           )
                                       ), br(), br(),
                                     fluidRow(
                                       column(10,offset = 1,
                                         column(4,
                                                numericInput("updating_last_transformation_NumberOfCultures", "Number of Cultures", value = NULL, min = 0, max = 255, width = "100%"),
                                                disabled(dateInput("updating_last_transformation_DateOfCulture", "Date of Culture", value = NULL, width = "100%")),
                                                disabled(selectInput('updating_last_transformation_CulturedBy', "Cultured By", choices = NULL, width = "100%")),
                                                disabled(selectInput("updating_last_transformation_Media", "Media", choices = NULL, width = "100%")),
                                                disabled(textInput("updating_last_transformation_Additives", "Additives", width = "100%")),
                                                disabled(numericInput("updating_last_transformation_LabBookNumber", "Lab Book Number", value = NULL,min = 0, max = 767, width = "100%")),
                                                disabled(numericInput("updating_last_transformation_PageNumber", "Page Number", value = NULL,min = 0, max = 767, width = "100%")),
                                                textAreaInput("updating_last_transformation_Comments", "Comments", width = "100%")
                                         )
                                       )
                                     )
                                  ),
                                     fluidRow(
                                       
                                     ),
                                     fluidRow(
                                       column(10, offset = 1,
                                         column(2, offset = 2, actionBttn("updating_last_transformation_Update", "Update", style = "material-flat", size = "xs", color = "primary")),
                                         column(2),
                                         column(2, actionBttn("updating_last_transformation_Clear", "Clear", style = "material-flat", size = "xs", color = "primary"))
                                     )),hr(),
                              fluidRow(
                                column(6),
                                column(6,
                                       column(4),
                                       column(3, actionBttn("updating_last_transformation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                                       column(3, actionBttn("updating_last_transformation_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                                       column(2, actionBttn('updating_last_transformation_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
                                )
                              )
)