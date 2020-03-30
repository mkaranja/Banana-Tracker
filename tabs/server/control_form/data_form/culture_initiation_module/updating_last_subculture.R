updating_last_subculture <- tabPanel("Updating Last Subculture", value = "updating_last_subculture",
                                     br(),br(),
                              div(id = "updating_last_subculture_Form",       
                                     fluidRow(
                                       column(10,offset = 1,
                                           column(4,
                                                  selectizeInput("updating_last_subculture_CultureInitiationIdentity", "Culture Initiation Identity",
                                                                 choices = c('', mfc$ExplantIdentity), width = "100%")
                                                 ),
                                           column(1, br(), actionBttn("updating_last_subculture_LoadData", "Load Data", style = "fill", size = "xs", color = "primary", block = T)),
                                           column(4, br(), "Only the last subculture will be loaded for updating")
                                           )
                                       ), br(), br(),
                                     fluidRow(
                                       column(10,offset = 1,
                                         column(4,
                                                numericInput("updating_last_subculture_NumberOfCultures", "Number of Cultures", value = NULL, min = 0, max = 255, width = "100%"),
                                                disabled(dateInput("updating_last_subculture_DateOfCulture", "Date of Culture", value = NULL, width = "100%")),
                                                disabled(selectInput('updating_last_subculture_CulturedBy', "Cultured By", choices = NULL, width = "100%")),
                                                disabled(selectInput("updating_last_subculture_Media", "Media", choices = NULL, width = "100%")),
                                                disabled(textInput("updating_last_subculture_Additives", "Additives", width = "100%")),
                                                disabled(numericInput("updating_last_subculture_LabBookNumber", "Lab Book Number", value = NULL,min = 0, max = 767, width = "100%")),
                                                disabled(numericInput("updating_last_subculture_PageNumber", "Page Number", value = NULL,min = 0, max = 767, width = "100%")),
                                                textAreaInput("updating_last_subculture_Comments", "Comments", width = "100%")
                                         )
                                       )
                                     )
                                  ),
                                     fluidRow(
                                       
                                     ),
                                     fluidRow(
                                       column(10, offset = 1,
                                         column(2, offset = 2, actionBttn("updating_last_subculture_Update", "Update", style = "material-flat", size = "sm", color = "primary")),
                                         column(2),
                                         column(2, actionBttn("updating_last_subculture_Clear", "Clear", style = "material-flat", size = "sm", color = "primary"))
                                     )),
                                     fluidRow(br(),hr(),
                                              column(8, offset = 2,
                                                     column(2, actionBttn("updating_last_subculture_FormToPicture", "Form to Picture", style = "jelly", size = "sm", color = "primary", block=T)),
                                                     column(4, actionBttn("updating_last_subculture_MFC_SCP_CSC_ControlForm", "MFC, SCP and CSC Control Form", style = "jelly", size = "sm", color = "warning", block=T)),
                                                     column(2, actionBttn("updating_last_subculture_Exit", "Exit", style = "jelly", size = "sm", color = "primary", block=T))
                                              )# actionBttn("admin_Source_MFC_SCP_CSC_ControlForm", "MFC SCP CSC ControlForm", style = "jelly", size = "xs", color = "warning", block=T)
                                     )
)