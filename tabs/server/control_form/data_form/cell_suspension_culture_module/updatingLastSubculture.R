


## ------------------------------------------------------- Updating Last CSC

update_last_subculture <- 
  tabPanel("Updating Last Subculture", value = "updating_last_subculture_CSC",
           br(), br(), 
           div(id = "updating_last_subculture_CSC_Form",
               fluidRow(
                 column(5,
                        column(6, br(), tags$p(style="text-align:right","Cell Suspension Culture Initiation Identity")),
                        column(6,uiOutput("updating_last_subculture_CSC_CellSuspensionCultureInitiationIdentity_output")),
                        column(6, br(), tags$p(style="text-align:right","Number of Cultures")),
                        column(6, numericInput("updating_last_subculture_CSC_NumberOfCultures", "", value = NULL, width = "100%")),
                        column(6, br(), tags$p(style="text-align:right","Date of Culture")),
                        column(6, disabled(dateInput("updating_last_subculture_CSC_DateOfCulture", "", value = NULL, width = "100%"))),
                        column(6, br(), tags$p(style="text-align:right",'Cultured By')),
                        column(6, disabled(selectInput('updating_last_subculture_CSC_CulturedBy', "", choices = NULL, width = "100%"))),
                        column(6, br(), tags$p(style="text-align:right","Media")),
                        column(6, disabled(selectInput("updating_last_subculture_CSC_Media", "", choices = NULL, width = "100%"))),
                        column(6, br(), tags$p(style="text-align:right","Address")),
                        column(6, disabled(textInput("updating_last_subculture_CSC_Address", "", width = "100%"))),
                        column(6, br(), tags$p(style="text-align:right","Lab Book Number")),
                        column(6, disabled(textInput("updating_last_subculture_CSC_LabBookNumber", "", width = "100%"))),
                        column(6, br(), tags$p(style="text-align:right","Page Number")),
                        column(6, disabled(numericInput("updating_last_subculture_CSC_PageNumber", "", value = NULL, width = "100%"))),
                        column(6, br(), tags$p(style="text-align:right","Comments")),
                        column(6, textAreaInput("updating_last_subculture_CSC_Comments", "", width = "100%"))
                 ),
                 column(1,br(), actionBttn("updating_last_subculture_CSC_LoadData", "Load Data", style = "fill", size = "xs", color = "primary")),
                 column(5, br(), 
                        "Only the last subculture will be loaded for updating")
               )
              ),
           fluidRow(
             column(2, offset = 3, actionBttn("updating_last_subculture_CSC_Update", "Update", style = "material-flat", size = "xs", color = "primary", block = T)),
             column(1),
             column(2, actionBttn("updating_last_subculture_CSC_Clear", "Clear", style = "material-flat", size = "xs", color = "primary", block = T))
           ), hr(),
           
           fluidRow(
             column(6),
             column(6,
                    column(3, offset = 4, actionBttn("updating_last_subculture_CSC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("updating_last_subculture_CSC_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn("updating_last_subculture_CSC_Exit", "Exit", style = "jelly", size = "xs", color = "danger", block=T)
                    )
             )
           )
  )

