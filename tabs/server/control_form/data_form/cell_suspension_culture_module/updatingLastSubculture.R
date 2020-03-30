update_last_subcultureCSC <- 
  tabPanel("Updating Last Subculture", value = "updating_last_subculture",
           br(), br(), 
         fluidRow(
           column(5,
                  column(6, br(), "Culture Initiation Identity"),
                  column(6, disabled(selectizeInput("updating_last_subculture_CultureInitiationIdentityCSC", "", choices = NULL, width = "100%"))),
                  
                  column(6, br(), "Number of Cultures"),
                  column(6, numericInput("updating_last_subculture_NumberOfCulturesCSC", "", value = NULL, width = "100%")),
                  
                  column(6, br(), "Date of Culture"),
                  column(6, disabled(dateInput("updating_last_subculture_DateOfCultureCSC", "", value = NULL, width = "100%"))),
                  
                  column(6, br(), 'Cultured By'),
                  column(6, disabled(selectInput('updating_last_subculture_CulturedByCSC', "", choices = NULL, width = "100%"))),
                  
                  column(6, br(), "Media"),
                  column(6, disabled(selectInput("updating_last_subculture_MediaCSC", "", choices = NULL, width = "100%"))),
                  
                  column(6, br(), "Address"),
                  column(6, disabled(textInput("updating_last_subculture_AddressCSC", "", width = "100%"))),
                  
                  column(6, br(), "Lab Book Number"),
                  column(6, disabled(textInput("updating_last_subculture_LabBookNumberCSC", "", width = "100%"))),
                  
                  column(6, br(), "Page Number"),
                  column(6, disabled(numericInput("updating_last_subculture_PageNumberCSC", "", value = NULL, width = "100%"))),
                  
                  column(6, br(), "Comments"),
                  column(6, textAreaInput("updating_last_subculture_CommentsCSC", "", width = "100%"))
           ),
           column(2,
                  actionBttn("updating_last_subculture_LoadDataCSC", "Load Data", style = "fill", size = "xs", color = "primary", block = T)),
           column(5, br(), 
                  "Only the last subculture will be loaded for updating")
         ),
         fluidRow(
           
         ),
         fluidRow(
           column(2, offset = 2, actionBttn("updating_last_subculture_UpdateCSC", "Update", style = "material-flat", size = "xs", color = "primary", block = T)),
           column(2),
           column(2, actionBttn("updating_last_subculture_ClearCSC", "Clear", style = "material-flat", size = "xs", color = "primary", block = T))
         )
)