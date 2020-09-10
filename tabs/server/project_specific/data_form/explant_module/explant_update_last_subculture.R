explant_update_last_subculture <- 
  tabPanel("Updating Last Subculture", value = "explant_updating_last_subculture",
           br(), br(), 
         fluidRow(
           div(id="explant_updating_last_subculture_Form",
           column(5,
                  column(4, br(), "Explant ID"),
                  column(6, selectizeInput("explant_updating_last_subculture_ExplantID", "", choices = NULL, width = "100%")),
                  column(2),
                  column(4, br(), "Number of Cultures"),
                  column(6, numericInput("explant_updating_last_subculture_NumberOfCultures", "", value = NULL, width = "100%")),
                  column(2),
                  column(4, br(), "Date of Culture"),
                  column(6, disabled(dateInput("explant_updating_last_subculture_DateOfCulture", "", value = NULL, width = "100%"))),
                  column(2),
                  column(4, br(), 'Cultured By'),
                  column(6, disabled(selectInput('explant_updating_last_subculture_CulturedBy', "", choices = NULL, width = "100%"))),
                  column(2),
                  column(4, br(), "Media"),
                  column(6, disabled(selectInput("explant_updating_last_subculture_Media", "", choices = NULL, width = "100%"))),
                  column(2),
                  column(4, br(), "Additives"),
                  column(6, disabled(textInput("explant_updating_last_subculture_Additives", "", width = "100%"))),
                  column(2),
                  column(4, br(), "Lab Book Number"),
                  column(6, disabled(textInput("explant_updating_last_subculture_LabBookNumber", "", width = "100%"))),
                  column(2),
                  column(4, br(), "Page Number"),
                  column(6, disabled(numericInput("explant_updating_last_subculture_PageNumber", "", value = NULL, width = "100%"))),
                  column(2),
                  column(4, br(), "Comments"),
                  column(6, textAreaInput("explant_updating_last_subculture_Comments", "", width = "100%"))
           )),
           column(1,br(), actionBttn("explant_updating_last_subculture_LoadData", "Load Data", style = "fill", size = "xs", color = "primary")),
           column(5, br(), 
                  "Only the last subculture will be loaded for updating")
         ),
         fluidRow(
           
         ),
         fluidRow(
           column(2, offset = 2, actionBttn("explant_updating_last_subculture_Update", "Update", style = "material-flat", size = "xs", color = "primary", block = T)),
           column(2),
           column(2, actionBttn("explant_updating_last_subculture_Clear", "Clear", style = "material-flat", size = "xs", color = "primary", block = T))
         ), hr(),
         
         fluidRow(
                  column(6),
                  column(6,
                         column(4),
                         column(3, actionBttn("explant_updating_last_subculture_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                         column(3, actionBttn("explant_updating_last_subculture_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                         column(2, actionBttn('explant_updating_last_subculture_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
                  )
         )
)