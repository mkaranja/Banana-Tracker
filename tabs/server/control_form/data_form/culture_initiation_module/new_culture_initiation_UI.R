## ------------------------------------------------------------- New Culture

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

new_culture_initiation <- 
  
  tabPanel("New Culture Intiation", value = "new_culture_initiation",
   br(),br(), useShinyalert(),
   div(id = "new_culture_initiation_Form",
       fluidRow(
         column(12,
                column(3,
                       selectizeInput("new_culture_initiation_CultureType", labelMandatory("Culture Type"), 
                                      choices = c("","Male Flower Culture (MFC)" = "MFC", "Scalp Culture (SPC)" = "SPC"), multiple = F),
                       disabled(textInput("new_culture_initiation_ExplantIdentity", labelMandatory("Explant Identity"))),
                       disabled(textInput("new_culture_initiation_ExplantIdentityType", labelMandatory("Explant Identity Type")))
                ),
                column(2, br(),br(),
                       actionBttn("new_culture_initiation_GenerateIdentity", "Generate Identity", size= "xs", style = "fill", color = "primary"),
                       shinyBS::bsTooltip("new_culture_initiation_GenerateIdentity", "Please select the correct year first",
                                          "right", options = list(container = "body"))),
                column(2, selectInput("new_culture_initiation_Year", "Year", choices = c(seq(2000,lubridate::year(Sys.Date()),1)),selected = lubridate::year(Sys.Date()))),
                column(1),
                column(4,
                       conditionalPanel(condition = "input.new_culture_initiation_ReadyToCulture=='Yes'",
                                        numericInput("new_culture_initiation_NumberOfCultures", labelMandatory("Number of Cultures"), value = 0),
                                        dateInput("new_culture_initiation_DateOfCultures", labelMandatory("Date of Culture"), value = NULL),
                                        uiOutput("new_culture_initiation_CulturedBy_Output"), 
                                        textAreaInput("new_culture_initiation_Comments1",  "Comments")
                       ))
         )
       ),
       fluidRow(
         column(12,
                column(3, uiOutput("new_culture_initiation_Cultivar_Output"))
         ),
         column(12,
                column(3, selectizeInput("new_culture_initiation_CultivarConfirmed",labelMandatory("Cultivar Confirmed"), choices = c("","Yes","No"), multiple = F)),
                column(6, textAreaInput("new_culture_initiation_CultivarConfirmedComments", "Comments", width = "100px"))
         )
       ),
       fluidRow(
         column(12,
                column(3, uiOutput("new_culture_initiation_Source_Output"))
         ),
         column(12,
                column(3, selectizeInput("new_culture_initiation_VirusIndexed", labelMandatory("Virus Indexed"), choices = c("","Yes", "No"), multiple = F)),
                column(1, conditionalPanel(condition = "input.new_culture_initiation_VirusIndexed=='Yes'", br(), br(), p("IF 'Yes'"))),
                column(2, conditionalPanel(condition = "input.new_culture_initiation_VirusIndexed=='Yes'", dateInput("new_culture_initiation_VirusIndexedDate", "Virus Indexed Date", value = "1980-01-01"))),
                column(2, conditionalPanel(condition = "input.new_culture_initiation_VirusIndexed=='Yes'", uiOutput("new_culture_initiation_VirusIndexedBy_Output"))),
                column(2, br(), br(), 
                       conditionalPanel(
                         condition = "input.new_culture_initiation_ReadyToCulture=='No'",
                         actionBttn("new_culture_initiation_SaveStarterCulture", "Save Starter Culture", style = "fill", size = "xs", color = "primary")
                       ),
                       conditionalPanel(
                         condition = "input.new_culture_initiation_ReadyToCulture=='Yes'",
                         actionBttn("new_culture_initiation_SaveStarterCultureAndSubCulture", "Save Starter Culture and Sub Culture", style = "fill", size = "xs", color = "primary")
                       ),
                )
         )
       ),
       fluidRow(
         column(11,
                shiny::uiOutput("new_culture_initiation_PermitType_Output")
         )
       ),
       fluidRow(
         column(11,
                column(2, numericInput("new_culture_initiation_PermitNumber1", labelMandatory("Permit Number"), value = NULL)),
                column(2, numericInput("new_culture_initiation_PermitNumber2", "Permit Number", value = NULL)),
                column(2, numericInput("new_culture_initiation_PermitNumber3", "Permit Number", value = NULL)),
                column(2, numericInput("new_culture_initiation_PermitNumber4", "Permit Number", value = NULL)),
                column(2, numericInput("new_culture_initiation_PermitNumber5", "Permit Number", value = NULL)),
                column(2, numericInput("new_culture_initiation_PermitNumber6", "Permit Number", value = NULL))
         )
       ),
       fluidRow(
         column(12,
                column(3,
                       dateInput("new_culture_initiation_DateOfStarterCulture", labelMandatory("Date of Initial Culture"), value=NULL),
                       uiOutput("new_culture_initiation_Media_Output"),
                       tags$b("Additives"),
                       panel_div(class_type = "default", 
                                 content = tags$div(uiOutput("new_culture_initiation_Additives_Output")
                                 )),
                       #selectizeInput("new_culture_initiation_Additives", "Additives", choices = c("", additives$Additives), multiple = T),
                       numericInput("new_culture_initiation_LabBookNumber", labelMandatory("Lab Book Number"), value = 0),
                       numericInput("new_culture_initiation_PageNumber", labelMandatory("Page Number"), value = 0),
                       radioGroupButtons(inputId = "new_culture_initiation_ReadyToCulture",label = "Ready to Culture?",
                                         choices = c("Yes", "No"), selected = "No", individual = TRUE, size ="xs",status = "info",
                                         checkIcon = list(yes = icon("ok", lib = "glyphicon")))
                ),
                column(5, br(),br(),
                       conditionalPanel(condition = "input.new_culture_initiation_SaveStarterCultureAndSubCulture",
                                        panel_div(class_type = "default",
                                                  content = tags$div(
                                                    p("Select the Fields Required to be Exported to Excel"),
                                                    column(9,uiOutput("select_fields_to_be_exported_to_excel_Output")),
                                                    column(3,  br(),br(), downloadBttn("new_culture_initiation_SelectFieldToExportToExcel", "Export to Excel", style = "bordered", size = "s", color = "primary"))
                                                  ))     
                       )
                ),
                column(4, align="center", br(),br(),br(),
                       panel_div(class_type = "default",
                                 content = tags$div(
                                   actionBttn("new_culture_initiation_Refresh", "Refresh", style = "fill", size = "xs", color = "primary"), br(), br(),
                                   selectInput("new_culture_initiation_ExplantIdentiy", "Explant Identity", choices = NULL, multiple = F), br(),
                                   actionBttn("new_culture_initiation_LoadData", "Load Data", style = "fill", size = "xs", color = "primary"), br(), br(),
                                   actionBttn("new_culture_initiation_Update", "Update", style = "fill", size = "xs", color = "primary")
                                 ))
                )
         )
       ),
       fluidRow(
         verbatimTextOutput("tbl"),
         column(2, offset = 5,
                actionBttn("new_culture_initiation_ClearForm", "Clear Form", style = "jelly", size = "xs", color = "primary")
         )
         
       ), hr(),
       fluidRow(
         column(6),
         column(6,
                column(4),
                column(3, actionBttn("new_culture_initiation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                column(3, actionBttn("new_culture_initiation_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                column(2, actionBttn('new_culture_initiation_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T)
                )
         )
       )
   )
                                   
                                   
)

