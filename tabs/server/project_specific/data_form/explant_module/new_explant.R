new_explant <- 
  tabPanel("New Explant", value = "new_explant",
           br(), br(), 
           useShinyalert(),
           div(id = "new_explant_form",
               fluidRow(
                 column(12,
                        column(3,
                               selectizeInput("new_explant_ParentIdentity", labelMandatory("Parent Identity"), choices = NULL, multiple = F),
                               disabled(textInput("new_explant_Identity", tags$p("Identity"))),
                               disabled(textInput("new_explant_IdentityType", tags$p("Identity Type"))),
                               disabled(textInput("new_explant_Cultivar", tags$p("Cultivar"))),
                               disabled(textInput("new_explant_CultivarConfirmed",tags$p("Cultivar Confirmed"))),
                               disabled(textInput("new_explant_Source", tags$p("Source"))),
                               disabled(textInput("new_explant_VirusIndexed", tags$p("Virus Indexed")))
                        ),
                        column(1, br(),
                               actionBttn("new_explant_GetData", "Get Data", size= "xs", style = "fill", color = "primary", block = T),br(),br(),
                               actionBttn("new_explant_GenerateIdentity", "Generate Identity", size= "xs", style = "fill", color = "primary", block = T)
                        ),
                        column(1, br(),br(),br(), selectizeInput("new_explant_Year", "Year", 
                                                                 choices = c(seq(from = 2000, to = lubridate::year(Sys.Date()),1)), multiple = F)),
                        column(4, offset = 2,
                               conditionalPanel(condition = "input.new_explant_ReadyToCulture=='Yes'",
                                                numericInput("new_explant_NumberOfCultures", labelMandatory("Number of Cultures"), value = NULL),
                                                dateInput("new_explant_DateOfCultures", labelMandatory("Date of Culture")),
                                                selectInput("new_explant_CulturedBy", labelMandatory("Cultured By"), choices = c('')),
                                                textAreaInput("new_explant_Comments", "Comments"), br(), br(),br(), br(),br(), br(),br(), br(),
                                                actionBttn("new_explant_SaveStarterCulture_Ready_Yes", "Save Starter Culture", style = "fill", size = "sm", color = "primary")
                               ),
                               conditionalPanel(
                                 condition = "input.new_explant_ReadyToCulture=='No'",
                                 br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                 actionBttn("new_explant_SaveStarterCulture_Ready_No", "Save Starter Culture", style = "fill", size = "sm", color = "primary")
                               )
                        )
                 )
               ),
               fluidRow(
                 column(11,
                        column(2, br(), tags$b("Permit Type")),
                        column(10,
                               column(2, disabled(textInput("new_explant_PermitType1", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitType2", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitType3", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitType4", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitType5", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitType6", "", width = "100%")))
                        )
                 )
               ),
               fluidRow(
                 column(11,
                        column(2, br(), tags$b("Permit Number")),
                        column(10,
                               column(2, disabled(textInput("new_explant_PermitNumber1", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitNumber2", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitNumber3", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitNumber4", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitNumber5", "", width = "100%"))),
                               column(2, disabled(textInput("new_explant_PermitNumber6", "", width = "100%")))
                        )
                 )
               ),
               fluidRow(
                 column(4,
                        column(5, br(), tags$b("Explant Initial Culture Date")),
                        column(6, dateInput("new_explant_ExplantInitialCultureDate", "")),
                        
                        column(5, br(), labelMandatory(tags$b("Media"))),
                        column(6, selectInput("new_explant_Media", "", choices = c(''), multiple = F)),
                        
                        column(5, br(), tags$b("Additives")),
                        column(6, selectInput("new_explant_Additives", "", choices =  c(''), multiple = T)),
                        
                        column(5, br(), tags$b("Lab Book Number")),
                        column(6, numericInput("new_explant_LabBookNumber", "", value = NULL)),
                        
                        column(5, br(), tags$b("Page Number")),
                        column(6, numericInput("new_explant_PageNumber", "", value = NULL)),
                        
                        column(5, br(), tags$b("Ready to Culture?")),
                        column(6, radioGroupButtons(inputId = "new_explant_ReadyToCulture",label = "",  choices = c("Yes", "No"), selected = "No",
                                                    individual = TRUE, size ="sm",status = "info", checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
                 ),
                 column(3, br(),br(),
                        conditionalPanel(condition = "input.new_explant_SaveStarterCulture_Ready_Yes",# new_explant_SaveStarterCulture_Ready_Yes
                                         panel_div(class_type = "default",
                                                   content = tags$div(
                                                     tags$p("Select the Fields Recurred to be Exported to Excel"),
                                                     column(8,
                                                            awesomeCheckboxGroup(inputId = "select_Explant_fields_to_be_exported_to_excel", label = "",
                                                                                 choices = NULL, selected = NULL, status = "info")),
                                                     column(4, br(),br(),br(),br(),
                                                            downloadBttn("new_explant_SelectFieldToExportToExcel", "Export to Excel", style = "bordered", size = "xs", color = "primary")
                                                     )
                                                   ))
                        )
                 ),
                 column(4)
               )
           ),
           fluidRow(
             column(2, offset = 6, actionBttn("new_explant_ClearForm", "Clear Form", style = "fill", size = "xs", color = "primary"))
           ), hr(),
           
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("new_explant_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("new_explant_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('new_explant_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
  )

#div(style="display:inline-block",downloadButton('downloadData', 'Download Data'), style="float:right")