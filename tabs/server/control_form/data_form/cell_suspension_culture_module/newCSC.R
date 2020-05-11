new_cell_suspension_culture <- 
  tabPanel("New CSC", value = "new_CSC",
     br(), br(), 
     useShinyalert(),
     div(id = "new_CSC_form",
         fluidRow(
           column(12,
               column(3,
                      selectizeInput("new_CSC_ParentIdentity", labelMandatory("Parent Identity"), choices = NULL, multiple = F),
                      disabled(textInput("new_CSC_Identity", tags$p("Identity"))),
                      disabled(textInput("new_CSC_IdentityType", tags$p("Identity Type"))),
                      disabled(textInput("new_CSC_Cultivar", tags$p("Cultivar"))),
                      disabled(textInput("new_CSC_CultivarConfirmed",tags$p("Cultivar Confirmed"))),
                      disabled(textInput("new_CSC_Source", tags$p("Source"))),
                      disabled(textInput("new_CSC_VirusIndexed", tags$p("Virus Indexed")))
               ),
               column(1, br(),
                      actionBttn("new_CSC_GetData", "Get Data", size= "xs", style = "fill", color = "primary", block = T),br(),br(),
                      actionBttn("new_CSC_GenerateIdentity", "Generate Identity", size= "xs", style = "fill", color = "primary", block = T)
               ),
               column(1, br(),br(),br(), selectizeInput("new_CSC_Year", "Year", 
                                                        choices = c(seq(from = 2000, to = lubridate::year(Sys.Date()),1)), multiple = F)),
               column(4, offset = 2,
                      conditionalPanel(condition = "input.new_CSC_ReadyToCulture=='Yes'",
                             numericInput("new_CSC_NumberOfCultures", labelMandatory("Number of Cultures"), value = NULL),
                             dateInput("new_CSC_DateOfCultures", labelMandatory("Date of Culture")),
                             selectInput("new_CSC_CulturedBy", labelMandatory("Cultured By"), choices = c('', cultured_by$CulturedBy)),
                             textAreaInput("new_CSC_Comments", "Comments"), br(), br(),br(), br(),br(), br(),br(), br(),
                             actionBttn("new_CSC_SaveStarterCulture_Ready_Yes", "Save Starter Culture", style = "fill", size = "sm", color = "primary")
                      ),
                      conditionalPanel(
                        condition = "input.new_CSC_ReadyToCulture=='No'",
                        br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        actionBttn("new_CSC_SaveStarterCulture_Ready_No", "Save Starter Culture", style = "fill", size = "sm", color = "primary")
                      )
               )
           )
         ),
         fluidRow(
           column(11,
                  column(2, br(), tags$b("Permit Type")),
                  column(10,
                         column(2, disabled(textInput("new_CSC_PermitType1", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitType2", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitType3", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitType4", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitType5", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitType6", "", width = "100%")))
             )
           )
         ),
         fluidRow(
           column(11,
                  column(2, br(), tags$b("Permit Number")),
                  column(10,
                         column(2, disabled(textInput("new_CSC_PermitNumber1", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitNumber2", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitNumber3", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitNumber4", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitNumber5", "", width = "100%"))),
                         column(2, disabled(textInput("new_CSC_PermitNumber6", "", width = "100%")))
                  )
           )
         ),
         fluidRow(
           column(4,
                  column(5, br(), tags$b("CSC Initial Culture Date")),
                  column(6, dateInput("new_CSC_CSCInitialCultureDate", "")),
                  
                  column(5, br(), labelMandatory(tags$b("Media"))),
                  column(6, selectInput("new_CSC_Media", "", choices = c('', media$Media), multiple = F)),
                  
                  column(5, br(), tags$b("Additives")),
                  column(6, selectInput("new_CSC_Additives", "", choices =  c('', additives$Additives), multiple = T)),
                  
                  column(5, br(), tags$b("Lab Book Number")),
                  column(6, numericInput("new_CSC_LabBookNumber", "", value = NULL)),
                  
                  column(5, br(), tags$b("Page Number")),
                  column(6, numericInput("new_CSC_PageNumber", "", value = NULL)),
                  
                  column(5, br(), tags$b("Ready to Culture?")),
                  column(6, radioGroupButtons(inputId = "new_CSC_ReadyToCulture",label = "",  choices = c("Yes", "No"), selected = "No",
                                              individual = TRUE, size ="sm",status = "info", checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
           ),
           column(3, br(),br(),
                  conditionalPanel(condition = "input.new_CSC_SaveStarterCulture_Ready_Yes",# new_CSC_SaveStarterCulture_Ready_Yes
                       panel_div(class_type = "default",
                           content = tags$div(
                               tags$p("Select the Fields Recurred to be Exported to Excel"),
                               column(8,
                                      awesomeCheckboxGroup(inputId = "select_csc_fields_to_be_exported_to_excel", label = "",
                                                           choices = NULL, selected = NULL, status = "info")),
                               column(4, br(),br(),br(),br(),
                                      downloadBttn("new_CSC_SelectFieldToExportToExcel", "Export to Excel", style = "bordered", size = "xs", color = "primary")
                                      )
                               ))
                  )
           ),
           column(4)
          )
         ),
         fluidRow(
           column(2, offset = 6, actionBttn("new_CSC_ClearForm", "Clear Form", style = "fill", size = "xs", color = "primary"))
         ), hr(),
         fluidRow(br(),hr(),
                   column(8, offset = 2,
                          column(2, actionBttn("new_CSC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                          column(4, actionBttn("new_CSC_SCP_CSC_ControlForm", "MFC, SCP and CSC Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                          column(2, 
                                 tags$button(id = 'new_CSC_Exit', type = "button", class = "btn action-button",
                                             onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                             "Exit", style="color: #fff; background-color: #ff0011; border-color: #ff0011; padding:4px; font-size:80%;")
                                 #actionBttn("search_culture_initiation_Exit", "Exit", style = "jelly", size = "xs", color = "danger", block=T)
                          )
                   )
         )
)

#div(style="display:inline-block",downloadButton('downloadData', 'Download Data'), style="float:right")