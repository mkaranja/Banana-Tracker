new_PTC <- 
  tabPanel("New Plant Tissue Culture", value = "new_PTC", hr(),
           
       fluidRow(
         div(id = "new_PTC_form",
         column(5,
                column(2, br(), h4("Source")),
                column(8),
                    radioGroupButtons("new_PTC_source","", choices = c("TPC","UPC", "Explant", "New"), individual = TRUE, selected = NULL, width = "100%",
                                checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"), no = tags$i(class = "fa fa-circle-o",  style = "color: steelblue"))),
                    shinyBS::bsTooltip("new_PTC_source", "Select the source type", "top", options = list(container = "body")), br(),
              column(12,
              column(3, selectInput("new_PTC_TPCID", "TPC ID", choices = c(''), width = "100%")),
              column(3, selectInput("new_PTC_UPCID", "UPC ID", choices = c(''), width = "100%")),
              column(3, selectInput("new_PTC_ExplantID", "Explant ID", choices = c(''), width = "100%")),
              column(1, br(), 
                     actionBttn("new_PTC_ID_LoadData", "", icon = icon("arrow-circle-right", lib='font-awesome'), size = "sm"),
                     shinyBS::bsTooltip("new_PTC_ID_LoadData", "Select the source ID and press the button to load the data",
                                      "right", options = list(container = "body")))
              ),
              
               column(2, br(), h5("Vector ID")),
               column(4, textInput("new_PTC_VectorID1", "")),
               column(4, textInput("new_PTC_VectorID2", "")),
               column(12,
               textInput("new_PTC_VectorID1_PromoterGene",tags$p(style="text-align:center","Vector ID 1 Promoter-Gene"), width = "90%"), 
               textInput("new_PTC_VectorID2_PromoterGene",tags$p(style="text-align:center","Vector ID 2 Promoter-Gene"), width = "90%")
               ),
               column(3, br(), h5("Virus Indexed")),
               column(9, textInput("new_PTC_VirusIndexed", "")),
                     
              column(6, numericInput("new_PTC_NumberOfPlants", "Number of plants", value = NULL, width = "90%")),
              column(4, selectInput("new_PTC_Year", "Year", choices = c("", 2001:(lubridate::year(Sys.Date()))))),
              column(12, 
                 actionBttn("new_PTC_GenerateIdentity", "Generate Identity", style = "fill", size = "xs", color = "primary"),
                 shinyBS::bsTooltip("new_PTC_GenerateIdentity", "Generate the ID",  "right", options = list(container = "body"))
                    ), br(),
              column(12, 
                     textInput("new_PTC_PTCIdentity","PTC Identity"), 
                     textInput("new_PTC_IdentityType","Identity Type")
                     )
              ),
          column(7,
                 conditionalPanel(
                   condition = "input.new_PTC_source=='New'",
                        panel_div(class_type = "default",
                            content = tags$div(
                                column(12,
                                       column(4, selectInput("new_PTC_Cultivar", labelMandatory("Cultivar"), choices = NULL))
                                       ),
                                column(12,
                                       column(4, selectInput("new_PTC_CultivarConfirmed", labelMandatory("Cultivar Confirmed"), choices = c('','Yes','No'))),
                                       column(4, textAreaInput("new_PTC_Comments", "Comments"))
                                ),
                                column(12,
                                       column(4, selectInput("new_PTC_Source", labelMandatory("Source"), choices = NULL))
                                ),
                                column(12,
                                       column(4, selectInput("new_PTC_VirusIndexed", labelMandatory("Virus Indexed"), choices = c('','Yes','No'))),
                                       conditionalPanel(
                                         condition = "input.new_PTC_VirusIndexed=='Yes'",
                                         column(4, dateInput("new_PTC_VirusIndexedDate","Virus Indexed Date", value = NULL)),
                                         column(4, selectInput("new_PTC_VirusIndexedBy", "Virus Indexed By", choices = NULL))
                                       )
                                ),
                                column(12,
                                    column(7,   
                                       column(4, br(), tags$p(style="text-align:right", "Permit Type")),
                                       column(4, selectInput("new_PTC_PermitType1A", "", choices = NULL)),
                                       column(4, selectInput("new_PTC_PermitType1B", "", choices = NULL)),
                                       
                                       column(4, br(), tags$p(style="text-align:right", "Permit Number")),
                                       column(4, numericInput("new_PTC_PermitNumber1A", "", value = NULL)),
                                       column(4, numericInput("new_PTC_PermitNumber1B", "", value = NULL)),
                                       
                                       column(4, br(), tags$p(style="text-align:right", "Permit Type")),
                                       column(4, selectInput("new_PTC_PermitType2A", "", choices = NULL)),
                                       column(4, selectInput("new_PTC_PermitType2B", "", choices = NULL)),
                                       
                                       column(4, br(), tags$p(style="text-align:right", "Permit Number")),
                                       column(4, numericInput("new_PTC_PermitNumber2A", "", value = NULL)),
                                       column(4, numericInput("new_PTC_PermitNumber2B", "", value = NULL)),
                                       
                                       column(4, br(), tags$p(style="text-align:right", "Permit Type")),
                                       column(4, selectInput("new_PTC_PermitType3A", "", choices = NULL)),
                                       column(4, selectInput("new_PTC_PermitType3B", "", choices = NULL)),
                                       
                                       column(4, br(), tags$p(style="text-align:right", "Permit Number")),
                                       column(4, numericInput("new_PTC_PermitNumber3A", "", value = NULL)),
                                       column(4, numericInput("new_PTC_PermitNumber3B", "", value = NULL))
                                    ),
                                    column(5, br(), br(), br(),br(), br(),
                                           panel_div(class_type = "default",
                                                     content = tags$div(
                                                       column(12,
                                                         column(8, offset=2, actionBttn("new_PTC_Refresh", "Refresh", style = "fill", color = "primary", size = "xs", block = T)),br(), br(),
                                                         column(4)
                                                       ),
                                                       column(12,
                                                         column(6, br(), tags$p(style="text-align:right", "PTC Identity")),
                                                         column(6, selectInput("new_PTC_PTCIdentity","", choices = NULL))
                                                         ),
                                                       column(4, offset=2, actionBttn("new_PTC_LoadData", "Load Data", style = "fill", color = "primary", size = "xs", block = T)),
                                                       column(4, actionBttn("new_PTC_Update", "Update", style = "fill", color = "primary", size = "xs", block = T))
                                                     ))
                                    )
                                )
                                       
                            ))
                        )),
         column(12,
                column(4, 
                       dateInput("new_PTC_TPL_UPL_InitialCultureDate", labelMandatory("TPL/UPL Initial Culture Date"), value=NULL),
                       selectInput("new_PTC_Media", "Media", choices = NULL),
                       prettyCheckboxGroup("new_PTC_Additives", "Additives",choices = NULL,
                         icon = icon("check-square-o"), status = "info", outline = TRUE, animation = "jelly"),
                       numericInput("new_PTC_LabBookNumber", labelMandatory("Lab Book NUmber"), value = NULL),
                       numericInput("new_PTC_PageNumber", labelMandatory("Page NUmber"), value = NULL),
                       radioGroupButtons("new_PTC_ReadyToCulture","Ready to Culture?", choices = c("Yes","No"),
                                         individual = TRUE, selected = NULL),
                       actionBttn("new_PTC_ClearTheForm", "Clear the Form", style = "fill", color = "primary", size = "xs")
                       ),
                column(4,
                       column(8,
                            prettyCheckboxGroup("new_PTC_SelectTheFieldsRequiredToBeExportedToExcel", "Select the Fields Required to be Exported to Excel",choices = NULL,
                                           icon = icon("check-square-o"), status = "info", outline = TRUE, animation = "jelly")),
                       column(3, br(), br(), br(), br(),br(), br(), actionBttn("new_PTC_ExportToExcel", "Export To Excel", style = "fill", color = "primary", size = "xs", block = T))
                       ),
                column(4,
                       numericInput("new_PTC_NumberOfCultures", labelMandatory("Number of Cultures"), value = NULL),
                       dateInput("new_PTC_DateOfCulture", labelMandatory("Date of Culture")),
                       selectInput("new_PTC_CulturedBy", labelMandatory("Cultured By"), choices = NULL),
                       textAreaInput("new_PTC_Comments", "Comments"), br(),
                       column(6, offset = 3,
                              actionBttn("new_PTC_SaveStarterCultureAndSubCulture", "Save Starter Culture and Sub Culture", style = "fill", color = "primary", size = "xs")
                       )
                       )
                )
      )), hr(),
      fluidRow(
        column(6),
        column(6,
               column(4),
               column(3, actionBttn("new_PTC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
               column(3, actionBttn("new_PTC_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
               column(2, actionBttn('new_PTC_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
        )
      )
           
  )