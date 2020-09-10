panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

new_transformation <- 
  tabPanel("New Transformation", value = "new_transformation",
           hr(), useShinyalert(),
      div(id = "new_transformation_Form",
           fluidRow(
             column(8,
                  column(12,
                       column(4, selectInput("new_transformation_TransformationType", labelMandatory("Transformation Type"), choices = c(""), multiple = F)),
                       column(4, offset = 4, selectInput("new_transformation_VirusIndexed", "Virus Indexed", choices = c("","Yes","No"), multiple = F))
                  ),
                  column(12,
                         column(4, selectInput("new_transformation_AgrobacteriumStrains", labelMandatory("Agrobacterium Strains"), choices = c(""), multiple = F))
                  ),
                  column(12,
                         column(4, selectInput("new_transformation_ParentIDType", labelMandatory("Parent ID Type"), choices = c("Cell Suspension Culture (CSC)", "Explant"), multiple = F)),
                         column(1, br(), tags$b("->>")),
                         column(4, selectInput("new_transformation_ParentID", "", choices = c(""), multiple = F)), # CSCIdentity
                         column(2, br(), actionBttn("new_transformation_GetData", "Get Data", style = "fill", size = "xs", color = "primary", block = T))
                  ),
                  column(12, hr(),
                         column(2, offset = 1, actionBttn("new_transformation_UseVectors", "Use Vectors", style = "fill", size = "xs", color = "primary", block = T)), br()
                         ),
                  column(12,
                         column(2, br(), tags$b("Vector 1")),
                         column(3, selectInput("new_transformation_PlantSelection_Vector1", "", choices = c(""))),
                         column(3, textInput("new_transformation_PlantSelection_Vector1", "Plant Selection")),
                         column(4, textInput("new_transformation_PromoterGene_Vector1", "Promoter-Gene"))
                         ),
                  column(12,
                         column(2, br(), tags$b("Vector 2")),
                         column(3, selectInput("new_transformation_PlantSelection_Vector2", "", choices = c(""))),
                         column(3, textInput("new_transformation_PlantSelection_Vector2", "")),
                         column(4, textInput("new_transformation_PromoterGene_Vector2", ""))
                  ),
                  column(12,
                         column(2, br(), tags$b("Transformation ID")),
                         column(3, textInput("new_transformation_TransformationID", "")),
                         column(3, br(), actionBttn("new_transformation_GenerateIdentity", "Generate Identity", style = "fill", size = "xs", color = "primary")),
                         column(2, airYearpickerInput("new_transformation_Year", "Year", maxDate=Sys.Date()))
                         )
            ),
            column(4,
                   conditionalPanel(# new_transformation_ReadyToCulture
                     condition = "input.new_transformation_ReadyToCulture == 'Yes'",
                       numericInput("new_transformation_NumberOfCultures", labelMandatory("Number of Cultures"), value = NULL, min=0),
                       dateInput("new_transformation_DateOfCultures", "Date of Cultures"),
                       selectInput("new_transformation_CulturedBy","Cultured By", choices = c("")),
                       textInput("new_transformation_Comments", "Comments")
                     )
                   )
            ),
          fluidRow(
            column(8,
              column(12,
                     column(2, br(), tags$b("Identity Type")),
                     column(3,textInput("new_transformation_IdentityType", ""))
              ),
              column(12,
                     column(2, br(),tags$b("Cultivar")),
                     column(3,textInput("new_transformation_Cultivar", ""))
                )
              ),
            column(2, br(),br(),br(),
                   actionBttn("new_transformation_SaveStarterCultureAndSubCulture", "Save Starter Culture and Sub Culture", 
                              style = "fill", size = "xs", color = "primary")
                   )
            ),
          
           fluidRow(
             column(11,
             column(2, selectInput("new_transformation_PermitType1", "Permit Type", choices = c(""), multiple = F)),
             column(2, selectInput("new_transformation_PermitType2", "Permit Type", choices = c(""), multiple = F)),
             column(2, selectInput("new_transformation_PermitType3", "Permit Type", choices = c(""), multiple = F)),
             column(2, selectInput("new_transformation_PermitType4", "Permit Type", choices = c(""), multiple = F)),
             column(2, selectInput("new_transformation_PermitType5", "Permit Type", choices = c(""), multiple = F)),
             column(2, selectInput("new_transformation_PermitType6", "Permit Type", choices = c(""), multiple = F))
             )
           ),
           fluidRow(
             column(11,
             column(2, numericInput("new_transformation_PermitNumber1", labelMandatory("Permit Number"), value = NULL, min = 0)),
             column(2, numericInput("new_transformation_PermitNumber2", "Permit Number", value = NULL, min = 0)),
             column(2, numericInput("new_transformation_PermitNumber3", "Permit Number", value = NULL, min = 0)),
             column(2, numericInput("new_transformation_PermitNumber4", "Permit Number", value = NULL, min = 0)),
             column(2, numericInput("new_transformation_PermitNumber5", "Permit Number", value = NULL, min = 0)),
             column(2, numericInput("new_transformation_PermitNumber6", "Permit Number", value = NULL, min = 0))
             )
           ),
           fluidRow(
             column(12,
                 column(3,
                        dateInput("new_transformation_TPC_UPC_InitialCultureDate", labelMandatory("TPC/ UPC/ Initial Culture Date"), value=NULL),
                        selectInput("new_transformation_Media", labelMandatory("Media"), choices = c(""), multiple = F),
                        selectInput("new_transformation_Additives", "Additives", choices = c(""), multiple = T),
                        numericInput("new_transformation_LabBookNumber", labelMandatory("Lab Book Number"), value = NULL, min = 0),
                        numericInput("new_transformation_PageNumber", labelMandatory("Page Number"), value = NULL, min = 0),
                        radioGroupButtons(inputId = "new_transformation_ReadyToCulture",label = "Ready to Culture?",
                                                    choices = c("Yes", "No"), selected = "No", individual = TRUE, size ="xs",status = "info",
                                                    checkIcon = list(yes = icon("ok", lib = "glyphicon")))
                  ),
                 column(5, br(),br(),
                        conditionalPanel(condition = "input.new_transformation_SaveStarterCultureAndSubCulture",
                             panel_div(class_type = "default",
                                  content = tags$div(
                                         p("Select the Fields Required to be Exported to Excel"),
                                         column(9,uiOutput("select_fields_to_be_exported_to_excel_Output")),
                                         column(3,  br(),br(), downloadBttn("new_transformation_SelectFieldToExportToExcel", "Export to Excel", style = "bordered", size = "s", color = "primary"))
                                  ))     
                        )
                 ),
                 column(4, align="center", br(),br(),br(),
                        panel_div(class_type = "default",
                                  content = tags$div(
                                    actionBttn("new_transformation_Refresh", "Refresh", style = "fill", size = "xs", color = "primary"), br(), br(),
                                    selectInput("new_transformation_ExplantIdentity", "Explant Identity", choices = NULL, multiple = F), br(),
                                    actionBttn("new_transformation_LoadData", "Load Data", style = "fill", size = "xs", color = "primary"), br(), br(),
                                    actionBttn("new_transformation_Update", "Update", style = "fill", size = "xs", color = "primary")
                                  ))
                 )
                )
           ),
           fluidRow(
             verbatimTextOutput("tbl"),
             column(2, offset = 5,
                    actionBttn("new_transformation_ClearForm", "Clear Form", style = "fill", size = "xs", color = "primary")
             )
             
           ), hr(),
          fluidRow(
            column(6),
            column(6,
                   column(4),
                   column(3, actionBttn("new_transformation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                   column(3, actionBttn("new_transformation_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                   column(2, actionBttn('new_transformation_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
            )
          )
      )
           
                                   
)

