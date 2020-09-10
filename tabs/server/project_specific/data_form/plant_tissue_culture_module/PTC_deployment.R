PTC_deployment <- 
  tabPanel("Plant Tissue Culture Deployment", value = "PTC_deployment", hr(),
           fluidRow(
             column(12,
               actionBttn("PTC_deployment_SelectPlantTissueCulture", "Select Plant Tissue Culture", style = "fill", size = "sm", color = "primary"),
                 shinyBS::bsTooltip("PTC_deployment_SelectPlantTissueCulture", "Press this button first","right", options = list(container = "body"))
               ), br(),br(),
             column(8,
                    fluidRow(
                       column(3, selectInput("PTC_deployment_TPLIdentity","TPL Identity", choices = NULL, width = "100%")),
                       column(2, br(), actionBttn("PTC_deployment_TPLGetVectorData", "Get Vector Data", style = "fill", size = "xs", color = "primary")),
                       column(3, offset = 2, selectInput("PTC_deployment_UPLIdentity","UPL Identity", choices = NULL, width = "100%")),
                       column(1, br(), actionBttn("PTC_deployment_UPLGetVectorData", "", icon = icon("arrow-right", lib="font-awesome"), style = "fill", size = "sm", color = "primary"))
                      ),
                    column(7,
                        fluidRow(
                           column(3, br(), h5("Vector ID")),
                           column(4, textInput("PTC_deployment_Vector1", "Vector 1", width = "100%")),
                           column(4, textInput("PTC_deployment_Vector2", "Vector 2", width = "100%"))
                        ),
                        fluidRow(      
                           column(3, br(), h5("Promoter")),
                           column(4, textInput("PTC_deployment_Promoter1", "", width = "100%")),
                           column(4, textInput("PTC_deployment_Promoter2", "", width = "100%"))
                        ),
                        fluidRow(       
                           column(3, br(), h5("Gene")),
                           column(4, textInput("PTC_deployment_Gene1", "", width = "100%")),
                           column(4, textInput("PTC_deployment_Gene2", "", width = "100%"))
                        ),
                        fluidRow(
                           column(4, br(), h5("Transgene Confirmation")),
                           column(3, textInput("PTC_deployment_TransgeneConfirmation1", "", width = "100%")),
                           column(3, textInput("PTC_deployment_TransgeneConfirmation2", "", width = "100%"))
                        ),
                        fluidRow(
                          column(4, br(), h5("Molecular Data File")),
                          column(4, fileInput("PTC_deployment_MolecularDataFile",""))
                        ),
                        fluidRow(
                          column(4, br(), h5("Lab Book Number")),
                          column(3, numericInput("PTC_deployment_LabBookNumber","", value = NULL, width = "100%"))
                        ),
                        fluidRow(
                          column(4, br(), h5("Page Number")),
                          column(3, numericInput("PTC_deployment_PageNumber","", value = NULL, width = "100%"))
                        ),
                        
                        tags$h4(style="text-align:center", "TPL"),
                        fluidRow(
                          column(4, p("Deployment Type")),
                          column(3,p("Number"))
                        ),
                        fluidRow(
                          column(2, br(), h5("TMPL")),
                          column(5, numericInput("PTC_deployment_TMPL_Number","", value = NULL)),
                          column(5, br(), actionBttn("PTC_deployment_TMPL_GenerateIdentities", "Generate Identities", style = "fill", size = "xs", color = "primary"))
                        ),
                        fluidRow(
                          column(2, br(), h5("TGPL")),
                          column(5, numericInput("PTC_deployment_TGPL_Number","", value = NULL)),
                          column(5, br(), actionBttn("PTC_deployment_TGPL_GenerateIdentities", "Generate Identities", style = "fill", size = "xs", color = "primary"))
                        ),
                        fluidRow(
                          column(2, br(), h5("TFPL")),
                          column(5, numericInput("PTC_deployment_TFPL_Number","", value = NULL)),
                          column(5, br(), actionBttn("PTC_deployment_TFPL_GenerateIdentities", "Generate Identities", style = "fill", size = "xs", color = "primary"))
                        ), br(), 
                        
                        column(1, offset = 11, actionBttn("PTC_deployment_Clear", "Clear", style = "fill", size = "xs", color = "primary"))
                    ),
                    column(5,
                           tags$h4(style="text-align:center", "UPL"),
                           fluidRow(
                             column(4, p("Deployment Type")),
                             column(3,p("Number"))
                             ),
                           fluidRow(
                             column(2, br(), h5("UMPL")),
                             column(5, numericInput("PTC_deployment_UMPL_Number","", value = NULL)),
                             column(5, br(), actionBttn("PTC_deployment_UMPL_GenerateIdentities", "Generate Identities", style = "fill", size = "xs", color = "primary"))
                           ),
                           fluidRow(
                             column(2, br(), h5("UGPL")),
                             column(5, numericInput("PTC_deployment_UGPL_Number","", value = NULL)),
                             column(5, br(), actionBttn("PTC_deployment_UGPL_GenerateIdentities", "Generate Identities", style = "fill", size = "xs", color = "primary"))
                           ),
                           fluidRow(
                             column(2, br(), h5("UFPL")),
                             column(5, numericInput("PTC_deployment_UFPL_Number","", value = NULL)),
                             column(5, br(), actionBttn("PTC_deployment_UFPL_GenerateIdentities", "Generate Identities", style = "fill", size = "xs", color = "primary"))
                           )
                      )
                    
                ),
             column(4,
                    box(width = 12, height = "200px", status = "info"),
                    actionBttn("PTC_deployment_SaveDeploymentIdentities", "Save Deploment Identities", style = "fill", size = "xs", color = "primary")
                    )
        ),
        fluidRow(
          column(6),
          column(6,
                 column(4),
                 column(3, actionBttn("PTC_deployment_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                 column(3, actionBttn("PTC_deployment_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                 column(2, actionBttn('PTC_deployment_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
          )
        )
           
  )
