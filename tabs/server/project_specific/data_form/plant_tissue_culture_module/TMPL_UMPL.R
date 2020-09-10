TMPL_UMPL <- 
  tabPanel("TMPL/UMPL", value = "TMPL_UMPL", hr(),
           fluidRow(
             column(6, 
                    actionBttn("TMPL_UMPL_Refresh", "Refresh", style = "fill", size = "sm", color = "primary"), br(),
                    column(12,
                           column(5, br(), p("Plant Tissue Culture Identity")),
                           column(5, selectInput("TMPL_UMPL_PlantTissueCultureIdentity", "", choices = c(""))),
                           column(2, br(), actionBttn("TMPL_UMPL_LoadData", ""))
                           ),
                    column(12,
                           column(5, br(), p("TMPL/UMPL Deployment Identity")),
                           column(5, selectInput("TMPL_UMPL_DeploymentIdentity", "", choices = c(""))),
                           column(2, br(), actionBttn("TMPL_UMPL_DeploymentViewData", "View Data"))
                    ),
                    column(12,
                           column(5, br(), p("Deployment Location")),
                           column(5, selectInput("TMPL_UMPL_DeploymentLocation", "", choices = c("")))
                    ),
                    column(12,
                           column(5, br(), p("Deployment Date")),
                           column(5, dateRangeInput("TMPL_UMPL_Deployment_Date", ""))
                    ),br(),
                    div(style="text-align:center", actionBttn("TMPL_UMPL_LoadData", "Save")),
                    column(12,
                           panel_div(class_type = "default",
                                     content = tags$div(
                                       h5("Redeployment to TGPL/UGPL or TFPL/UFPL"),
                                       column(6, 
                                              awesomeCheckbox("TMPL_UMPL_Redeployment_TGPL_UGPL", "TGPL/UGPL",value = TRUE,status = "info"),
                                              actionBttn("TMPL_UMPL_DeploymentTo_TGPL_UGPL", "Deploy to TGPL/UGPL")
                                              ),
                                       column(6, awesomeCheckbox("TMPL_UMPL_Redeployment_TFPL_UFPL", "TFPL/UFPL",value = TRUE,status = "info"),
                                              actionBttn("TMPL_UMPL_DeploymentTo_TFPL_UFPL", "Deploy to TFPL/UFPL")
                                              ),
                                     )
                           )
                    ),
                    column(12,
                           panel_div(class_type = "default",
                                     content = tags$div(
                                     
                           ))
                    )
             ),
             column(6, actionBttn("TMPL_UMPL_SaveDeploymentIdentities", "Save Deploment Identities", style = "fill", size = "xs", color = "primary")
             )
           ),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("TMPL_UMPL_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("TMPL_UMPL_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('TMPL_UMPL_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
           
  )
