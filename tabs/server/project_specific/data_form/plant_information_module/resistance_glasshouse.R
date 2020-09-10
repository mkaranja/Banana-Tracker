
resistance_glasshouse <- 
  tabPanel(paste("Disease","Resistance-Glasshouse"), value = "resistance_glasshouse", hr(),
           
           fluidRow(
             column(6,
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Plant Tissue Culture Identity")),
                      column(4, selectInput("resistance_glasshouse_PlantTissueCultureIdentity","", choices = NULL)),
                      column(2, br(), actionBttn("resistance_glasshouse_PlantTissueCultureIdentityBttn", "", style = "jelly", 
                                                 color = "primary", size="xs", icon = icon("arrow-right", lib="font-awesome")))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","TGPL/UGPL Deployment Identity")),
                      column(4, selectInput("resistance_glasshouse_TGPLUGPLDeploymentIdentity","", choices = NULL)),
                      column(2, br(), actionBttn("resistance_glasshouse_TGPLUGPLDeploymentIdentityBttn", "", style = "jelly", 
                                                 color = "primary", size="xs", icon = icon("arrow-right", lib="font-awesome")))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Vector ID")),
                      column(4, textInput("resistance_glasshouse_VectorID1","")),
                      column(4, textInput("resistance_glasshouse_VectorID2",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Promoter")),
                      column(4, textInput("resistance_glasshouse_Promoter1","")),
                      column(4, textInput("resistance_glasshouse_Promoter2",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Gene")),
                      column(4, textInput("resistance_glasshouse_Gene1","")),
                      column(4, textInput("resistance_glasshouse_Gene2",""))
                    ), br(),
                    fluidRow(
                      column(12,
                           column(4, offset = 4,actionBttn("resistance_glasshouse_GenerateSampleID", "Generate Sample ID", style = "jelly", size = "xs", color = "primary", block = T)),
                      ),
                      column(4, br(), tags$p(style="text-align:right;","Glass House Trial ID")),
                      column(4, selectInput("resistance_glasshouse_GlassHouseTrialID","", choices = NULL))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Sample ID")),
                      column(4, selectInput("resistance_glasshouse_SampleID", "", choices = NULL))
                    ),
             ),
             column(5,
                    column(6, offset = 3, actionBttn("resistance_glasshouse_LoadAndUpdateData", "Load and Update Data", style = "jelly", size = "xs", color = "primary")),
                    column(12, br(),
                           panel_div(class_type = "default",
                                     content = tags$div(
                                       column(8, selectInput("resistance_glasshouse_LoadAndUpdateData_FieldTrialPlantID","Field Trial Plant ID", choices = NULL)),
                                       column(4, br(), actionBttn("resistance_glasshouse_LoadAndUpdateData_FieldTrialPlantIdInfo", "", icon = icon("arrow-right", lib="font-awesome"), style = "jelly", size="xs", color = "primary")),
                                       
                                       column(8, selectInput("resistance_glasshouse_LoadAndUpdateData_SampleID","Sample ID", choices = NULL)),
                                       column(4, br(), actionBttn("resistance_glasshouse_LoadAndUpdateData_GetData", "Get Data", style = "jelly", size="xs", color = "primary")),
                                       
                                       column(3, offset = 4, br(), actionBttn("resistance_glasshouse_LoadAndUpdateData_Update", "Update", style = "jelly", size="xs", color = "primary"))
                                     )
                           )
                    )
             )
             
           ),hr(),
           fluidRow(
             column(6,h4(paste("Glasshouse Evaluation for","Disease","Resistance")),
                    panel_div(class_type = "default",
                         content = tags$div(
                           
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;", paste("Date of", "Disease","inoculation"))),
                                  column(5, dateInput("resistance_glasshouse_DateOfInoculation","", value = NULL))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Plant Size at the Time of Inoculation")),
                                  column(5, textInput("resistance_glasshouse_PlantSizeAtTheTimeOfInoculation","")),
                                  column(1, br(), tags$p(style="text-align:left;","(cm)"))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;", paste("Disease","species used for inoculation"))),
                                  column(5, numericInput("resistance_glasshouse_species used for inoculation","", value = NULL))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Date of data collection")),
                                  column(5, dateInput("resistance_glasshouse_DateOfDataCollection",""))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Total root fresh weight")),
                                  column(5, dateInput("resistance_glasshouse_TotalRootFreshWeight","")),
                                  column(1, br(), tags$p(style="text-align:left;","kg"))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Root Necrosis")),
                                  column(5, textInput("resistance_glasshouse_RootNecrosis",""))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Percentage of root necrosis")),
                                  column(5, dateInput("resistance_glasshouse_DateOfDataCollection",""))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;", paste("Disease","court"))),
                                  column(5, dateInput("resistance_glasshouse_court",""))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;",paste("Resistance (%) to","Disease"))),
                                  column(5, dateInput("resistance_glasshouse_DateOfDataCollection",""))
                                )
                         ))
                    
             ),
             column(5,
                    h4("Agronomic Data"),
                    panel_div(class_type = "default",
                              content = tags$div(
                                
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Plant Height")),
                                  column(5, dateInput("resistance_glasshouse_PlantHeight","")),
                                  column(1, br(), tags$p(style="text-align:left;","(cm)"))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Number of functional leaves")),
                                  column(5, dateInput("resistance_glasshouse_NumberOfFunctionalLeaves",""))
                                ),
                                fluidRow(
                                  column(5, br(), tags$p(style="text-align:right;","Total leaf area")),
                                  column(5, dateInput("resistance_glasshouse_TotalLeaftArea",""))
                                )
                              )), br(),
                      fluidRow(
                        column(2, offset=4, actionBttn("resistance_glasshouse_data_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                        column(2, actionBttn("resistance_glasshouse_data_Save", "Save", style = "jelly", size = "xs", color = "primary", block=T))
                      )
                    )
           ),hr(),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("resistance_glasshouse_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("resistance_glasshouse_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('resistance_glasshouse_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
                    )
             )
           
           
  )