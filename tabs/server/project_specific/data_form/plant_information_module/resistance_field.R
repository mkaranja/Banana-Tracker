
resistance_field <- 
  tabPanel(paste("Disease","Resistance-Field"), value = "resistance_field", hr(),
           
           fluidRow(
             column(5,
                    column(12,
                      column(4, br(), tags$p(style="text-align:right;","Field Trial Plant ID")),
                      column(4, selectInput("resistance_field_FieldTrialPlantID","", choices = NULL)),
                      column(2, br(), actionBttn("resistance_field_FieldTrialPlantIdInfo", "", style = "jelly", 
                                                 color = "primary", size="xs", icon = icon("arrow-right", lib="font-awesome")))
                    ),
                    column(12,
                      column(4, br(), tags$p(style="text-align:right;","Sample Plant ID")),
                      column(4, textInput("resistance_field_SamplePlantID", ""))
                    ),
                    column(12,
                      column(4, br(), tags$p(style="text-align:right;","Vector ID")),
                      column(4, textInput("resistance_field_VectorID1","")),
                      column(4, textInput("resistance_field_VectorID2",""))
                    ),
                    column(12,
                      column(4, br(), tags$p(style="text-align:right;","Promoter")),
                      column(4, textInput("resistance_field_Promoter1","")),
                      column(4, textInput("resistance_field_Promoter2",""))
                    ),
                    column(12,
                      column(4, br(), tags$p(style="text-align:right;","Gene")),
                      column(4, textInput("resistance_field_Gene1","")),
                      column(4, textInput("resistance_field_Gene2",""))
                    ),
                    column(12,
                      column(4, br(), tags$p(style="text-align:right;","Field Trial Identity")),
                      column(4, selectInput("resistance_field_FieldTrialIdentity","", choices = NULL))
                    ),
                    column(12,
                      column(4, br(), tags$p(style="text-align:right;","Sample ID")),
                      column(4, selectInput("resistance_field_SampleID", "", choices = NULL))
                    ),
                    column(12,
                           h4("Field Evaluation for Nematode Resistance"),
                           panel_div(class_type = "default",
                                     content = tags$div(
                                       column(12,
                                              column(7, br(), tags$p(style="text-align:right;","Date of Planting")),
                                              column(5, dateInput("resistance_field_DateOfPlanting", "", value = NULL))
                                       ),
                                       column(12,
                                              column(7, br(), tags$p(style="text-align:right;","Date of nematode inoculation")),
                                              column(5, dateInput("resistance_field_DateOfNematodeInoculation", "", value = NULL))
                                       ),
                                       column(12,
                                              column(7, br(), tags$p(style="text-align:right;","Nematode species used for inoculation")),
                                              column(5, textInput("resistance_field_DiseasepeciesUsedForInoculation", ""))
                                       ),
                                       column(12,
                                              column(7, br(), tags$p(style="text-align:right;","Nematode inoculation prior to planting")),
                                              column(5, selectInput("resistance_field_NematodeInoculationPriorToPlanting", "", choices = c("","Yes","No")))
                                       )
                                     ))
                           
                    ),
                    column(8,
                           h4("Date of data collection 1 (4 MAP):"),
                           dateInput("resistance_field_DateOfDataCollection1","", width = "100%"),
                           column(12,
                                  column(6, br(), tags$p(style="text-align:right;","Root Necrosis")),
                                  column(6, selectInput("resistance_field_RootNecrosis1", "", choices = c("","Yes","No"), width = "100%"))
                           ),
                           column(12,
                                  column(6, br(), tags$p(style="text-align:right;","Disease court")),
                                  column(6, numericInput("resistance_field_DiseaseCourt1", "", value = NULL, width = "100%"))
                           )
                           ),
                    column(4),
                    column(8,
                           h4("Date of data collection 2 (7 MAP):"),
                           dateInput("resistance_field_DateOfDataCollection2","", width = "100%"),
                           column(12,
                                  column(6, br(), tags$p(style="text-align:right;","Root Necrosis")),
                                  column(6, selectInput("resistance_field_RootNecrosis2", "", choices = c("","Yes","No"), width = "100%"))
                           ),
                           column(12,
                                  column(6, br(), tags$p(style="text-align:right;","Disease court")),
                                  column(6, numericInput("resistance_field_DiseaseCourt2", "", value = NULL, width = "100%"))
                           )
                    ),
                    column(4)
             ),
             column(7,
                    column(2, offset = 4, actionBttn("resistance_field_LoadAndUpdateData", "Load and Update Data", style = "fill", size = "xs", color = "primary", block = T)),
                    column(12, br(),
                           panel_div(class_type = "default",
                                content = tags$div(
                                  column(12,
                                       column(6, selectInput("resistance_field_LoadAndUpdateData_FieldTrialPlantID","Field Trial Plant ID", choices = NULL, width="100%")),
                                       column(2, br(), actionBttn("resistance_field_LoadAndUpdateData_FieldTrialPlantIdInfo", "", 
                                                                  icon = icon("arrow-right", lib="font-awesome"), style = "jelly", size="xs", color = "primary"))
                                       ),
                                  column(12,
                                       column(6, selectInput("resistance_field_LoadAndUpdateData_SampleID","Sample ID", choices = NULL, width = "100%")),
                                       column(2, br(), actionBttn("resistance_field_LoadAndUpdateData_GetData", "Get Data", style = "jelly", size="xs", color = "primary", block=T))
                                       ),
                                       column(3, offset = 4, br(), actionBttn("resistance_field_LoadAndUpdateData_Update", "Update", style = "jelly", size="xs", color = "primary"))
                                     )
                           )
                    ),
                    column(12,
                           column(6,
                                   
                                   dateInput("resistance_field_DateOfDataCollection3",h4("Date of data collection 3 (10 MAP):"), width = "100%"),
                                   selectInput("resistance_field_RootNecrosis3", tags$h5("Root Necrosis"), choices = c("","Yes","No")),
                                   numericInput("resistance_field_DiseaseCourt3", tags$h5("Disease court"), value = NULL),
                                   
                                   dateInput("resistance_field_DateOfDataCollection4", tags$h4("Date of data collection 4 (At flowering):"), width = "100%"),
                                   selectInput("resistance_field_RootNecrosis4", tags$h5("Root Necrosis"), choices = c("","Yes","No")),
                                   textInput("resistance_field_DiseaseCourt4", tags$h5("Disease court")),
                                   
                                   dateInput("resistance_field_DateOfDataCollection6", tags$h4("Date of data collection 6 (At harvest):"), width = "100%"),
                                   selectInput("resistance_field_RootNecrosis6", tags$h5("Root Necrosis"), choices = c("","Yes","No")),
                                   selectInput("resistance_field_PercentageRootNecrosis6", tags$h5("Percentage root necrosis (%)"), choices = NULL),
                                   selectInput("resistance_field_TotalRootFreshWeight", tags$h5("Total root fresh weight (kg)"), choices = NULL),
                                   textInput("resistance_field_DiseaseCourt6", tags$h5("Disease court")),
                                   selectInput("resistance_field_PlantToppling", tags$h5("Plant toppling"), choices = c("","Yes","No")),
                                   selectInput("resistance_field_PlantBreaking", tags$h5("Plant breaking"), choices = c("","Yes","No")),
                                   numericInput("resistance_field_NumberOfStandingYieldingPlants", tags$h5("Number of standing/ yielding plants"), value = NULL),
                                   numericInput("resistance_field_ResistancePercentToDisease", tags$h5("Resistance (%) to Disease"), value = NULL)
                                  ),
                           column(6,
                                  tags$h4(style="text-align:center;","Agronomic Data"),
                                  panel_div(class_type = "default",
                                            content = tags$div(
                                              
                                              fluidRow(
                                                column(7, br(), tags$p(style="text-align:right;","Plant Height at flowering (cm)")),
                                                column(5, numericInput("resistance_glasshouse_PlantHeightAtFlowering","", value=NULL))
                                                ),
                                                fluidRow(
                                                  column(7, br(), tags$p(style="text-align:right;","Number of functional leaves at flowering")),
                                                  column(5, numericInput("resistance_glasshouse_NumberOfFunctionalLeavesAtFlowering","", value = NULL))
                                                ),
                                                fluidRow(
                                                  column(7, br(), tags$p(style="text-align:right;","Total leaf area at flowering")),
                                                  column(5, numericInput("resistance_glasshouse_TotalLeaftAreaAtFlowering","", value=NULL))
                                                ),
                                              fluidRow(
                                                column(7, br(), tags$p(style="text-align:right;","Days to flowering")),
                                                column(5, numericInput("resistance_glasshouse_DaysToFlowering","", value = NULL))
                                              ),
                                              fluidRow(
                                                column(7, br(), tags$p(style="text-align:right;","Bunch weight (kg)")),
                                                column(5, numericInput("resistance_glasshouse_BunchWeight","", value = NULL))
                                              ),
                                              fluidRow(
                                                column(7, br(), tags$p(style="text-align:right;","Total yield")),
                                                column(5, numericInput("resistance_glasshouse_TotalYield","", value = NULL))
                                              )
                                              )), br(),
                                            fluidRow(
                                              column(3, offset=4, actionBttn("resistance_glasshouse_data_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                                              column(3, actionBttn("resistance_glasshouse_data_Save", "Save", style = "jelly", size = "xs", color = "primary", block=T))
                                            )
                                  )
                                  )
                           )
             ), hr(),
        
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("resistance_field_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("resistance_field_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('resistance_field_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
                    )
             )
         )