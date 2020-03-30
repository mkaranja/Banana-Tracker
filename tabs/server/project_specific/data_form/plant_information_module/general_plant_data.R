
general_plant_data <- 
  tabPanel("General Plant Data", value = "general_plant_data", hr(),
           
           fluidRow(
                    column(6,
                           fluidRow(
                             column(4, br(), tags$p(style="text-align:right;","Field Trial Plant ID")),
                             column(4, selectInput("general_plant_data_FieldTrialPlantID","", choices = NULL)),
                             column(2, br(), actionBttn("general_plant_data_FieldTrialPlantIdInfo", "", style = "jelly", 
                                                        color = "primary", size="sm", icon = icon("arrow-right", lib="font-awesome")))
                           ),
                           fluidRow(
                             column(4, br(), tags$p(style="text-align:right;","Old Field Trial Plant ID")),
                             column(4, selectInput("general_plant_data_OldFieldTrialPlantID","", choices = NULL))
                                  ),
                           fluidRow(
                             column(4, br(), tags$p(style="text-align:right;","Sample Plant ID")),
                             column(4, disabled(textInput("general_plant_data_SamplePlantID", "")))
                           ),
                           fluidRow(
                             column(4, br(), tags$p(style="text-align:right;","Vector ID")),
                             column(4, disabled(textInput("general_plant_data_VectorID1",""))),
                             column(4, disabled(textInput("general_plant_data_VectorID2","")))
                           ),
                           fluidRow(
                             column(4, br(), tags$p(style="text-align:right;","Promoter")),
                             column(4, disabled(textInput("general_plant_data_Promoter1",""))),
                             column(4, disabled(textInput("general_plant_data_Promoter2","")))
                           ),
                           fluidRow(
                             column(4, br(), tags$p(style="text-align:right;","Gene")),
                             column(4, disabled(textInput("general_plant_data_Gene1",""))),
                             column(4, disabled(textInput("general_plant_data_Gene2","")))
                           )
                        ),
                    column(5,
                           column(6, offset = 3, actionBttn("general_plant_data_LoadAndUpdateData", "Load and Update Data", style = "jelly", size = "sm", color = "primary")),
                           column(12, br(),
                                  panel_div(class_type = "default",
                                      content = tags$div(
                                        column(8, selectInput("general_plant_data_load_and_update_FieldTrialPlantID","Field Trial Plant ID", choices = NULL)),
                                        column(4, br(), actionBttn("general_plant_data_load_and_update_FieldTrialPlantIdInfo", "", icon = icon("arrow-right", lib="font-awesome"), style = "jelly", size="sm", color = "primary")),
                                        
                                        column(8, selectInput("general_plant_data_load_and_update_SampleID","Sample ID", choices = NULL)),
                                        column(4, br(), actionBttn("general_plant_data_load_and_update_GetData", "Get Data", style = "jelly", size="sm", color = "primary")),
                                        
                                        column(3, offset = 4, br(), actionBttn("general_plant_data_load_and_update_Update", "Update", style = "jelly", size="sm", color = "primary"))
                                      )
                                  )
                              )
                           )
                    
           ),
           fluidRow(
             column(6,
                    fluidRow(
                        column(4, br(), tags$p(style="text-align:right;","Field Trial Identity")),
                        column(4, disabled(textInput("general_plant_data_FieldTrialIdentity","")))
                        ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Crop Cycle")),
                      column(4, selectInput("general_plant_data_CropCycle","", choices = NULL))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Date of Planting")),
                      column(4, dateInput("general_plant_data_DateOfPlanting",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Phenotype")),
                      column(4, selectInput("general_plant_data_Phenotype","", choices = NULL))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Date of Bunch Emergence")),
                      column(4, dateInput("general_plant_data_DateOfBunchEmergence",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Date of Finish Bunch Harvest")),
                      column(4, dateInput("general_plant_data_DateOfFinishBunchHarvest",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Bunch Weight")),
                      column(4, dateInput("general_plant_data_BunchWeight","")),
                      column(2, br(), tags$b("Kg"))
                    )
                  ),
             column(5, h4("Sample Selection"),
                    panel_div(class_type = "default",
                              content = tags$div(
                                                 fluidRow(
                                                   column(5, br(), tags$p(style="text-align:right;","Type of Sample")),
                                                   column(5, selectInput("general_plant_data_TypeOfSample","", choices = NULL)),
                                                   column(2, br(), actionBttn("general_plant_data_SampleInfo", "", style = "jelly", 
                                                                              color = "primary", size="sm", icon = icon("arrow-right", lib="font-awesome")))
                                                 ),
                                                 fluidRow(
                                                   column(5, br(), tags$p(style="text-align:right;","Stage of Fruit Sampling")),
                                                   column(5, selectInput("general_plant_data_StageOfFruitSampling","", choices = NULL))
                                                 ),
                                                 fluidRow(
                                                   column(5, br(), tags$p(style="text-align:right;","Date of Sampling")),
                                                   column(5, dateInput("general_plant_data_DateOfSampling",""))
                                                 ),
                                                 fluidRow(
                                                   column(6, offset=2, actionBttn("general_plant_data_GenerateSampleID", "Generate Sample ID", style = "jelly", 
                                                                     color = "primary", size="sm"))
                                                          ),
                                                 fluidRow(
                                                   column(5, br(), tags$p(style="text-align:right;","Sample ID")),
                                                   column(5, disabled(textInput("general_plant_data_SampleID","")))
                                                 )
                              )
                    )
                    )
           )
           
    )