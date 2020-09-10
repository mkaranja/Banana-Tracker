
proximate_analysis <- 
  tabPanel("Proximate Analysis", value = "proximate_analysis", hr(),
           
           fluidRow(
             column(6,
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Field Trial Plant ID")),
                      column(4, selectInput("proximate_analysis_FieldTrialPlantID","", choices = NULL)),
                      column(2, br(), actionBttn("proximate_analysis_FieldTrialPlantIdInfo", "", style = "jelly", 
                                                 color = "primary", size="xs", icon = icon("arrow-right", lib="font-awesome")))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Sample Plant ID")),
                      column(4, textInput("proximate_analysis_SamplePlantID", ""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Vector ID")),
                      column(4, textInput("proximate_analysis_VectorID1","")),
                      column(4, textInput("proximate_analysis_VectorID2",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Promoter")),
                      column(4, textInput("proximate_analysis_Promoter1","")),
                      column(4, textInput("proximate_analysis_Promoter2",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Gene")),
                      column(4, textInput("proximate_analysis_Gene1","")),
                      column(4, textInput("proximate_analysis_Gene2",""))
                    ),
                   fluidRow(
                     column(4, br(), tags$p(style="text-align:right;","Field Trial Identity")),
                     column(4, selectInput("proximate_analysis_FieldTrialIdentity","", choices = NULL))
                   ),
                   fluidRow(
                     column(4, br(), tags$p(style="text-align:right;","Sample ID")),
                     column(4, selectInput("proximate_analysis_SampleID", "", choices = NULL))
                   ),
             ),
             column(5,
                    column(6, offset = 3, actionBttn("proximate_analysis_LoadAndUpdateData", "Load and Update Data", style = "jelly", size = "xs", color = "primary")),
                    column(12, br(),
                           panel_div(class_type = "default",
                                     content = tags$div(
                                       column(8, selectInput("proximate_analysis_LoadAndUpdateData_FieldTrialPlantID","Field Trial Plant ID", choices = NULL)),
                                       column(4, br(), actionBttn("proximate_analysis_LoadAndUpdateData_FieldTrialPlantIdInfo", "", icon = icon("arrow-right", lib="font-awesome"), style = "jelly", size="xs", color = "primary")),
                                       
                                       column(8, selectInput("proximate_analysis_LoadAndUpdateData_SampleID","Sample ID", choices = NULL)),
                                       column(4, br(), actionBttn("proximate_analysis_LoadAndUpdateData_GetData", "Get Data", style = "jelly", size="xs", color = "primary")),
                                       
                                       column(3, offset = 4, br(), actionBttn("proximate_analysis_LoadAndUpdateData_Update", "Update", style = "jelly", size="xs", color = "primary"))
                                     )
                           )
                    )
             )
             
           ),br(),
           fluidRow(
             column(6,
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Moisture")),
                      column(4, numericInput("proximate_analysis_Moisture","", value = NULL)),
                      column(4, br(), tags$p(style="text-align:left;","gm/100gm Wet Weight"))
                    
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Ash")),
                      column(4, numericInput("proximate_analysis_Ash","", value = NULL)),
                      column(4, br(), tags$p(style="text-align:left;","gm/100gm dry Weight"))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Protein")),
                      column(4, numericInput("proximate_analysis_Protein","", value = NULL)),
                      column(4, br(), tags$p(style="text-align:left;","gm/100gm dry Weight"))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Sugar")),
                      column(4, numericInput("proximate_analysis_Sugar","", value = NULL)),
                      column(4, br(), tags$p(style="text-align:left;","gm/100gm dry Weight"))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Starch")),
                      column(4, numericInput("proximate_analysis_Starch","", value = NULL)),
                      column(4, br(), tags$p(style="text-align:left;","gm/100gm dry Weight"))
                    ),
                    column(2, offset=4, actionBttn("proximate_analysis_Clear", "Clear", style = "jelly", size = "xs", color = "primary", block=T)),
                    column(2, actionBttn("proximate_analysis_Save", "Save", style = "jelly", size = "xs", color = "primary", block=T)),
                    
             ),
             column(5)
           ),br(),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("proximate_analysis_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("proximate_analysis_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('proximate_analysis_Exit',"Exit",style = "jelly", size = "xs", color = "danger", block=T))
                    )
             )
             )