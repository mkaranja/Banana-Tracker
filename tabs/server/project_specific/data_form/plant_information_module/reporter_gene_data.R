
reporter_gene_data <- 
  tabPanel("Reporter Gene Data", value = "reporter_gene_data", hr(),
           
           fluidRow(
             column(6,
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Field Trial Plant ID")),
                      column(4, selectInput("reporter_gene_data_FieldTrialPlantID","", choices = NULL)),
                      column(2, br(), actionBttn("reporter_gene_data_FieldTrialPlantIdInfo", "", style = "jelly", 
                                                 color = "primary", size="xs", icon = icon("arrow-right", lib="font-awesome")))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Sample Plant ID")),
                      column(4, textInput("reporter_gene_data_SamplePlantID", ""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Vector ID")),
                      column(4, textInput("reporter_gene_data_VectorID1","")),
                      column(4, textInput("reporter_gene_data_VectorID2",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Promoter")),
                      column(4, textInput("reporter_gene_data_Promoter1","")),
                      column(4, textInput("reporter_gene_data_Promoter2",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Gene")),
                      column(4, textInput("reporter_gene_data_Gene1","")),
                      column(4, textInput("reporter_gene_data_Gene2",""))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Field Trial Identity")),
                      column(4, selectInput("reporter_gene_data_FieldTrialIdentity","", choices = NULL))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Sample ID")),
                      column(4, selectInput("reporter_gene_data_SampleID", "", choices = NULL))
                    ),
             ),
             column(5,
                    column(6, offset = 3, actionBttn("reporter_gene_data_LoadAndUpdateData", "Load and Update Data", style = "jelly", size = "xs", color = "primary")),
                    column(12, br(),
                           panel_div(class_type = "default",
                                     content = tags$div(
                                       column(8, selectInput("reporter_gene_data_LoadAndUpdateData_FieldTrialPlantID","Field Trial Plant ID", choices = NULL)),
                                       column(4, br(), actionBttn("reporter_gene_data_LoadAndUpdateData_FieldTrialPlantIdInfo", "", icon = icon("arrow-right", lib="font-awesome"), style = "jelly", size="xs", color = "primary")),
                                       
                                       column(8, selectInput("reporter_gene_data_LoadAndUpdateData_SampleID","Sample ID", choices = NULL)),
                                       column(4, br(), actionBttn("reporter_gene_data_LoadAndUpdateData_GetData", "Get Data", style = "jelly", size="xs", color = "primary")),
                                       
                                       column(3, offset = 4, br(), actionBttn("reporter_gene_data_LoadAndUpdateData_Update", "Update", style = "jelly", size="xs", color = "primary"))
                                     )
                           )
                    )
             )
             
           ),hr(),
           fluidRow(
             column(6,
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","GUS Histological Assay")),
                      column(4, numericInput("reporter_gene_data_GUSHistologicalAssay","", value = NULL))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Photo")),
                      column(4, fileInput("reporter_gene_data_Photo","", accept = c('image/png', 'image/jpeg')))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","Average GUS Activity from MUG Assay")),
                      column(4, numericInput("reporter_gene_data_AverageGUSActivityFromMUGAssay","", value = NULL))
                    ),
                    fluidRow(
                      column(4, br(), tags$p(style="text-align:right;","MUG Final Data File")),
                      column(4, fileInput("reporter_gene_data_MUGFinalDataFile",""))
                    ),
                    fluidRow(
                      column(4, offset=2, actionBttn("reporter_gene_data_Clear", "Clear", style = "jelly", size = "xs", color = "primary")),
                      column(4, actionBttn("reporter_gene_data_Save", "Save", style = "jelly", size = "xs", color = "primary"))
                    )
                    
             ),
             column(5)
           ),hr(),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("reporter_gene_data_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("reporter_gene_data_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('reporter_gene_data_Exit',"Exit",style = "jelly", size = "xs", color = "danger", block=T))
                    )
              )
           
  )