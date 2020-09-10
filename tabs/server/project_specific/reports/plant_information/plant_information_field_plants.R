plant_information_field_plants <- 
  tabPanel("Plant Information - Field Plants", value = "plant_information_field_plants", hr(),
          fluidRow(
            column(3,
              panel_div(class_type = "default",
                        content = tags$div(
                          p("Select the Plant Information Report Required"),
                          prettyRadioButtons(inputId = "plant_information_field_plants_report_required",label = "", 
                            choices = c("General Plant Data", "Reporter Gene Data", "Proximate Analyisis",
                                        "Disease Resistance Data-Field", "Bioinformation Data"),
                            inline = TRUE,   status = "info",  fill = F
                          )   
                       ))
                ),
            column(2,
                   panel_div(class_type = "default",
                             content = tags$div(
                               selectInput("plant_information_field_plants_FieldTrialPlantID", "Field Trial Plant ID", choices = c("")),
                               actionBttn("plant_information_field_plants_FieldTrialPlantID_LoadData","Load Data", style = "bordered",  color = "primary", size="xs")
                               ))
            ),
            column(2,
                   panel_div(class_type = "default",
                             content = tags$div(
                               selectInput("plant_information_field_plants_Cultivar", "Cultivar", choices = c("")),
                               actionBttn("plant_information_field_plants_Cultivar_LoadData","Load Data", style = "bordered",  color = "primary", size="xs")
                               
                             ))
            ),
            column(2,
                   panel_div(class_type = "default",
                             content = tags$div(
                               selectInput("plant_information_field_plants_FieldTrialIdentity", "Identity", choices = c("")),
                               actionBttn("plant_information_field_plants_FieldTrialIdentity_LoadData","Load Data", style = "bordered",  color = "primary", size="xs")
                               
                             ))
            ),
            column(2,
                   panel_div(class_type = "default",
                             content = tags$div(
                               selectInput("plant_information_field_plants_OldFieldTrialPlantID", "Old Field Trial Plant ID", choices = c("")),
                               actionBttn("plant_information_field_plants_OldFieldTrialPlantID_LoadData","Load Data", style = "bordered",  color = "primary", size="xs")
                               
                             ))
            )
          ),
          fluidRow(
            column(8,
                   panel_div(class_type = "default",
                          content = tags$div(
                               column(2, selectInput("plant_information_field_plants_VectorID", "Vector ID", choices = c(""))),
                               column(1, br(), actionBttn("plant_information_field_plants_Vector_Go","", icon = icon("angle-double-right", lib="font-awesome"), style = "bordered",  color = "primary", size="xs")),
                               column(2, textInput("plant_information_field_plants_PlantSelection", "Plant Selection")),
                               column(5, textInput("plant_information_field_plants_PromoterGene","Promoter Gene", width = "100%")),
                               column(2, br(), actionBttn("plant_information_field_plants_Vector_LoadData","Load Data", icon = icon("arrow"), style = "bordered",  color = "primary", size="xs"))
                             )),
                   panel_div(class_type = "default",
                             content = tags$div(
                               column(2, br(), p("Date of Planting")),
                               column(3, dateRangeInput("plant_information_field_plants_DateOfPlanting","", width = "100%")),
                               column(2, br(), actionBttn("plant_information_field_plants_DateOfPlanting_LoadData","Load Data", style = "bordered",  color = "primary", size="xs"))
                               
                             )),
                  fluidRow(
                    rHandsontableOutput("plant_information_field_plants_Table"), br(),
                    column(2, offset = 3,
                      actionBttn("plant_information_field_plants_ExportToExcel","Export to Excel", style = "bordered",  color = "primary", size="xs")
                    )
                  )
              )
            ),
          fluidRow(
            column(6),
            column(6,
                   column(7),
                   column(3, actionBttn("plant_information_field_plants_ExportToExcel","Control Form", style = "jelly",  color = "warning", size="xs", block = T)),
                   column(2, actionBttn("plant_information_field_plants_ExportToExcel","Exit", style = "jelly",  color = "danger", size="xs", block = T))
            
            )
          )
  )