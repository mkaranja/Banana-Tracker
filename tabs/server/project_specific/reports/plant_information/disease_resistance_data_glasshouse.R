disease_resistance_data_glasshouse <- 
  tabPanel("Disease Resistance Data - Glasshouse", value = "disease_resistance_data_glasshouse", hr(),
           fluidRow(
             column(2,
                    panel_div(class_type = "default",
                              content = tags$div(
                                selectInput("disease_resistance_data_glasshouse_PlantTissueCultureIdentity", "Plant Tissue Culture Identity", choices = c("")),
                                actionBttn("disease_resistance_data_glasshouse_PlantTissueCultureIdentity_LoadData","Load Data", style = "bordered",  color = "primary", size="xs")
                              ))
             ),
             column(2,
                    panel_div(class_type = "default",
                              content = tags$div(
                                selectInput("disease_resistance_data_glasshouse_Cultivar", "Cultivar", choices = c("")),
                                actionBttn("disease_resistance_data_glasshouse_Cultivar_LoadData","Load Data", style = "bordered",  color = "primary", size="xs")
                                
                              ))
             ),
             column(2,
                    panel_div(class_type = "default",
                              content = tags$div(
                                selectInput("disease_resistance_data_glasshouse_GlasshouseTrialID", "Glasshouse Trial ID", choices = c("")),
                                actionBttn("disease_resistance_data_glasshouse_GlasshouseTrialID_LoadData","Load Data", style = "bordered",  color = "primary", size="xs")
                                
                              ))
             ),
             column(2, br(),  actionBttn("disease_resistance_data_glasshouse_Clear","Clear", style = "bordered",  color = "primary", size="xs")
             )
           ),
           fluidRow(
             column(8,
                    panel_div(class_type = "default",
                              content = tags$div(
                                column(2, selectInput("disease_resistance_data_glasshouse_VectorID", "Vector ID", choices = c(""))),
                                column(1, br(), actionBttn("disease_resistance_data_glasshouse_Vector_Go","", icon = icon("angle-double-right", lib="font-awesome"), style = "bordered",  color = "primary", size="xs")),
                                column(2, textInput("disease_resistance_data_glasshouse_PlantSelection", "Plant Selection")),
                                column(5, textInput("disease_resistance_data_glasshouse_PromoterGene","Promoter Gene", width = "100%")),
                                column(2, br(), actionBttn("disease_resistance_data_glasshouse_Vector_LoadData","Load Data", icon = icon("arrow"), style = "bordered",  color = "primary", size="xs"))
                              )),
                    panel_div(class_type = "default",
                              content = tags$div(
                                column(2, br(), p("Date of Planting")),
                                column(3, dateRangeInput("disease_resistance_data_glasshouse_DateOfPlanting","", width = "100%")),
                                column(2, br(), actionBttn("disease_resistance_data_glasshouse_DateOfPlanting_LoadData","Load Data", style = "bordered",  color = "primary", size="xs"))
                                
                              )),
                    fluidRow(
                      rHandsontableOutput("disease_resistance_data_glasshouse_Table"), br(),
                      column(2, offset = 3,
                             actionBttn("disease_resistance_data_glasshouse_ExportToExcel","Export to Excel", style = "bordered",  color = "primary", size="xs")
                      )
                    )
             )
           ),
           fluidRow(
             column(6),
             column(6,
                    column(7),
                    column(3, actionBttn("disease_resistance_data_glasshouse_ExportToExcel","Control Form", style = "jelly",  color = "warning", size="xs", block = T)),
                    column(2, actionBttn("disease_resistance_data_glasshouse_ExportToExcel","Exit", style = "jelly",  color = "danger", size="xs", block = T))
             )
           )
           
  )