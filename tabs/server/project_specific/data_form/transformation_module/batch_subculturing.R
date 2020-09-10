batch_subculturing <- 
  tabPanel("Batch Subculturing Module", value = "batch_subculturing",
           hr(),
           useShinyalert(),
           div(id = "batch_subculturing_Form",
               fluidRow(
                 column(3, dateRangeInput("batch_subculturingDateOfStarterCulture", "Date of Starter Culture")),
                 column(1, br(), actionBttn("batch_subculturingSearch", "Search", style = "fill", size = "xs", color = "primary", block = T)),
                 column(1),
                 column(1, br(), actionBttn("batch_subculturingClearForm", "Clear Form", style = "fill", size = "xs", color = "primary", block = T)),
                 ), 
                fluidRow(
                  column(11, rHandsontableOutput("hot")), 
                 column(3, br(),br(),
                        dateInput("batch_subculturing_DateOfCulture",labelMandatory("Date of Cultures"), width = "100%"),
                        selectInput("batch_subculturing_CulturedBy",labelMandatory("Cultured By"), choices = c('', loadData("tblCulturedBy")$CulturedBy), width = "100%"),
                        selectInput("batch_subculturing_MediaForCultures",labelMandatory("Media"), choices = c('', loadData("tblMedia")$Media), width = "100%"),
                        numericInput("batch_subculturing_LabBookNumberForCultures",labelMandatory("Lab Book Number"), min = 0, value = NULL, width = "100%"),
                        numericInput("batch_subculturing_PageNumberForCultures",labelMandatory("Page Number"), min = 0, value = NULL, width = "100%"),
                        textInput("batch_subculturing_Comments", "Comments", width = "100%")
                 ),
                 column(3, br(), br(),
                        panel_div(class_type = "default",content = tags$div(
                                    tags$b("Additives"),
                                    awesomeCheckboxGroup(inputId = "batch_subculturing_AdditivesForCultures", label = "", choices = c(loadData("tblAdditives")$Additives), selected = NULL, status = "info")
                                  )),br(), br(), 
                        column(12, offset = 3,
                        actionBttn("batch_subculturing_Save","Save Culture", style = "fill", size = "xs", color = "primary"), br(), br(),
                        actionBttn("batch_subculturing_ExportToExcel","Export To Excel", style = "fill", size = "xs", color = "primary")
                        ))
               )
           )
  )