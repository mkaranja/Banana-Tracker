
vector_inventory2 <- 
  tabPanel("Vector Inventory 2 - Cassette", value = "vector_inventory_2", 
           useShinyalert(),
           div(id = "vector_inventory_2_form",
               #tags$head(tags$style(HTML(".form-group {margin-bottom: 0 !important;}"))), # reduce space between widgets
               panel_div(class_type = "default",
                         content = tags$div(
                           fluidRow(
                             column(12,
                                    fluidRow(
                                      column(2, offset = 1, h4("Cassette 1")),
                                      column(2, h4("Cassette 2")),
                                      column(2, h4("Cassette 3")),
                                      column(2, h4("Cassette 4")),
                                      column(2, h4("Cassette 5"))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Promoter")),
                                      column(2, selectInput("vector_inventory_2_Cassette1Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette2Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette3Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette4Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette5Promoter","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Gene")),
                                      column(2, selectInput("vector_inventory_2_Cassette1Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette2Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette3Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette4Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette5Gene","", choices =  NULL))
                                    ),
                                    
                                    fluidRow(
                                      column(1, br(), tags$b("Terminator")),
                                      column(2, selectInput("vector_inventory_2_Cassette1Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette2Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette3Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette4Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette5Terminator","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Add Feature")),
                                      column(2, selectInput("vector_inventory_2_Cassette1Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette2Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette3Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette4Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette5Feature1","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(2, offset = 1, textInput("vector_inventory_2_Cassette1Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette2Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette3Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette4Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette5Feature1Desc",""))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Add Feature")),
                                      column(2, selectInput("vector_inventory_2_Cassette1Feature2","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette2Feature2"," ", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette3Feature2","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette4Feature2"," ", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette5Feature2","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(2, offset = 1, textInput("vector_inventory_2_Cassette1Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette2Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette3Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette4Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette5Feature2Desc",""))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Add Feature")),
                                      column(2, selectInput("vector_inventory_2_Cassette1Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette2Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette3Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette4Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette5Feature3","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(2, offset = 1, textInput("vector_inventory_2_Cassette1Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette2Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette3Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette4Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette5Feature3Desc",""))
                                    )
                             )
                           )
                         )
               ),
               panel_div(class_type = "default",
                         content = tags$div(
                           fluidRow(
                             column(12,
                                    fluidRow(
                                      column(2, offset = 1, h4("Cassette 6")),
                                      column(2, h4("Cassette 7")),
                                      column(2, h4("Cassette 8")),
                                      column(2, h4("Cassette 9")),
                                      column(2, h4("Cassette 10"))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Promoter")),
                                      column(2, selectInput("vector_inventory_2_Cassette6Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette7Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette8Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette9Promoter","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette10Promoter","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Gene")),
                                      column(2, selectInput("vector_inventory_2_Cassette6Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette7Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette8Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette9Gene","", choices =  NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette10Gene","", choices =  NULL))
                                    ),
                                    
                                    fluidRow(
                                      column(1, br(), tags$b("Terminator")),
                                      column(2, selectInput("vector_inventory_2_Cassette6Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette7Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette8Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette9Terminator","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette10Terminator","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Add Feature")),
                                      column(2, selectInput("vector_inventory_2_Cassette6Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette7Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette8Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette9Feature1","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette10Feature1","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(2, offset = 1, textInput("vector_inventory_2_Cassette6Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette7Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette8Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette9Feature1Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette10Feature1Desc",""))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Add Feature")),
                                      column(2, selectInput("vector_inventory_2_Cassette6Feature2","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette7Feature2","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette8Feature2","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette9Feature2"," ", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette10Feature2","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(2, offset = 1, textInput("vector_inventory_2_Cassette6Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette7Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette8Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette9Feature2Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette10Feature2Desc",""))
                                    ),
                                    fluidRow(
                                      column(1, br(), tags$b("Add Feature")),
                                      column(2, selectInput("vector_inventory_2_Cassette6Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette7Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette8Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette9Feature3","", choices = NULL)),
                                      column(2, selectInput("vector_inventory_2_Cassette10Feature3","", choices = NULL))
                                    ),
                                    fluidRow(
                                      column(2, offset = 1, textInput("vector_inventory_2_Cassette6Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette7Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette8Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette9Feature3Desc","")),
                                      column(2, textInput("vector_inventory_2_Cassette10Feature3Desc",""))
                                    )
                             )
                             
                             # uiOutput("vector_inventory2_cassette_out")
                           )
                         )
               )
           ),
           fluidRow(
             column(1, offset = 7, actionBttn("vector_inventory_2_Clear", "Clear", size = "xs", style = "fill", color = "primary", block=T))
           ),hr(),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("vector_inventory_2_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("vector_inventory_2_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('vector_inventory_2_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
           
  )