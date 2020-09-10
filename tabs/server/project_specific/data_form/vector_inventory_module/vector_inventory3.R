
vector_inventory3 <- 
  tabPanel("Vector Inventory 3 - Screening Primers 1", value = "vector_inventory_3", 
           useShinyalert(),
           panel_div(class_type = "default",
                content = tags$div(
                    div(id = "vector_inventory_3_form", 
                        column(12,   
                            fluidRow(
                              column(2, offset = 1, tags$h4("PCR Screening Primers")),
                              column(2, offset = 1, tags$b(style="text-align:center", "Name")),
                              column(2, tags$b(style="text-align:center", "Code")),
                              column(2, tags$b(style="text-align:center", "Sequence"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(5, offset = 4, br(), h4("Cassette 1")),
                                     column(3, br(), tags$h5(style="text-align:right", "Forward"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette1ForwardName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette1ForwardCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette1ForwardSequence", "", width = "100%"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(6),
                                     column(6, br(), tags$h5(style="text-align:right","Reverse"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette1ReverseName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette1ReverseCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette1ReverseSequence", "", width = "100%"))
                            ),
                            
                            # 2 ..
                            
                            fluidRow(
                              column(3, 
                                     column(5, offset = 4, br(), h4("Cassette 2")),
                                     column(3, br(), tags$h5(style="text-align:right", "Forward"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette2ForwardName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette2ForwardCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette2ForwardSequence", "", width = "100%"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(6),
                                     column(6, br(), tags$h5(style="text-align:right","Reverse"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette2ReverseName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette2ReverseCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette2ReverseSequence", "", width = "100%"))
                            ),
                            
                            # 3 .. 
                            
                            fluidRow(
                              column(3, 
                                     column(5, offset = 4, br(), h4("Cassette 3")),
                                     column(3, br(), tags$h5(style="text-align:right", "Forward"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette3ForwardName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette3ForwardCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette3ForwardSequence", "", width = "100%"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(6),
                                     column(6, br(), tags$h5(style="text-align:right","Reverse"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette3ReverseName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette3ReverseCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette3ReverseSequence", "", width = "100%"))
                            ),
                            
                            # 4..
                            
                            fluidRow(
                              column(3, 
                                     column(5, offset = 4, br(), h4("Cassette 4")),
                                     column(3, br(), tags$h5(style="text-align:right", "Forward"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette4ForwardName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette4ForwardCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette4ForwardSequence", "", width = "100%"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(6),
                                     column(6, br(), tags$h5(style="text-align:right","Reverse"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette4ReverseName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette4ReverseCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette4ReverseSequence", "", width = "100%"))
                            ),
                            
                            # 5..
                            
                            fluidRow(
                              column(3, 
                                     column(5, offset = 4, br(), h4("Cassette 5")),
                                     column(3, br(), tags$h5(style="text-align:right", "Forward"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette5ForwardName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette5ForwardCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette5ForwardSequence", "", width = "100%"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(6),
                                     column(6, br(), tags$h5(style="text-align:right","Reverse"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette5ReverseName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette5ReverseCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette5ReverseSequence", "", width = "100%"))
                            ),
                            
                            # --------------------------------------------------------------------------------------------------------
                            # 6 ..
                            
                            fluidRow(
                              column(3, 
                                     column(5, offset = 4, br(), h4("Cassette 6")),
                                     column(3, br(), tags$h5(style="text-align:right", "Forward"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette6ForwardName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette6ForwardCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette6ForwardSequence", "", width = "100%"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(6),
                                     column(6, br(), tags$h5(style="text-align:right","Reverse"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette6ReverseName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette6ReverseCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette6ReverseSequence", "", width = "100%"))
                            ),
                            
                            # 7 ..
                            
                            fluidRow(
                              column(3, 
                                     column(5, offset = 4, br(), h4("Cassette 7")),
                                     column(3, br(), tags$h5(style="text-align:right", "Forward"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette7ForwardName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette7ForwardCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette7ForwardSequence", "", width = "100%"))
                            ),
                            fluidRow(
                              column(3, 
                                     column(6),
                                     column(6, br(), tags$h5(style="text-align:right","Reverse"))
                              ),
                              column(2, textInput("vector_inventory_3_Cassette7ReverseName", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette7ReverseCode", "", width = "100%")),
                              column(2, textInput("vector_inventory_3_Cassette7ReverseSequence", "", width = "100%"))
                            ),
                              
                              # 8 .. 
                              fluidRow(
                                column(3, 
                                       column(5, offset = 4, br(), h4("Cassette 8")),
                                       column(3, br(), tags$h5(style="text-align:right", "Forward"))
                                ),
                                column(2, textInput("vector_inventory_3_Cassette8ForwardName", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette8ForwardCode", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette8ForwardSequence", "", width = "100%"))
                              ),
                              fluidRow(
                                column(3, 
                                       column(6),
                                       column(6, br(), tags$h5(style="text-align:right","Reverse"))
                                ),
                                column(2, textInput("vector_inventory_3_Cassette8ReverseName", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette8ReverseCode", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette8ReverseSequence", "", width = "100%"))
                              ),
                              
                              # 9..
                              
                              fluidRow(
                                column(3, 
                                       column(5, offset = 4, br(), h4("Cassette 9")),
                                       column(3, br(), tags$h5(style="text-align:right", "Forward"))
                                ),
                                column(2, textInput("vector_inventory_3_Cassette9ForwardName", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette9ForwardCode", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette9ForwardSequence", "", width = "100%"))
                              ),
                              fluidRow(
                                column(3, 
                                       column(6),
                                       column(6, br(), tags$h5(style="text-align:right","Reverse"))
                                ),
                                column(2, textInput("vector_inventory_3_Cassette9ReverseName", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette9ReverseCode", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette9ReverseSequence", "", width = "100%"))
                              ),
                              
                              # 10..
                              
                              fluidRow(
                                column(3, 
                                       column(5, offset = 4, br(), h4("Cassette 10")),
                                       column(3, br(), tags$h5(style="text-align:right", "Forward"))
                                ),
                                column(2, textInput("vector_inventory_3_Cassette10ForwardName", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette10ForwardCode", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette10ForwardSequence", "", width = "100%"))
                              ),
                              fluidRow(
                                column(3, 
                                       column(6),
                                       column(6, br(), tags$h5(style="text-align:right","Reverse"))
                                ),
                                column(2, textInput("vector_inventory_3_Cassette10ReverseName", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette10ReverseCode", "", width = "100%")),
                                column(2, textInput("vector_inventory_3_Cassette10ReverseSequence", "", width = "100%")),
                                column(1, offset = 1, br(), actionBttn("vector_inventory_3_Clear", "Clear", size = "xs", style = "fill", color = "primary", block=T))
                              )
                            )
                     ), br()
                    )),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("vector_inventory_3_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("vector_inventory_3_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('vector_inventory_3_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
  )
  
           
           
           