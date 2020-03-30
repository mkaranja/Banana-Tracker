vector_inventory4 <- 
  tabPanel("Vector Inventory 4", value = "vector_inventory_4", hr(),
        div(id = "vector_inventory_4_form",
           fluidRow(br(),
                    column(12,
                           column(2, tags$b("Sequencing Primers")),
                           column(1, actionBttn("vector_inventory_4_SequencingPrimers", "Locate", 
                                                size = "xs", style = "jelly", color = "primary", block = T))
                    )
           ), br(),
           fluidRow(
             column(12,
                    column(3, selectInput("vector_inventory_4_SequencingCompleted", "Sequencing Completed", choices = NULL)),
                    column(3, dateRangeInput("vector_inventory_4_DateOfSequencing", "Date of sequencing")),
                    column(2, numericInput("vector_inventory_4_SeqLabBookNumber", "Lab Book Number", value=0)),
                    column(2, numericInput("vector_inventory_4_SeqPageNumber", "Page Number", value=0))
             )
           ),
           fluidRow(br(),
                    column(12,
                           column(3, tags$h5(style="text-align;","Contig Express Sequencing Alignment")),
                           column(1, br(), actionBttn("vector_inventory_4_ContigExpressSequencingAlignment", "Locate", 
                                                      size = "xs", style = "jelly", color = "primary", block = T)),
                           column(2),
                           column(2, tags$h5(style="text-align;","Sequencing Files")),
                           column(1, br(), actionBttn("vector_inventory_4_SequencingFiles", "Locate", 
                                                      size = "xs", style = "jelly", color = "primary", block = T))
                    )
           ), br(),
           fluidRow(
             column(12,
                    column(3, textInput("vector_inventory_4_CheckedBy", "Checked By")),
                    column(2, dateRangeInput("vector_inventory_4_CheckedDate", "Date")),
                    column(2, textInput("vector_inventory_4_VerifiedBy", "Verified By")),
                    column(2, dateRangeInput("vector_inventory_4_VerifiedDate", "Date"))
             )
           ),
           fluidRow(
             column(12,
                    column(3, selectInput("vector_inventory_4_TranformedIntoAgro", "Tranformed Into Agro", choices = NULL)),
                    column(2, selectInput("vector_inventory_4_Strain", "Strain", choices = NULL)),
                    column(2, selectInput("vector_inventory_4_ConfirmedByPCR", "Confirmed by PCR", choices = NULL)),
                    column(2, dateRangeInput("vector_inventory_4_ConfirmedByPCRDate", "Date"))
             )
           ),br(),
           fluidRow(
             column(2, offset = 7, actionBttn("vector_inventory_4_Clear", "Clear", size = "xs", style = "jelly", color = "primary", block = T))
           ),hr(), br(),
           fluidRow(
             column(12,
                    column(5),
                    column(2, actionBttn("vector_inventory_4_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary", block = T)),
                    column(1),
                    column(2, actionBttn("vector_inventory_4_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block = T)),
                    column(1, actionBttn("vector_inventory_4_Exit", "Exit", size = "xs", style = "jelly", color = "primary", block = T))
             )
           ) 
        )
  )