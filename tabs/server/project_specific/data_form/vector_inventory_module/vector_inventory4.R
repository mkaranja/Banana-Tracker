vector_inventory4 <- 
  tabPanel("Vector Inventory 4 - Screening Primers 2", value = "vector_inventory_4", 
           useShinyalert(),
        div(id = "vector_inventory_4_form",
            panel_div(class_type = "default",
                content = tags$div(
                     fluidRow(
                        column(10,
                               column(2, tags$h5("Sequencing Primers")),
                               column(1, actionBttn("vector_inventory_4_SequencingPrimers", "Locate", size = "xs", style = "fill", color = "primary", block=T))
                        )
                     ), br(),
                     fluidRow(
                       column(10,
                              column(3, selectInput("vector_inventory_4_SequencingCompleted", "Sequencing Completed", choices = c("","Yes","No"))),
                              column(3, dateInput("vector_inventory_4_DateOfSequencing", "Date of sequencing", format = "yyyy-mm-dd")),
                              column(3, numericInput("vector_inventory_4_SeqPrimersLabBookNumber", "Lab Book Number", value=0)),
                              column(3, numericInput("vector_inventory_4_SeqPrimersPageNumber", "Page Number", value=0))
                       )
                     ),br(),
                     fluidRow(
                        column(10,
                               column(3, tags$h5("Contig Express Sequencing Alignment")),
                               column(3, fileInput("vector_inventory_4_ContigExpressSequencingAlignment", "", placeholder = "Locate", multiple = F)),
                               column(1), 
                               column(2, tags$h5(style="text-align:right;","Sequencing Files")),
                               column(3, fileInput("vector_inventory_4_SequencingFiles", "", multiple = F, placeholder = "Locate"))
                        )
                     ), br(),
                     fluidRow(
                       column(10,
                              column(3, selectInput("vector_inventory_4_CheckedBy", "Checked By", choices = NULL)),
                              column(3, dateInput("vector_inventory_4_CheckedDate", "Date", format = "yyyy-mm-dd")),
                              column(3, selectInput("vector_inventory_4_VerifiedBy", "Verified By", choices = NULL)),
                              column(3, dateInput("vector_inventory_4_VerifiedDate", "Date", format = "yyyy-mm-dd"))
                       )
                     ), br(),
                     fluidRow(
                       column(10,
                              column(3, selectInput("vector_inventory_4_TranformedIntoAgro", "Tranformed Into Agro", choices = c('',"Yes","No"))),
                              column(3, selectInput("vector_inventory_4_Strain", "Strain", choices = NULL)),
                              column(3, selectInput("vector_inventory_4_ConfirmedByPCR", "Confirmed by PCR", choices = c('',"Yes","No"))),
                              column(3, dateInput("vector_inventory_4_ConfirmedByPCRDate", "Date", format = "yyyy-mm-dd"))
                       )
                     ),br(),
                     fluidRow(
                       column(1, offset = 7, actionBttn("vector_inventory_4_Clear", "Clear", size = "xs", style = "fill", color = "primary", block=T))
                     )
                    )),hr(),
            fluidRow(
              column(6),
              column(6,
                     column(4),
                     column(3, actionBttn("vector_inventory_4_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                     column(3, actionBttn("vector_inventory_4_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                     column(2, actionBttn('vector_inventory_4_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
              )
            ) 
        )
  )