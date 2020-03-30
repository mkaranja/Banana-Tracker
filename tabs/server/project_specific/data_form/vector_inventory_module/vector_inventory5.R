vector_inventory5 <- 
  tabPanel("Vector Inventory 5", value = "vector_inventory_5", hr(),
    div(id = "vector_inventory_5_form",
     panel_div(class_type = "default",
        content = tags$div(
           fluidRow(
             column(12,
                    fluidRow(
                      column(4, 
                             column(5, br(), tags$p(style="text-align:right;","DNA Storage Location")),
                             column(7, textInput("vector_inventory_5_DNAStorageLocation", "", width = "100%"))
                             ),
                      column(4, 
                             column(5, br(), tags$p(style="text-align:right;","E.Coli Glycerol Storage Location")),
                             column(7, textInput("vector_inventory_5_EcoliGlycerolStorageLocation", ""))
                             ),
                      column(4, 
                             column(5, br(), tags$p(style="text-align:right;","Agro Glycerol Storage Location")),
                             column(7, textInput("vector_inventory_5_AgroGlycerolStorageLocation", ""))
                             )
                        ),
                    fluidRow(
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","DNA Storage Box")),
                             column(7, textInput("vector_inventory_5_DNAStorageBox", "", width = "100%"))
                             ),
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","E.Coli Glycerol Storage Box")),
                             column(7, textInput("vector_inventory_5_EcoliGlycerolStorageBox", ""))
                             ),
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","Agro Glycerol Storage Box")),
                             column(7, textInput("vector_inventory_5_AgroGlycerolStorageBox", ""))
                             )
                    ),
                    fluidRow(
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","Stored By")),
                             column(7, textInput("vector_inventory_5_DNAStoredBy", "", width = "100%"))
                             ),
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","Stored By")),
                             column(7, textInput("vector_inventory_5_EcoliGlycerolStoredBy", "")),
                      ),
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","Stored By")),
                             column(7, textInput("vector_inventory_5_AgroGlycerolStorageStoredBy",""))
                      )
                    ),
                    fluidRow(
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","Date")),
                             column(7, dateRangeInput("vector_inventory_5_DNAStorageDate", "", width = "100%"))
                      ),
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","Date")),
                             column(7, textInput("vector_inventory_5_EcoliGlycerolStorageDate", ""))
                      ),
                      column(4,
                             column(5, br(), tags$p(style="text-align:right;","Date")),
                             column(7, textInput("vector_inventory_5_AgroGlycerolStorageDate", ""))
                      )
                    )
                  ),
             column(2, offset = 9, br(), actionBttn("vector_inventory_5_SaveThePlantExpressionVectorRecord", "Save the Plant Expression Vector Record", size = "xs", style = "fill", color = "primary", block = T)),
             column(2, offset = 6, actionBttn("vector_inventory_5_Clear", "Clear", size = "xs", style = "jelly", color = "primary", block = T))
           )
        )
      )
  ),hr(), br(),
  fluidRow(
    column(12,
           column(5),
           column(2, actionBttn("vector_inventory_5_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary", block = T)),
           column(1),
           column(2, actionBttn("vector_inventory_5_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block = T)),
           column(1, actionBttn("vector_inventory_5_Exit", "Exit", size = "xs", style = "jelly", color = "primary", block = T))
    )
  )
)