vector_inventory5 <- 
  tabPanel("Vector Inventory 5", value = "vector_inventory_5", 
           useShinyalert(),
    div(id = "vector_inventory_5_form",
     panel_div(class_type = "default",
        content = tags$div(
           fluidRow(
             column(12,
                    fluidRow(
                      column(4, 
                             column(6, br(), tags$p(style="text-align:right;","DNA Storage Location")),
                             column(6, textInput("vector_inventory_5_DNAStorageLocation", "", width = "100%"))
                             ),
                      column(4, 
                             column(6, br(), tags$p(style="text-align:right;","E.Coli Glycerol Storage Location")),
                             column(6, textInput("vector_inventory_5_EcoliGlycerolStorageLocation", ""))
                             ),
                      column(4, 
                             column(6, br(), tags$p(style="text-align:right;","Agro Glycerol Storage Location")),
                             column(6, textInput("vector_inventory_5_AgroGlycerolStorageLocation", ""))
                             )
                        ),
                    fluidRow(
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","DNA Storage Box")),
                             column(6, textInput("vector_inventory_5_DNAStorageBox", "", width = "100%"))
                             ),
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","E.Coli Glycerol Storage Box")),
                             column(6, textInput("vector_inventory_5_EcoliGlycerolStorageBox", ""))
                             ),
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","Agro Glycerol Storage Box")),
                             column(6, textInput("vector_inventory_5_AgroGlycerolStorageBox", ""))
                             )
                    ),
                    fluidRow(
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","Stored By")),
                             column(6, textInput("vector_inventory_5_DNAStoredBy", "", width = "100%"))
                             ),
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","Stored By")),
                             column(6, textInput("vector_inventory_5_EcoliGlycerolStoredBy", "")),
                      ),
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","Stored By")),
                             column(6, textInput("vector_inventory_5_AgroGlycerolStoredBy",""))
                      )
                    ),
                    fluidRow(
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","Date")),
                             column(6, dateInput("vector_inventory_5_DNAStorageDate", "", width = "100%", value = NULL, min = NULL, max = NULL))
                      ),
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","Date")),
                             column(6, dateInput("vector_inventory_5_EcoliGlycerolStorageDate", "", value = NULL, min = NULL, max = NULL))
                      ),
                      column(4,
                             column(6, br(), tags$p(style="text-align:right;","Date")),
                             column(6, dateInput("vector_inventory_5_AgroGlycerolStorageDate", "", value = NULL, min = NULL, max = NULL))
                      )
                    )
                  ),
             column(1, offset = 4, br(), actionBttn("vector_inventory_5_Clear", "Clear", size = "xs", style = "fill", color = "primary", block=T)),
             column(2, offset = 1,  br(), actionBttn("vector_inventory_5_SaveThePlantExpressionVectorRecord", "Save the Plant Expression Vector Record", 
                                              size = "xs", style = "fill", color = "primary", block=T)),
             # conditionalPanel(
             #   condition = "input.vector_inventory_search_LoadDataToView",
             #   column(2, offset = 1,  br(), actionBttn("vector_inventory_5_SaveThePlantExpressionVectorRecord_View", "Save the Plant Expression Vector Record", 
             #                                           size = "xs", style = "fill", color = "primary", block=T))
             # ),
             conditionalPanel(
               condition = "input.vector_inventory_search_LoadDataToUpdate",
                column(2, offset = 1,  br(), actionBttn("vector_inventory_5_UpdateThePlantExpressionVectorRecord", "Update the Plant Expression Vector Record", 
                                                     size = "xs", style = "fill", color = "primary", block=T))
             )
           )
        )
      )
  ),hr(),
  fluidRow(
    column(6),
    column(6,
           column(4),
           column(3, actionBttn("vector_inventory_5_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
           column(3, actionBttn("vector_inventory_5_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
           column(2, actionBttn('vector_inventory_5_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
    )
  )
)