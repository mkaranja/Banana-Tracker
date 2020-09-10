

explant_labels <- 
  tabPanel("Labels", value = "explant_labels",
           br(),br(),
           useShinyjs(),
           
           br(),
           useShinyjs(),
           
           fluidRow(
             column(12, 
                    column(2, br(), tags$h4(style="text-align:right;","Select the Date")),
                    column(2, dateInput("explant_labels_SelectDate", "")),
                    column(2, br(), actionBttn("explant_labels_LOadData", "Load Data", style = "fill", size = "xs", color = "primary"))
             ),
             column(12,
                    rHandsontableOutput("explant_labels_Table")
             )
           ),br(), br(),
           fluidRow(
             column(2, offset = 4, downloadBttn("explant_labels_ExportToExcel", "Export to Excel", style = "bordered", size = "xs", color = "primary"))
           ), br(), 
           fluidRow(
             column(2, offset = 4, actionBttn("explant_labels_Clear", "Clear", style = "fill", size = "xs", color = "primary"))
           ), hr(),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("explant_search_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("explant_search_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('explant_search_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
  )