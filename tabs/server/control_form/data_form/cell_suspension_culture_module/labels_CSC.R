## ------------------------------------------ Labels CSC

labels_CSC <-  
  tabPanel("Labels CSC", value = "labels_CSC",
           br(),
           useShinyjs(),
           
           fluidRow(
             column(12, 
                    column(2, br(), tags$h4(style="text-align:right;","Select the Date")),
                    column(2, uiOutput("labels_CSC_SelectDate_output")),
                    column(2, br(), actionBttn("labels_CSC_LOadData", "Load Data", style = "fill", size = "xs", color = "primary"))
             ),
             column(12,
                    rHandsontableOutput("labels_CSC_Table")
             )
           ),br(), br(),
           fluidRow(
             column(2, offset = 2, downloadBttn("labels_CSC_ExportToExcel", "Export to Excel", style = "bordered", size = "xs", color = "primary")),
             column(2, offset = 1, downloadBttn("labels_CSC_barcodeLabels", "Barcodes", style = "bordered", size = "xs", color = "success")),
             column(2, downloadBttn("labels_CSC_excelLabels", "Excel Labels", style = "bordered", size = "xs", color = "success"))
           ), br(), 
           fluidRow(
             column(2, offset = 4, actionBttn("labels_CSC_Clear", "Clear", style = "fill", size = "xs", color = "primary"))
           ),
           hr(),
           fluidRow(
             column(6),
             column(6,
                    column(3,offset = 4, actionBttn("labels_CSC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("labels_CSC_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn("labels_CSC_Exit", "Exit", style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
           
  )

#div(style="display:inline-block",downloadButton('downloadData', 'Download Data'), style="float:right")