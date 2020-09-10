## ------------------------------------------ Labels Transformation

labels_Transformation <-  
  tabPanel("Labels", value = "labelsTransformation",
           hr(),
           useShinyjs(),
           
           fluidRow(
             column(12, 
                    column(2, br(), tags$h5(style="text-align:right;","Select the Date")),
                    column(2, #uiOutput("labelsTransformation_SelectDate_output")
                           dateInput("labelsTransformation_SelectDate", "", min = NULL, max = NULL)
                           ),
                    column(2, br(), actionBttn("labelsTransformation_LoadData", "Load Data", style = "fill", size = "xs", color = "primary"))
             ),
             column(12,
                    rHandsontableOutput("labelsTransformation_Table")
             )
           ),br(), br(),
           fluidRow(
             column(2, offset = 2, downloadBttn("labelsTransformation_ExportToExcel", "Export to Excel", style = "bordered", size = "xs", color = "primary")),
             column(1, downloadBttn("labelsTransformation_barcodeLabels", "Barcodes", style = "bordered", size = "xs", color = "success")),
             column(2, downloadBttn("labelsTransformation_excelLabels", "Excel Labels", style = "bordered", size = "xs", color = "success"))
           ), br(), 
           fluidRow(
             column(2, offset = 4, actionBttn("labelsTransformation_Clear", "Clear", style = "fill", size = "xs", color = "primary"))
           ),
           hr(),
           fluidRow(
             column(6),
             column(6,
                    column(3,offset = 4, actionBttn("labelsTransformation_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("labelsTransformation_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn("labelsTransformation_Exit", "Exit", style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
           
  )

#div(style="display:inline-block",downloadButton('downloadData', 'Download Data'), style="float:right")