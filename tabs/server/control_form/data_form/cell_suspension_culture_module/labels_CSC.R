
labels_CSC <-  
  tabPanel("Labels CSC", value = "labels_CSC",
          br(),
          useShinyjs(),
          
          fluidRow(
            column(12, 
                   column(2, br(), tags$h4(style="text-align:right;","Select the Date")),
                   column(2, dateInput("labels_CSC_SelectDate", "")),
                   column(2, br(), actionBttn("labels_CSC_LOadData", "Load Data", style = "fill", size = "xs", color = "primary"))
                   ),
            column(12,
                   rHandsontableOutput("labels_CSC_Table")
                   )
            ),br(), br(),
            fluidRow(
              column(2, offset = 4, downloadBttn("labels_CSC_ExportToExcel", "Export to Excel", style = "bordered", size = "xs", color = "primary"))
              ), br(), 
            fluidRow(
              column(2, offset = 4, actionBttn("labels_CSC_Clear", "Clear", style = "fill", size = "xs", color = "primary"))
            ),
          
         fluidRow(br(),hr(),
                      column(8, offset = 2,
                             column(2, actionBttn("labels_CSC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                             column(4, actionBttn("labels_CSC_SCP_CSC_ControlForm", "MFC, SCP and CSC Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                             column(2, 
                                    tags$button(id = 'labels_CSC_Exit', type = "button", class = "btn action-button",
                                                onclick = "setTimeout(function(){window.close();},500);",  # close browser
                                                "Exit", style="color: #fff; background-color: #ff0011; border-color: #ff0011; padding:4px; font-size:80%;")
                                    #actionBttn("search_culture_initiation_Exit", "Exit", style = "jelly", size = "xs", color = "danger", block=T)
                             )
                      )
          )
          
)