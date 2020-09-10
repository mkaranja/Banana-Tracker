panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}



new_plant_tissue_culture <- 
  fluidRow(         
       column(12, hr(), br(),
              div(id = "new_plant_tissue_culture_Form",                   
                  fluidRow(
                    column(2, selectInput("new_plant_tissue_culture_PTCIdentify", "PTC Identify", choices = c(''), width = "100%")),
                    column(2, selectInput("new_plant_tissue_culture_Source", "Source", choices = c(''), width = "100%")),
                    column(2, selectInput("new_plant_tissue_culture_Cultivar", "Cultivar", choices = c('', mfc$Cultivar), width = "100%")),
                    column(2, selectInput("new_plant_tissue_culture_CultivarConfirmed", "Cultivar Confirmed", choices = c('','Yes','No'), width = "100%")),
                    column(2, selectInput("new_plant_tissue_culture_VirusIndexed", "Virus Indexed", choices = c('','Yes','No'), width = "100%"))
                  ),
                  fluidRow(
                    column(2, uiOutput("new_plant_tissue_culture_SelectTheFields_Output")),
                    column(9, br(), rHandsontableOutput("new_plant_tissue_culture_ResultsTable"))
                  )
              ), br(),
              fluidRow(
                column(1, actionBttn("new_plant_tissue_culture_LoadData", "Load Data", size = "xs", style = "jelly", color = "primary", block=T)),
                column(2, downloadBttn("new_plant_tissue_culture_ExportToExcel", "Export to Excel", size = "xs", style = "bordered", color = "primary")),
                column(1, actionBttn("new_plant_tissue_culture_Clear", "Clear", size = "xs", style = "jelly", color = "warning", block=T)),
                column(2 , actionBttn("new_plant_tissue_culture_ControlForm", "Control Form", size = "xs", style = "jelly", color = "primary", block=T)),
                column(2, actionBttn("new_plant_tissue_culture_FormToPicture", "Form to Picture", size = "xs", style = "jelly", color = "primary"))
              )
       )
  )


# ------------SERVER-------------------------------

observeEvent(input$new_plant_in_plant_tissue_culture_report,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("New Plant Tissue Culture Reports - ", input$project_selected)),
                        new_plant_tissue_culture,
                        easyClose = F, size = "l"
  ))
  
})