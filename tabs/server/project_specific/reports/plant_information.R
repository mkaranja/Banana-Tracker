tab_files <- list.files(path = "tabs/server/project_specific/reports/plant_information", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

observeEvent(input$plant_information_reports,{
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Plant Information - ", input$project_selected)),
                        
                        tabsetPanel(id = "plant_information_reports", type = "pills",
                                    plant_information_field_plants,
                                    disease_resistance_data_glasshouse
                        ), easyClose = F, size = "l"
  ))
})

