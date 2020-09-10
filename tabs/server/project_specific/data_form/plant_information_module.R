tab_files <- list.files(path = "tabs/server/project_specific/data_form/plant_information_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

# Plant Information Module

observeEvent(input$plant_information_module, {
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;", paste("Plant Information Module - ",input$project_selected)),
                        
                        tabsetPanel(type = "pills", selected = "general_plant_data",
                                    general_plant_data,
                                    proximate_analysis,
                                    reporter_gene_data,
                                    resistance_glasshouse,
                                    resistance_field
                                     
                        ),
                        easyClose = F, size = "l"
  ))
})

## GENERAL PLANT DATA
observeEvent(input$plant_information_module, {
  tb <- paste0(input$project_selected, "_tblFieldTrialIdentity")
  updateSelectInput(session, "general_plant_data_FTPlantID", "", choices = c('', loadData(tb)$FieldTrialIdentity))
  
})