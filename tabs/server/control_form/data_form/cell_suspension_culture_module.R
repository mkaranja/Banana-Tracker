
tab_files <- list.files(path = "tabs/server/control_form/data_form/cell_suspension_culture_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


observeEvent(input$cell_suspension_culture_module,{
  
  showModal(modalDialog(tags$h3(style="color:#800000;text-align:center;","Cell Suspension Culture Module"),
                        
                        tabsetPanel( type = "pills", 
                                     search_CSC,
                                     new_CSC,
                                     update_last_subcultureCSC
                                     
                        ),easyClose = F, size = "l"
  ))
  
})
