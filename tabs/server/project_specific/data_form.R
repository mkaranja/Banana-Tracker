# tab_files1 <- list.files(path = "tabs/server/project_specific/IBBTV/ui/vector_inventory_module", full.names = T, recursive = T)
# tab_files2 <- list.files(path = "tabs/server/project_specific/IBBTV/ui/transformation_module", full.names = T, recursive = T)
# suppressMessages(lapply(tab_files1, source))
# suppressMessages(lapply(tab_files2, source))

source("tabs/server/project_specific/IBBTV/ui/vector_inventory_module/vector_inventory1.R")
source("tabs/server/project_specific/IBBTV/ui/vector_inventory_module/vector_inventory2.R")
source("tabs/server/project_specific/IBBTV/ui/vector_inventory_module/vector_inventory3.R")
source("tabs/server/project_specific/IBBTV/ui/vector_inventory_module/search.R")

source("tabs/server/project_specific/IBBTV/ui/transformation_module/new_transformation.R")
observeEvent(input$ibbtv_vector_inventory_module,{
  
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;","Vector Inventory Module - IBBTV Project"),
                        
                        tabsetPanel(type = "pills", selected = "ibbtv_vector_inventory_1",
                                    ibbtv_vector_inventory1,
                                    ibbtv_vector_inventory2,
                                    ibbtv_vector_inventory3,
                                    ibbtv_search
                                    ),easyClose = F, size = "l"
  ))
  
})

observeEvent(input$ibbtv_transformation_module,{
 
 showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;","Transformation Module - IBBTV Project"),
                       
                       tabsetPanel(type = "pills", selected = "ibbtv_new_transformation",
                                   ibbtv_new_transformation#,
                                   # ibbtv_search_transformation,
                                   # ibbtv_updating_last_subculture,
                                   ),easyClose = F, size = "l"
 ))
 
})

# observeEvent(input$ibbtv_plant_tissue_culture_module,{
#   
#   showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;","Plant Tissue Culture Module - IBBTV Project"),
#                         
#                         tabsetPanel( type = "pills", selected = ""),easyClose = F, size = "l"
#   ))
#   
# })
# 
# observeEvent(input$ibbtv_plant_information_module,{
#   
#   showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;","Plant Information Module - IBBTV Project"),
#                         
#                         tabsetPanel( type = "pills", selected = ""),easyClose = F, size = "l"
#   ))
#   
# })
# 
# 
# ## ------------------------------------------- 1. Vector Inventory Module ---------------------------------
# 
# observeEvent(input$vector_inventory_1_ClearAllTheTabs,{
#   reset("vector_inventory_1_VectorID")
#   reset("ibbtv_vector_inventory_1_form")
# })
# 
# observeEvent(input$vector_inventory_1_VectorCode,{
#   suffix_dt <- tbl(pool, "IBBTV_tblVectorSuffixIdentity") %>% collect()
#   suffix <- max(as.integer(suffix_dt$VectorSuffixID))+1
#   id <- paste0(input$vector_inventory_1_VectorPrefix, "-",toupper(input$vector_inventory_1_VectorCode),"-",suffix)
#     
#   updateTextInput("vector_inventory_1_VectorID", "Vector ID", value = as.character(id))
#   updateTextInput("vector_inventory_1_VectorSuffix", "Vector Suffix", value = as.character(suffix))
# })
