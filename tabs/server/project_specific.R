tab_files1 <- list.files(path = "tabs/server/project_specific/data_form/vector_inventory_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files1, source))
tab_files2 <- list.files(path = "tabs/server/project_specific/data_form/plant_information_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files2, source))
tab_files3 <- list.files(path = "tabs/server/project_specific/administration", full.names = T, recursive = T)
suppressMessages(lapply(tab_files3, source))

assigned_projects <- reactive({
  username <- input$username
  user_projects <- usersinfo %>%
    filter(UserName == username)
  
   pjs <- stringi::stri_remove_empty(c(ifelse(grepl("YES",user_projects$Project1[1])==T, "IBBTV",''),
            ifelse(grepl("YES",user_projects$Project2[1])==T, "IBXW",''),
            ifelse(grepl("YES",user_projects$Project3[1])==T, "IBSV",''),
            ifelse(grepl("YES",user_projects$Project4[1])==T, "Nematode",'')
           ))
   pjs
})

output$select_project_specific_bttn <- renderUI({
  
  radioGroupButtons(inputId = "project_selection_selected",label = "", size = "lg",width = "75%",status = "primary",
    choices = assigned_projects(), individual = TRUE, justified = TRUE
  )
})

## 1. ------------DATA FORM---------------------------------------
### A ..........................Vector Inventory Module .......................
vector_module_project <- reactive({
  req(input$project_selection_selected)
  paste("Vector Inventory Module - ",input$project_selection_selected)
})

observeEvent(input$vector_inventory_module,{
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Vector Inventory Module - ",input$project_selection_selected)),
                        
                         tabsetPanel(id = "vector_inventory_Tabs", type = "pills", selected = "vector_inventory_1",
                                     vector_inventory1,
                                     vector_inventory2,
                                     vector_inventory3,
                                     vector_inventory4,
                                     vector_inventory5,
                                     search
                                     ),
                        easyClose = F, size = "l"
  ))
})

source("tabs/server/project_specific/data_form/vector_inventory_module.R", local=T)


# ------------------------------------------------------------------------------------------------------------------------
observeEvent(input$transformation_module, {
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Transformation Module - ",input$project_selection_selected)),
                        
                        tabsetPanel(type = "pills", selected = ""
                        ),
                        easyClose = F, size = "l"
  ))
})

observeEvent(input$plant_tissue_culture_module, {
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Plant Tissue Culture Module - ",input$project_selection_selected)),
                        
                        tabsetPanel(type = "pills", selected = ""
                        ),
                        easyClose = F, size = "l"
  ))
})


# Plant Information Module

observeEvent(input$plant_information_module, {
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Plant Information Module - ",input$project_selection_selected)),
                        
                        tabsetPanel(type = "pills", selected = "general_plant_data",
                                    general_plant_data
                        ),
                        easyClose = F, size = "l"
  ))
})

# update fields

# general_plant_data_values <- reactiveValues(id, cycle, phenotype, type, stage, )
# observeEvent(input$plant_information_module, {
#   tb <- paste0(input$project_selection_selected,"_tblFieldTrialPlantID")
#   dt <- tbl(pool, id) %>% collect()
#   updateSelectInput(session, "general_plant_data_FieldTrialPlantID", "", choices = dt$FieldTrialPlantID)
# })
## 2. -------------------------ADMINISTRATION -------------------------------------

source("tabs/server/project_specific/administration.R", local=T)

## 3. -------------------------REPORTS --------------------------------------------


## 4. -----------------------------USER DETAILS --------------------------------------