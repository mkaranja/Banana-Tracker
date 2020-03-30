# -----------------VECTOR INVENTORY MODULE -----------------------------
## 1. Vector Inventory 1

# clear all tabs

observeEvent(input$vector_inventory_1_ClearAllTheTabs,{
  
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset("vector_inventory_1_form")
        reset("vector_inventory_2_form")
        reset("vector_inventory_3_form")
        reset("vector_inventory_4_form")
        reset("vector_inventory_5_form")
        reset("vector_inventory_search_form")
    },
    text = "Do you really want to clear all tabs?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})


observeEvent(input$project_selection_selected,{
  vectorid <- paste0("p", input$project_selection_selected)
  updateTextInput(session, "vector_inventory_1_VectorID","Vector ID", value = )
  updateTextInput(session, "vector_inventory_1_VectorPrefix", "Vector Prefix", value = paste0("p", input$project_selection_selected))
})

# clear current tab

observeEvent(input$vector_inventory_1_Clear,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset("vector_inventory_1_form")
    },
    text = "Do you really want to clear the fields?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})

## 2. Vector Inventory 2

observeEvent(input$vector_inventory_2_Clear,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
      reset("vector_inventory_2_form")
    },
    text = "Do you really want to clear the fields?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})

## 3. Vector Inventory 3


## 4. Vector Inventory 4

## 5. Vector Inventory 5

## 6. Search

observeEvent(input$vector_inventory_module,{
  # Vector ID
  #id <- tbl(pool, "IBBTV_tblVectorInventory") %>% collect()
  promoter_dt <- tbl(pool, paste0(input$project_selection_selected, "_tblPromoter")) %>% collect()
  gene_dt <- tbl(pool, paste0(input$project_selection_selected, "_tblGene")) %>% collect()
  terminator_dt <- tbl(pool, paste0(input$project_selection_selected, "_tblTerminator")) %>% collect()
  backbone_dt <- tbl(pool, paste0(input$project_selection_selected, "_tblBackbone")) %>% collect()
  #clonedby_dt <- tbl(pool, paste0(input$project_selection_selected, "_tblCloneBy")) %>% collect()
  
  updateSelectInput(session, "vector_inventory_search_VectorID", "Vector ID",choices = NULL)
  updateSelectInput(session, "vector_inventory_search_Promoter", "Promoter", choices = c('', promoter_dt$Promoter))
  updateSelectInput(session, "vector_inventory_search_Gene", "Gene", choices = c('', gene_dt$Gene))
  updateSelectInput(session, "vector_inventory_search_Terminator", "Terminator", choices = c('',terminator_dt$Terminator))
  updateSelectInput(session, "vector_inventory_search_Backbone", "Backbone", choices = c('', backbone_dt$Backbone))
  updateSelectInput(session, "vector_inventory_search_ClonedBy", "Cloned By", choices = NULL)
  
})

vector_inventory_search_input <- reactive({
  
})

# Clear all selections

observeEvent(input$vector_inventory_search_ClearForm,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        reset("vector_inventory_search_form")
    },
    text = "Do you really want to clear the fields?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
  
})

# Back to Control Form

observeEvent(input$vector_inventory_search_ControlForm,{
  
})
# ..... Exit 

observeEvent(input$vector_inventory_search_Exit,{
  shinyalert(
    title = "",
    callbackR = function(x) {
      if(x != FALSE)
        p("Close app") ##session$stopApp()
    },
    text = "Do you really want to close the app?",
    type = "warning",
    showCancelButton = TRUE,
    confirmButtonCol = '#DD6B55',
    confirmButtonText = 'Yes, clear!'
  )
})

tbl(pool, "IBBTV_tblVectorInventory")
sql <- "SELECT * FROM IBBTV_tblVectorInventory;"
query <- sqlInterpolate(pool, sql)
dbExecute(pool, query)


# 0774, 774, 774
# 0733, 923, 703