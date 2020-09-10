

promoter <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblPromoter")) %>% collect()
})

gene <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblGene")) %>% collect()
})

terminator <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblTerminator")) %>% collect()
})
backbone <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblBackbone")) %>% collect()
})
# PLANT TISSUE CULTURE MODILE DATA 
PTC <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblPlantTissueCulture")) %>% collect()
})
deletedPTC <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblDeletedPlantTissueCulture")) %>% collect()
})
culturesPTC <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblCulturesPlantTissueCulture")) %>% collect()
})
# EXPLANT MODULE DATA
explant_data <- reactive({
  tbl(pool, paste0(input$project_selected,"_tblExplant")) %>% collect()
})
explant_val <- reactiveValues()

# TRANSFORMATION DATA
transformation <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblTransformation")) %>% collect()
})
transformation_values <- reactiveValues()

cultureTransformation <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblCulturesTransformation")) %>% collect()
})

deletedTransformation <- reactive({
  tbl(pool, paste0(input$project_selected, "_tblDeletedTransformation")) %>% collect()
})
deletedTransformation_values <- reactiveValues()


## Inventory Module
source("tabs/server/project_specific/data_form/vector_inventory_module.R", local=T)

## Transformation Module
source("tabs/server/project_specific/data_form/transformation_module.R", local=T)

## PLant Tissue Culture Module
source("tabs/server/project_specific/data_form/plant_tissue_culture_module.R", local=T)

## Explant Module
source("tabs/server/project_specific/data_form/explant_module.R", local=T)

## Plant Information Module
source("tabs/server/project_specific/data_form/plant_information_module.R", local=T)

