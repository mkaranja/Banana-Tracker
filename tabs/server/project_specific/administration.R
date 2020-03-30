
## --------------------------------------------Agrobacteriom Strains ----------------------------------------------------

observeEvent(input$project_specific_admin_AgrobacteriumStrains, {
  showModal(
    modalDialog(size = "l",
                  title = tags$h2(style="color:#800000;","Agrobacterium Strains"),
                  useShinyalert(), 
                  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                project_specific_admin_AgrobacteriumStrains_Modal
  ))
})

## reactive values

loadAgrobacteriumStrains <- reactive({
  tb <- paste0(input$project_selection_selected, "_tblAgrobacteriumStrains")
  pool %>% tbl(tb) %>% collect()
})

AgrobacteriumStrainsCV <- reactiveValues()

output$project_specific_admin_AgrobacteriumStrains_Table <- renderRHandsontable({
  dt <- AgrobacteriumStrainsCV$Data <- loadAgrobacteriumStrains()
  rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
    hot_table(stretchH = "all")
})

## update

observeEvent(input$project_specific_admin_AgrobacteriumStrains_Table_select$select$r,{
  
  temp <- isolate(input$project_specific_admin_AgrobacteriumStrains_Table)
  df <- hot_to_r(temp)
  
  r <- input$project_specific_admin_AgrobacteriumStrains_Table_select$select$r
  updateTextInput(session, "project_specific_admin_AgrobacteriumStrains_UpdateAgrobacteriumStrains", label = "", value = df[r, 1])
  updateTextInput(session, "project_specific_admin_AgrobacteriumStrains_UpdateDescription", label = "", value = df[r, 2])
})

 
observeEvent(input$project_specific_admin_AgrobacteriumStrains_Update,{
  temp <- isolate(input$project_specific_admin_AgrobacteriumStrains_Table)
  df <- hot_to_r(temp)
  
  id=input$project_specific_admin_AgrobacteriumStrains_UpdateAgrobacteriumStrains
  col="Description"
  value=input$project_specific_admin_AgrobacteriumStrains_UpdateDescription
  tb <- paste0("IBBTV", "_tblAgrobacteriumStrains")
  
  sql <- paste("UPDATE",tb,"SET Description = ?value WHERE AgrobacteriumStrains = ?id1;")
  query <- sqlInterpolate(pool, sql, value = value, id1 = id)
  dbExecute(pool, query)
  
  AgrobacteriumStrainsCV$Data <- loadAgrobacteriumStrains()
  AgrobacteriumStrainsCV$Data
  shinyjs::js$refresh()
})


 # add new AgrobacteriumStrains
 # 
 # define fields to add

observeEvent(input$project_specific_admin_AgrobacteriumStrains_AddNew,{
  req(input$project_specific_admin_AgrobacteriumStrains_AddNewAgrobacteriumStrains)
  dt <- AgrobacteriumStrainsCV$Data <- loadAgrobacteriumStrains()
  df <- data.frame(AgrobacteriumStrains = input$project_specific_admin_AgrobacteriumStrains_AddNewAgrobacteriumStrains, Description = input$project_specific_admin_AgrobacteriumStrains_AddNewDescription)
  tb <- paste0(input$project_selection_selected,"_tblAgrobacteriumStrains")
  if((df$AgrobacteriumStrains %in% dt$AgrobacteriumStrains)==TRUE){
    shinyalert("Oops!", "AgrobacteriumStrains Exists", type = "error")
  }else {
    dbWriteTable(pool, tb, df, append = T)
    shinyalert("Success!", "Agrobacterium Strains Added", type = "success")
  }
})


observeEvent(input$project_specific_admin_AgrobacteriumStrains_Refresh,{
  AgrobacteriumStrainsCV$Data <- loadAgrobacteriumStrains()
  AgrobacteriumStrainsCV$Data
  shinyjs::js$refresh()
})


observeEvent(input$project_specific_admin_AgrobacteriumStrains_Clear, {
  reset(input$project_specific_admin_AgrobacteriumStrains_Form)
  updateTextInput(session,  inputId = "project_specific_admin_AgrobacteriumStrains_AddNewAgrobacteriumStrains", "Agrobacterium Strains", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_AgrobacteriumStrains_AddNewDescription", "Description", value = "")
  
  updateTextInput(session,  inputId = "project_specific_admin_AgrobacteriumStrains_UpdateAgrobacteriumStrains", "", value = "")
  updateTextInput(session,  inputId = "project_specific_admin_AgrobacteriumStrains_UpdateDescription", "", value = "")
})

observeEvent(input$project_specific_admin_AgrobacteriumStrains_ControlForm,{
  removeModal()
})

observeEvent(input$project_specific_admin_AgrobacteriumStrains_ControlForm,{
  updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
})

#  ------------------------------------------------Backbone ------------------------------------------------------------
 
 observeEvent(input$project_specific_admin_Backbone, {
   showModal(
      modalDialog(size = "l",
             title = tags$h2(style="color:#800000;","Backbone"),
             useShinyalert(), 
             shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
            
              project_specific_admin_Backbone_Modal
    )
   )
 })
 
 
 ## reactive values
 
 loadBackbone <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblBackbone")
   pool %>% tbl(tb) %>% collect()
 })
 
 BackboneCV <- reactiveValues()
 
 output$project_specific_admin_Backbone_Table <- renderRHandsontable({
   dt <- BackboneCV$Data <- loadBackbone()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
##  update
 
 observeEvent(input$project_specific_admin_Backbone_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_Backbone_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_Backbone_Table_select$select$r
   updateTextInput(session, "project_specific_admin_Backbone_UpdateBackbone", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_Backbone_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_Backbone_Update,{
   temp <- isolate(input$project_specific_admin_Backbone_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_Backbone_UpdateBackbone
   col="Description"
   value=input$project_specific_admin_Backbone_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE Backbone = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   BackboneCV$Data <- loadBackbone()
   BackboneCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new Backbone
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_Backbone_AddNew,{
   req(input$project_specific_admin_Backbone_AddNewBackbone)
   dt <- BackboneCV$Data <- loadBackbone()
   df <- data.frame(Backbone = input$project_specific_admin_Backbone_AddNewBackbone, Description = input$project_specific_admin_Backbone_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblBackbone")
   if((df$Backbone %in% dt$Backbone)==TRUE){
     shinyalert("Oops!", "Backbone Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "Backbone Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_Backbone_Refresh,{
   BackboneCV$Data <- loadBackbone()
   BackboneCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_Backbone_Clear, {
   reset(input$project_specific_admin_Backbone_Form)
   updateTextInput(session,  inputId = "project_specific_admin_Backbone_AddNewBackbone", "Backbone", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Backbone_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_Backbone_UpdateBackbone", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Backbone_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_Backbone_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_Backbone_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
 
 ## -----------------------------------------------------------------------------------------------------------------------------------------------------
 ## -------------------------------- Bacterial Selection
 
 observeEvent(input$project_specific_admin_BacterialSelection, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Bacterial Selection"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
                   
                   project_specific_admin_BacterialSelection_Modal
   ))
 })
 
 
 ## reactive values
 
 loadBacterialSelection <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblBacterialSelection")
   pool %>% tbl(tb) %>% collect()
 })
 
 BacterialSelectionCV <- reactiveValues()
 
 output$project_specific_admin_BacterialSelection_Table <- renderRHandsontable({
   dt <- CV$Data <- loadBacterialSelection()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
  ## update
 
 observeEvent(input$project_specific_admin_BacterialSelection_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_BacterialSelection_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_BacterialSelection_Table_select$select$r
   updateTextInput(session, "project_specific_admin_BacterialSelection_UpdateBacterialSelection", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_BacterialSelection_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_BacterialSelection_Update,{
   temp <- isolate(input$project_specific_admin_BacterialSelection_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_BacterialSelection_UpdateBacterialSelection
   col="Description"
   value=input$project_specific_admin_BacterialSelection_UpdateDescription
   
   sql <- paste("UPDATE ",tb," SET Description = ?value WHERE BacterialSelection = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   BacterialSelectionCV$Data <- loadBacterialSelection()
   BacterialSelectionCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new BacterialSelection
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_BacterialSelection_AddNew,{
   req(input$project_specific_admin_BacterialSelection_AddNewBacterialSelection)
   dt <- BacterialSelectionCV$Data <- loadBacterialSelection()
   df <- data.frame(BacterialSelection = input$project_specific_admin_BacterialSelection_AddNewBacterialSelection, Description = input$project_specific_admin_BacterialSelection_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblBacterialSelection")
   if((df$BacterialSelection %in% dt$BacterialSelection)==TRUE){
     shinyalert("Oops!", "BacterialSelection Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "BacterialSelection Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_BacterialSelection_Refresh,{
   BacterialSelectionCV$Data <- loadBacterialSelection()
   BacterialSelectionCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_BacterialSelection_Clear, {
   reset(input$project_specific_admin_BacterialSelection_Form)
   updateTextInput(session,  inputId = "project_specific_admin_BacterialSelection_AddNewBacterialSelection", "Bacterial Selection", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_BacterialSelection_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_BacterialSelection_UpdateBacterialSelection", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_BacterialSelection_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_BacterialSelection_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_BacterialSelection_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
 
  ## --------------------------------------------------------------------------------------------------------------------------------------------------
  ## ------------------------------Deployment Location
 
 observeEvent(input$project_specific_admin_DeploymentLocation, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Deployment Location"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_DeploymentLocation_Modal
   ))
 })
 
 
 ## reactive values
 
 loadDeploymentLocation <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblDeploymentLocation")
   pool %>% tbl(tb) %>% collect()
 })
 
 DeploymentLocationCV <- reactiveValues()
 
 output$project_specific_admin_DeploymentLocation_Table <- renderRHandsontable({
   dt <- DeploymentLocationCV$Data <- loadDeploymentLocation()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
##  update
 
 observeEvent(input$project_specific_admin_DeploymentLocation_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_DeploymentLocation_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_DeploymentLocation_Table_select$select$r
   updateTextInput(session, "project_specific_admin_DeploymentLocation_UpdateDeploymentLocation", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_DeploymentLocation_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_DeploymentLocation_Update,{
   temp <- isolate(input$project_specific_admin_DeploymentLocation_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_DeploymentLocation_UpdateDeploymentLocation
   col="Description"
   value=input$project_specific_admin_DeploymentLocation_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE DeploymentLocation = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   DeploymentLocationCV$Data <- loadDeploymentLocation()
   DeploymentLocationCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new DeploymentLocation
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_DeploymentLocation_AddNew,{
   req(input$project_specific_admin_DeploymentLocation_AddNewDeploymentLocation)
   dt <- DeploymentLocationCV$Data <- loadDeploymentLocation()
   df <- data.frame(DeploymentLocation = input$project_specific_admin_DeploymentLocation_AddNewDeploymentLocation, Description = input$project_specific_admin_DeploymentLocation_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblDeploymentLocation")
   if((df$DeploymentLocation %in% dt$DeploymentLocation)==TRUE){
     shinyalert("Oops!", "DeploymentLocation Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "DeploymentLocation Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_DeploymentLocation_Refresh,{
   DeploymentLocationCV$Data <- loadDeploymentLocation()
   DeploymentLocationCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_DeploymentLocation_Clear, {
   reset(input$project_specific_admin_DeploymentLocation_Form)
   updateTextInput(session,  inputId = "project_specific_admin_DeploymentLocation_AddNewDeploymentLocation", "Deployment Location", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_DeploymentLocation_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_DeploymentLocation_UpdateDeploymentLocation", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_DeploymentLocation_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_DeploymentLocation_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_DeploymentLocation_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------Field Trial Identity----------------------------------------------------
 
 observeEvent(input$project_specific_admin_FieldTrialIdentity, {
   showModal(
     modalDialog(size = "l",
                 title = tags$h2(style="color:#800000;","Field Trial Identity"),
                 useShinyalert(), 
                 shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_FieldTrialIdentity_Modal
   ))
 })
 
 
 # reactive values
 
 loadFieldTrialIdentity <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblFieldTrialIdentity")
   pool %>% tbl(tb) %>% collect()
 })
 
 FieldTrialIdentityCV <- reactiveValues()
 
 output$project_specific_admin_FieldTrialIdentity_Table <- renderRHandsontable({
   dt <- FieldTrialIdentityCV$Data <- loadFieldTrialIdentity()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
#  update
 
 observeEvent(input$project_specific_admin_FieldTrialIdentity_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_FieldTrialIdentity_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_FieldTrialIdentity_Table_select$select$r
   updateTextInput(session, "project_specific_admin_FieldTrialIdentity_UpdateFieldTrialIdentity", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_FieldTrialIdentity_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_FieldTrialIdentity_Update,{
   temp <- isolate(input$project_specific_admin_FieldTrialIdentity_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_FieldTrialIdentity_UpdateFieldTrialIdentity
   col="Description"
   value=input$project_specific_admin_FieldTrialIdentity_UpdateDescription
   
   sql <- paste("UPDATE ",tb," SET Description = ?value WHERE FieldTrialIdentity = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
   dbExecute(pool, query)
   
   FieldTrialIdentityCV$Data <- loadFieldTrialIdentity()
   FieldTrialIdentityCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new FieldTrialIdentity
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_FieldTrialIdentity_AddNew,{
   req(input$project_specific_admin_FieldTrialIdentity_AddNewFieldTrialIdentity)
   dt <- CV$Data <- loadFieldTrialIdentity()
   df <- data.frame(FieldTrialIdentity = input$project_specific_admin_FieldTrialIdentity_AddNewFieldTrialIdentity, Description = input$project_specific_admin_FieldTrialIdentity_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblFieldTrialIdentity")
   if((df$FieldTrialIdentity %in% dt$FieldTrialIdentity)==TRUE){
     shinyalert("Oops!", "FieldTrialIdentity Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "FieldTrialIdentity Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_FieldTrialIdentity_Refresh,{
   FieldTrialIdentityCV$Data <- loadFieldTrialIdentity()
   FieldTrialIdentityCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_FieldTrialIdentity_Clear, {
   reset(input$project_specific_admin_FieldTrialIdentity_Form)
   updateTextInput(session,  inputId = "project_specific_admin_FieldTrialIdentity_AddNewFieldTrialIdentity", "Field Trial Identity", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_FieldTrialIdentity_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_FieldTrialIdentity_UpdateFieldTrialIdentity", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_FieldTrialIdentity_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_FieldTrialIdentity_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_FieldTrialIdentity_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------Gene----------------------------------------------------
 
 observeEvent(input$project_specific_admin_Gene, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Gene"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_Gene_Modal
   ))
 })
 
 
#  reactive values
 
 loadGene <- reactive({
   tb <- tb <- paste0(input$project_selection_selected, "_tblGene")
   pool %>% tbl(tb) %>% collect()
 })
 
 GeneCV <- reactiveValues()
 
 output$project_specific_admin_Gene_Table <- renderRHandsontable({
   dt <- GeneCV$Data <- loadGene()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
 # update
 
 observeEvent(input$project_specific_admin_Gene_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_Gene_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_Gene_Table_select$select$r
   updateTextInput(session, "project_specific_admin_Gene_UpdateGene", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_Gene_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_Gene_Update,{
   temp <- isolate(input$project_specific_admin_Gene_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_Gene_UpdateGene
   col="Description"
   value=input$project_specific_admin_Gene_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE Gene = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   GeneCV$Data <- loadGene()
   GeneCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new Gene
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_Gene_AddNew,{
   req(input$project_specific_admin_Gene_AddNewGene)
   dt <- GeneCV$Data <- loadGene()
   df <- data.frame(Gene = input$project_specific_admin_Gene_AddNewGene, Description = input$project_specific_admin_Gene_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblGene")
   if((df$Gene %in% dt$Gene)==TRUE){
     shinyalert("Oops!", "Gene Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "Gene Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_Gene_Refresh,{
   GeneCV$Data <- loadGene()
   GeneCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_Gene_Clear, {
   reset(input$project_specific_admin_Gene_Form)
   updateTextInput(session,  inputId = "project_specific_admin_Gene_AddNewGene", "Gene", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Gene_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_Gene_UpdateGene", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Gene_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_Gene_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_Gene_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------GlassHouseTrialID----------------------------------------------------
 
 observeEvent(input$project_specific_admin_GlassHouseTrialID, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Glass House Trial ID"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_GlassHouseTrialID_Modal
     )
   )
 })
 
 
 # reactive values
 
 loadGlassHouseTrialID <- reactive({
   tb <- tb <- paste0(input$project_selection_selected, "_tblGlassHouseTrialID")
   pool %>% tbl(tb) %>% collect()
 })
 
 GlassHouseTrialIDCV <- reactiveValues()
 
 output$project_specific_admin_GlassHouseTrialID_Table <- renderRHandsontable({
   dt <- GlassHouseTrialIDCV$Data <- loadGlassHouseTrialID()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
 # update
 
 observeEvent(input$project_specific_admin_GlassHouseTrialID_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_GlassHouseTrialID_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_GlassHouseTrialID_Table_select$select$r
   updateTextInput(session, "project_specific_admin_GlassHouseTrialID_UpdateGlassHouseTrialID", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_GlassHouseTrialID_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_GlassHouseTrialID_Update,{
   temp <- isolate(input$project_specific_admin_GlassHouseTrialID_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_GlassHouseTrialID_UpdateGlassHouseTrialID
   col="Description"
   value=input$project_specific_admin_GlassHouseTrialID_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE GlassHouseTrialID = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   GlassHouseTrialIDCV$Data <- loadGlassHouseTrialID()
   GlassHouseTrialIDCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new GlassHouseTrialID
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_GlassHouseTrialID_AddNew,{
   req(input$project_specific_admin_GlassHouseTrialID_AddNewGlassHouseTrialID)
   dt <- GlassHouseTrialIDCV$Data <- loadGlassHouseTrialID()
   df <- data.frame(GlassHouseTrialID = input$project_specific_admin_GlassHouseTrialID_AddNewGlassHouseTrialID, Description = input$project_specific_admin_GlassHouseTrialID_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblGlassHouseTrialID")
   if((df$GlassHouseTrialID %in% dt$GlassHouseTrialID)==TRUE){
     shinyalert("Oops!", "GlassHouseTrialID Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "GlassHouseTrialID Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_GlassHouseTrialID_Refresh,{
   GlassHouseTrialIDCV$Data <- loadGlassHouseTrialID()
   GlassHouseTrialIDCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_GlassHouseTrialID_Clear, {
   reset(input$project_specific_admin_GlassHouseTrialID_Form)
   updateTextInput(session,  inputId = "project_specific_admin_GlassHouseTrialID_AddNewGlassHouseTrialID", "Glass House Trial ID", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_GlassHouseTrialID_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_GlassHouseTrialID_UpdateGlassHouseTrialID", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_GlassHouseTrialID_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_GlassHouseTrialID_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_GlassHouseTrialID_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------NewFeature----------------------------------------------------
 
 observeEvent(input$project_specific_admin_NewFeature, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","New feature"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_NewFeature_Modal
   ))
 })
 
 
 # reactive values
 
 loadNewFeature <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblNewFeature")
   pool %>% tbl(tb) %>% collect()
 })
 
 NewFeatureCV <- reactiveValues()
 
 output$project_specific_admin_NewFeature_Table <- renderRHandsontable({
   dt <- NewFeatureCV$Data <- loadNewFeature()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
 ## update
 
 observeEvent(input$project_specific_admin_NewFeature_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_NewFeature_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_NewFeature_Table_select$select$r
   updateTextInput(session, "project_specific_admin_NewFeature_UpdateNewFeature", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_NewFeature_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_NewFeature_Update,{
   temp <- isolate(input$project_specific_admin_NewFeature_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_NewFeature_UpdateNewFeature
   col="Description"
   value=input$project_specific_admin_NewFeature_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE NewFeature = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   NewFeatureCV$Data <- loadNewFeature()
   NewFeatureCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new NewFeature
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_NewFeature_AddNew,{
   req(input$project_specific_admin_NewFeature_AddNewNewFeature)
   dt <- NewFeatureCV$Data <- loadNewFeature()
   df <- data.frame(NewFeature = input$project_specific_admin_NewFeature_AddNewNewFeature, Description = input$project_specific_admin_NewFeature_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblNewFeature")
   if((df$NewFeature %in% dt$NewFeature)==TRUE){
     shinyalert("Oops!", "NewFeature Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "NewFeature Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_NewFeature_Refresh,{
   NewFeatureCV$Data <- loadNewFeature()
   NewFeatureCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_NewFeature_Clear, {
   reset(input$project_specific_admin_NewFeature_Form)
   updateTextInput(session,  inputId = "project_specific_admin_NewFeature_AddNewNewFeature", "New Feature", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_NewFeature_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_NewFeature_UpdateNewFeature", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_NewFeature_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_NewFeature_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_NewFeature_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------Phenotype----------------------------------------------------
 
 observeEvent(input$project_specific_admin_Phenotype, {
   showModal(
       modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Phenotype"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_Phenotype_Modal
   ))
 })
 
 
#  reactive values
 
 loadPhenotype <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblPhenotype")
   pool %>% tbl(tb) %>% collect()
 })
 
 PhenotypeCV <- reactiveValues()
 
 output$project_specific_admin_Phenotype_Table <- renderRHandsontable({
   dt <- PhenotypeCV$Data <- loadPhenotype()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
#  update
 
 observeEvent(input$project_specific_admin_Phenotype_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_Phenotype_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_Phenotype_Table_select$select$r
   updateTextInput(session, "project_specific_admin_Phenotype_UpdatePhenotype", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_Phenotype_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_Phenotype_Update,{
   temp <- isolate(input$project_specific_admin_Phenotype_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_Phenotype_UpdatePhenotype
   col="Description"
   value=input$project_specific_admin_Phenotype_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE Phenotype = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   PhenotypeCV$Data <- loadPhenotype()
   PhenotypeCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new Phenotype
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_Phenotype_AddNew,{
   req(input$project_specific_admin_Phenotype_AddNewPhenotype)
   dt <- PhenotypeCV$Data <- loadPhenotype()
   df <- data.frame(Phenotype = input$project_specific_admin_Phenotype_AddNewPhenotype, Description = input$project_specific_admin_Phenotype_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblPhenotype")
   if((df$Phenotype %in% dt$Phenotype)==TRUE){
     shinyalert("Oops!", "Phenotype Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "Phenotype Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_Phenotype_Refresh,{
   PhenotypeCV$Data <- loadPhenotype()
   PhenotypeCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_Phenotype_Clear, {
   reset(input$project_specific_admin_Phenotype_Form)
   updateTextInput(session,  inputId = "project_specific_admin_Phenotype_AddNewPhenotype", "Phenotype", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Phenotype_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_Phenotype_UpdatePhenotype", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Phenotype_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_Phenotype_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_Phenotype_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------PlantSelection----------------------------------------------------
 
 observeEvent(input$project_specific_admin_PlantSelection, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Plant Selection"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_PlantSelection_Modal
   ))
 })
 
 
 # reactive values
 
 loadPlantSelection <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblPlantSelection")
   pool %>% tbl(tb) %>% collect()
 })
 
 PlantSelectionCV <- reactiveValues()
 
 output$project_specific_admin_PlantSelection_Table <- renderRHandsontable({
   dt <- PlantSelectionCV$Data <- loadPlantSelection()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
  update
 
 observeEvent(input$project_specific_admin_PlantSelection_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_PlantSelection_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_PlantSelection_Table_select$select$r
   updateTextInput(session, "project_specific_admin_PlantSelection_UpdatePlantSelection", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_PlantSelection_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_PlantSelection_Update,{
   temp <- isolate(input$project_specific_admin_PlantSelection_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_PlantSelection_UpdatePlantSelection
   col="Description"
   value=input$project_specific_admin_PlantSelection_UpdateDescription
   
   sql <- paste("UPDATE ",tb," SET Description = ?value WHERE PlantSelection = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   PlantSelectionCV$Data <- loadPlantSelection()
   PlantSelectionCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new PlantSelection
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_PlantSelection_AddNew,{
   req(input$project_specific_admin_PlantSelection_AddNewPlantSelection)
   dt <- PlantSelectionCV$Data <- loadPlantSelection()
   df <- data.frame(PlantSelection = input$project_specific_admin_PlantSelection_AddNewPlantSelection, Description = input$project_specific_admin_PlantSelection_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblPlantSelection")
   if((df$PlantSelection %in% dt$PlantSelection)==TRUE){
     shinyalert("Oops!", "PlantSelection Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "PlantSelection Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_PlantSelection_Refresh,{
   PlantSelectionCV$Data <- loadPlantSelection()
   PlantSelectionCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_PlantSelection_Clear, {
   reset(input$project_specific_admin_PlantSelection_Form)
   updateTextInput(session,  inputId = "project_specific_admin_PlantSelection_AddNewPlantSelection", "Plant Selection", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_PlantSelection_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_PlantSelection_UpdatePlantSelection", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_PlantSelection_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_PlantSelection_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_PlantSelection_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------Promoter----------------------------------------------------
 
 observeEvent(input$project_specific_admin_Promoter, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Promoter"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_Promoter_Modal
   ))
 })
 
 
 # reactive values
 
 loadPromoter <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblPromoter")
   pool %>% tbl(tb) %>% collect()
 })
 
 PromoterCV <- reactiveValues()
 
 output$project_specific_admin_Promoter_Table <- renderRHandsontable({
   dt <- PromoterCV$Data <- loadPromoter()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
#  update
 
 observeEvent(input$project_specific_admin_Promoter_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_Promoter_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_Promoter_Table_select$select$r
   updateTextInput(session, "project_specific_admin_Promoter_UpdatePromoter", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_Promoter_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_Promoter_Update,{
   temp <- isolate(input$project_specific_admin_Promoter_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_Promoter_UpdatePromoter
   col="Description"
   value=input$project_specific_admin_Promoter_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE Promoter = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   PromoterCV$Data <- loadPromoter()
   PromoterCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new Promoter
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_Promoter_AddNew,{
   req(input$project_specific_admin_Promoter_AddNewPromoter)
   dt <- PromoterCV$Data <- loadPromoter()
   df <- data.frame(Promoter = input$project_specific_admin_Promoter_AddNewPromoter, Description = input$project_specific_admin_Promoter_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblPromoter")
   if((df$Promoter %in% dt$Promoter)==TRUE){
     shinyalert("Oops!", "Promoter Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "Promoter Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_Promoter_Refresh,{
   PromoterCV$Data <- loadPromoter()
   PromoterCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_Promoter_Clear, {
   reset(input$project_specific_admin_Promoter_Form)
   updateTextInput(session,  inputId = "project_specific_admin_Promoter_AddNewPromoter", "Promoter", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Promoter_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_Promoter_UpdatePromoter", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Promoter_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_Promoter_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_Promoter_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------Strains----------------------------------------------------
 
 observeEvent(input$project_specific_admin_Strains, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Strain"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_Strains_Modal
   ))
 })
 
 
#  reactive values
 
 loadStrains <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblStrains")
   pool %>% tbl(tb) %>% collect()
 })
 
 StrainsCV <- reactiveValues()
 
 output$project_specific_admin_Strains_Table <- renderRHandsontable({
   dt <- StrainsCV$Data <- loadStrains()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
#  update
 
 observeEvent(input$project_specific_admin_Strains_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_Strains_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_Strains_Table_select$select$r
   updateTextInput(session, "project_specific_admin_Strains_UpdateStrains", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_Strains_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_Strains_Update,{
   temp <- isolate(input$project_specific_admin_Strains_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_Strains_UpdateStrains
   col="Description"
   value=input$project_specific_admin_Strains_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value WHERE Strains = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id1 = id)
   dbExecute(pool, query)
   
   StrainsCV$Data <- loadStrains()
   StrainsCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new Strains
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_Strains_AddNew,{
   req(input$project_specific_admin_Strains_AddNewStrains)
   dt <- StrainsCV$Data <- loadStrains()
   df <- data.frame(Strains = input$project_specific_admin_Strains_AddNewStrains, Description = input$project_specific_admin_Strains_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblStrains")
   if((df$Strains %in% dt$Strains)==TRUE){
     shinyalert("Oops!", "Strains Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "Strains Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_Strains_Refresh,{
   StrainsCV$Data <- loadStrains()
   StrainsCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_Strains_Clear, {
   reset(input$project_specific_admin_Strains_Form)
   updateTextInput(session,  inputId = "project_specific_admin_Strains_AddNewStrains", "Strains", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Strains_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_Strains_UpdateStrains", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Strains_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_Strains_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_Strains_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
  # -------------------------------------------------------------------------------------------------------------------------------
  # --------------------------------------------Terminator----------------------------------------------------
 
 observeEvent(input$project_specific_admin_Terminator, {
   showModal(
     modalDialog(size = "l",
                   title = tags$h2(style="color:#800000;","Terminator"),
                   useShinyalert(), 
                   shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
     project_specific_admin_Terminator_Modal
   ))
 })
 
 
#  reactive values
 
 loadTerminator <- reactive({
   tb <- paste0(input$project_selection_selected, "_tblTerminator")
   pool %>% tbl(tb) %>% collect()
 })
 
 TerminatorCV <- reactiveValues()
 
 output$project_specific_admin_Terminator_Table <- renderRHandsontable({
   dt <- TerminatorCV$Data <- loadTerminator()
   rhandsontable(dt, selectCallback = TRUE, readOnly = FALSE, rowHeaders=F) %>%
     hot_table(stretchH = "all")
 })
 
 
#  update
 
 observeEvent(input$project_specific_admin_Terminator_Table_select$select$r,{
   
   temp <- isolate(input$project_specific_admin_Terminator_Table)
   df <- hot_to_r(temp)
   
   r <- input$project_specific_admin_Terminator_Table_select$select$r
   updateTextInput(session, "project_specific_admin_Terminator_UpdateTerminator", label = "", value = df[r, 1])
   updateTextInput(session, "project_specific_admin_Terminator_UpdateDescription", label = "", value = df[r, 2])
 })
 
 
  
 observeEvent(input$project_specific_admin_Terminator_Update,{
   temp <- isolate(input$project_specific_admin_Terminator_Table)
   df <- hot_to_r(temp)
   
   id=input$project_specific_admin_Terminator_UpdateTerminator
   col="Description"
   value=input$project_specific_admin_Terminator_UpdateDescription
   
   sql <- paste("UPDATE",tb,"SET Description = ?value, Terminator = ?id2 WHERE Terminator = ?id1;")
   query <- sqlInterpolate(pool, sql, value = value, id2 = id, id1 = id)
   dbExecute(pool, query)
   
   TerminatorCV$Data <- loadTerminator()
   TerminatorCV$Data
   shinyjs::js$refresh()
 })
 
 
  # add new Terminator
  # 
  # define fields to add
 
 observeEvent(input$project_specific_admin_Terminator_AddNew,{
   req(input$project_specific_admin_Terminator_AddNewTerminator)
   dt <- TerminatorCV$Data <- loadTerminator()
   df <- data.frame(Terminator = input$project_specific_admin_Terminator_AddNewTerminator, Description = input$project_specific_admin_Terminator_AddNewDescription)
   tb <- paste0(input$project_selection_selected,"_tblTerminator")
   if((df$Terminator %in% dt$Terminator)==TRUE){
     shinyalert("Oops!", "Terminator Exists", type = "error")
   }else {
     dbWriteTable(pool, tb, df, append = T)
     shinyalert("Success!", "Terminator Added", type = "success")
   }
 })
 
 
 observeEvent(input$project_specific_admin_Terminator_Refresh,{
   TerminatorCV$Data <- loadTerminator()
   TerminatorCV$Data
   shinyjs::js$refresh()
 })
 
 
 observeEvent(input$project_specific_admin_Terminator_Clear, {
   reset(input$project_specific_admin_Terminator_Form)
   updateTextInput(session,  inputId = "project_specific_admin_Terminator_AddNewTerminator", "Terminator", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Terminator_AddNewDescription", "Description", value = "")
   
   updateTextInput(session,  inputId = "project_specific_admin_Terminator_UpdateTerminator", "", value = "")
   updateTextInput(session,  inputId = "project_specific_admin_Terminator_UpdateDescription", "", value = "")
 })
 
 observeEvent(input$project_specific_admin_Terminator_ControlForm,{
   removeModal()
 })
 
 observeEvent(input$project_specific_admin_Terminator_ControlForm,{
   updateTabsetPanel(session, "controlform_Tabs", selected = "controlform_dataform")
 })
 
