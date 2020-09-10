

tab_files <- list.files(path = "tabs/server/project_specific/reports/tracing_module", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

# -----------------TRACING MODULE -----------------------------

observeEvent(input$tracing_module,{
  showModal(modalDialog(tags$h2(style="color:#800000;text-align:center;",paste("Tracing Module - ", input$project_selected)),
                        
                        tabsetPanel(id = "tracing_module_Tabs", type = "pills",
                                    culture_initiation
                        ), easyClose = F, size = "l"
  ))
})


# ---------------------------------- culture_initiation -------------------------------------
active_mfc <- reactive({
  tbl(pool, "tblMFC") %>% collect()
})

output$year<-renderPrint({
  input$tracing_culture_initiation_Get_Year<1
})

observeEvent(input$tracing_culture_initiation_Get_Year,{
  get <- input$tracing_culture_initiation_Get_Year>0
  shinyjs::toggleState(id = "tracing_culture_initiation_ExplantIdentityACTIVE", "", condition = get)
  
  active_explant <- tbl(pool,paste0("Nematode_tblExplant")) %>%
                          collect()
  updateSelectInput(session, "tracing_culture_initiation_ExplantIdentityACTIVE", "", choices = NULL)
  updateSelectInput(session, "tracing_culture_initiation_ExplantIdentityTRASHED", "", choices = NULL)
})