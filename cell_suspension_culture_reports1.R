
# load cultures table

cell_suspension_culture_reports_Input <- reactive({
   
   dt <- tbl(pool,"tblCSC") %>% 
      left_join(tbl(pool,"tblCulturesCSC")) %>%
      collect() %>%
       select(ExplantIdentity, CSCIdentity, IdentityType, Cultivar, Source, DateOfStarterCulture, Media, Additives, 
              LabBookNumber, PageNumber, NumberOfCultures, DateOfCulture, CulturedBy, MediaForCultures, AdditivesForCultures, 
              LabBookNumberForCultures, PageNumberForCultures, Comments)
   
   dt[,grep("Date", names(dt), value = T)] %<>% mutate_all(lubridate::date)
   
    if(input$cell_suspension_culture_reports_ExplantIdentify !=''){
       dt <- dt %>%
          filter(ExplantIdentity == input$cell_suspension_culture_reports_ExplantIdentify)
    }
    
    if(input$cell_suspension_culture_reports_Source !=''){
       dt <- dt %>%
          filter(Source == input$cell_suspension_culture_reports_Source)
    }
    
    if(input$cell_suspension_culture_reports_Cultivar !=''){
       dt <- dt %>%
          filter(Cultivar == input$cell_suspension_culture_reports_Cultivar)
    }
    
    if(input$cell_suspension_culture_reports_CSCIdentity !=''){
       dt <- dt %>%
          filter(CSCIdentity == input$cell_suspension_culture_reports_CSCIdentity)
    }
    
     if(!is.na(input$cell_suspension_culture_reports_DateOfStarterCulture)){
        dt <- dt %>%
           filter(between(lubridate::ymd(DateOfStarterCulture), 
                          lubridate::ymd(input$cell_suspension_culture_reports_DateOfStarterCulture[1]),
                          lubridate::ymd(input$cell_suspension_culture_reports_DateOfStarterCulture[2])))
     }
     
     if(!is.na(input$cell_suspension_culture_reports_DateOfSubCulture)){
        dt <- dt %>%
           filter(between(lubridate::ymd(DateOfCulture), 
                          lubridate::ymd(input$cell_suspension_culture_reports_DateOfSubCulture[1]), 
                          lubridate::ymd(input$cell_suspension_culture_reports_DateOfSubCulture[2])))
     }
    
    if(input$cell_suspension_culture_reports_CulturedBy !=''){
       dt <- dt %>%
          filter(CulturedBy == input$cell_suspension_culture_reports_CulturedBy)
    }
   dt
})

# Select Fields
output$cell_suspension_culture_reports_SelectTheFields_Output <- renderUI({
   panel_div(class_type = "default",
             content = tags$div(
                prettyCheckboxGroup(inputId = "cell_suspension_culture_reports_SelectTheFields", label = "Select the Fields", 
                                    choices = c(names(cell_suspension_culture_reports_Input())), status = "info", icon = icon("check"))
             ))
})

# Filter based on selected fields
cell_suspension_culture_reports_Fields_Selected_Input <- reactive({
   
   if (is.null(input$cell_suspension_culture_reports_SelectTheFields)){
      data.frame()
   } else {
      cell_suspension_culture_reports_Input() %>%
         dplyr::select(!!!input$cell_suspension_culture_reports_SelectTheFields)
   }
})

# show data table
observeEvent(input$cell_suspension_culture_reports_LoadData,{
   if(is.null(input$cell_suspension_culture_reports_SelectTheFields)){
      showNotification("Select atleast one field on the left", type = "error")
   }else {
      output$cell_suspension_culture_reports_ResultsTable <- renderRHandsontable({
         dt <- cell_suspension_culture_reports_Fields_Selected_Input()
         rhandsontable(dt)
      })
   }
})



# Export To Excel

output$cell_suspension_culture_reports_ExportToExcel <- downloadHandler(
   filename = function(){paste('Cell Suspension Culture Report-', Sys.time(), '.csv')},
   content = function(file) {
      write.csv(cell_suspension_culture_reports_Fields_Selected_Input(), file, row.names = F)
   }
)

# Clear Reports

observeEvent(input$cell_suspension_culture_reports_Clear,{
   reset('cell_suspension_culture_reports_Form')
   updatePrettyCheckboxGroup(session = session, inputId = "cell_suspension_culture_reports_SelectTheFields", selected = NULL)
}, ignoreNULL = FALSE)


