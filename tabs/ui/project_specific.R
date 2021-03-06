
tab_files <- list.files(path = "tabs/ui/project_specific", full.names = T, recursive = T)
suppressMessages(lapply(tab_files, source))

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


project_selection <-  
  tabPanel("PROJECT SELECTION", value = "projectselection",
     useShinyjs(),
     div(id = "project_selection_list",
       fluidRow(
         useShinyjs(),  # Set up shinyjs
         br(), br(),
         column(10, offset = 1, align="center",
                uiOutput("select_project_specific_bttn")
         )
       )
     ), br(),
   conditionalPanel(
     condition = "input.project_selected",
     
    fluidRow(
      
      tabsetPanel(type="pills",
        tabPanel("Data Form", value = "select_project_specific_DataForm",
             fluidRow(br(),br(),br(),br(),br(),br(),
                  column(8, 
                         column(4,
                         actionBttn("vector_inventory_module", "Vector Inventory Module", 
                                    style = "unite", color = "primary", size = "md", block = T)),
                         column(4,
                         actionBttn("transformation_module", "Transformation Module", 
                                    style = "unite", color = "primary", size = "md", block = T)),
                         column(4, actionBttn("explant_module", "Explant Module", 
                                              style = "unite", color = "primary", size = "md", block = T)),
                         br(),br(),br(),br(),br(),br(),
                         
                         column(6, 
                                actionBttn("plant_tissue_culture_module", "Plant Tissue Culture Module", 
                                              style = "unite", color = "primary", size = "md", block = T)),
                         column(6, 
                                actionBttn("plant_information_module", "Plant Information Module", 
                             style = "unite", color = "primary", size = "md", block = T)
                         )
                 )
               )  
             ),
        tabPanel("Administration", value = "project_specific_administration",
                 br(), br(), 
                 fluidRow(
                   
                   column(9, align = "center",
                          
                        panel_div(class_type = "default",
                                  content = tags$div(#"Plant Expression Vector Module",
                            column(12,
                                   
                                   column(3,
                                          actionBttn("project_specific_admin_BacterialSelection", label = "Bacterial Selection",  style = "unite", size = "md", color = "primary", block=T), br(), br(),
                                          actionBttn("project_specific_admin_Gene", label = "Gene",  style = "unite", size = "md", color = "primary", block=T)
                                          ),
                                   column(3,
                                          actionBttn("project_specific_admin_PlantSelection", label = "Plant Selection",  style = "unite", size = "md", color = "primary", block=T), br(), br(),
                                          actionBttn("project_specific_admin_Terminator", label = "Terminator",  style = "unite", size = "md", color = "primary", block=T)
                                   ),
                                   column(3,
                                          actionBttn("project_specific_admin_Backbone", label = "Backbone",  style = "unite", size = "md", color = "primary", block=T), br(), br(),
                                          actionBttn("project_specific_admin_Strain", label = "Strain",  style = "unite", size = "md", color = "primary", block=T)
                                   ),
                                   column(3,
                                          actionBttn("project_specific_admin_Promoter", label = "Promoter",  style = "unite", size = "md", color = "primary", block=T), br(), br(),
                                          actionBttn("project_specific_admin_NewFeature", label = "New Feature",  style = "unite", size = "md", color = "primary", block=T),
                                   )
                            )
                            )), br(),br(),
                            panel_div(class_type = "default",
                                content = tags$div(
                                  column(12,
                                         column(4,
                                                actionBttn("project_specific_admin_AgrobacteriumStrains", label = "Agrobacterium Strains",  style = "unite", size = "md", color = "primary", block=T), br(), br(),
                                                actionBttn("project_specific_admin_FieldTrialIdentity", label = "Field Trial Identity",  style = "unite", size = "md", color = "primary", block=T)
                                         ),
                                         column(4,
                                                actionBttn("project_specific_admin_DeploymentLocation", label = "Deployment Location",  style = "unite", size = "md", color = "primary", block=T), br(), br(),
                                                actionBttn("project_specific_admin_Phenotype", label = "Phenotype",  style = "unite", size = "md", color = "primary", block=T)
                                         ),
                                         column(4,
                                                actionBttn("project_specific_admin_GlassHouseTrialID", label = "Glass House Trial ID",  style = "unite", size = "md", color = "primary", block=T), br(), br(),
                                                disabled(actionBttn("project_specific_admin_DIRLicence", label = "DIR Licence",  style = "unite", size = "md", color = "primary", block=T))
                                         )
                              ))
                        ) 
                    )
                   )
                 ),
        tabPanel("Reports", value = "project_specific_reports",
                 column(8, br(), br(),br(),
                
                        fluidRow(
                          
                               column(4, 
                                      actionBttn("transformation_reports", "Transformation Reports", 
                                                 style = "unite", color = "primary", size = "md", block = T)
                               ),
                               
                               column(4, 
                                      actionBttn("plant_tissue_culture_reports", "Plant Tissue Culture Report", 
                                                 style = "unite", color = "primary", size = "md", block = T)
                               ),
                               
                               column(4, 
                                      actionBttn("FT_Number_Reports", "FT Number Report", 
                                                 style = "unite", color = "primary", size = "md", block = T)
                               )
                            ), br(),br(),br(),br(),
                        fluidRow(
                          column(4, 
                                 actionBttn("new_plant_in_plant_tissue_culture_report", "New Plant in Plant Tissue Culture Report", 
                                            style = "unite", color = "primary", size = "md", block = T)
                          ),
                          column(4, 
                                 actionBttn("explant_reports", "Explant Reports", 
                                            style = "unite", color = "primary", size = "md", block = T)
                            ),
                          column(4, 
                                 actionBttn("plant_information_reports", "Plant Information Report", 
                                            style = "unite", color = "primary", size = "md", block = T)
                            )
                          ), br(),br(),br(),br(),
                        fluidRow(
                               
                               column(4, offset = 4,
                                      actionBttn("tracing_module", "Tracing Module", 
                                                 style = "unite", color = "primary", size = "md", block = T)
                               )
                             )
                           )
                      ),
        tabPanel("User Details", value = "project_specific_user_details",
                 
                 fluidRow(
                   
                   column(8, br(), br(),
                          panel_div(class_type = "default",
                                    content = tags$div(
                                      uiOutput("userdetails_ProjectSpecific_Output")
                                    )
                                    
                          )
                   )
                 )
                )
      )
    )
   )
  )

