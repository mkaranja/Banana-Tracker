
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

admin <- tabPanel("Administration", value = "controlform_admin",
                  br(), br(), 
                  fluidRow(
                    
                    column(12, align = "center",br(), br(), br(),br(), br(), br(),
                                       
                           div(style = "text-align: center;", 
                               div(style = "text-align: center;", 
                                  column(12,
                                     column(3,
                                            actionBttn("admin_IdentityType", label = "Identity Type",  style = "unite", size = "md", color = "primary", block=T)),
                                     column(3,
                                            actionBttn("admin_Cultivar", label = "Cultivar",  style = "unite", size = "md", color = "primary", block=T)),
                                     column(3,
                                            actionBttn("admin_Source", label = "Source",  style = "unite", size = "md", color = "primary", block=T)),
                                     column(3,
                                            actionBttn("admin_PermitType", label = "Permit Type",  style = "unite", size = "md", color = "primary", block=T))
                                     ), br(),br(),br(),br(),
                                  column(12,
                                   
                                     column(4,
                                            actionBttn("admin_Media", label = "Media",  style = "unite", size = "md", color = "primary", block=T)),
                                     column(4,
                                            actionBttn("admin_Additives", label = "Additives",  style = "unite", size = "md", color = "primary", block=T)),
                                     column(4,
                                            actionBttn("admin_CulturedBy", label = "Cultured By",  style = "unite", size = "md", color = "primary", block=T))
                                  )
                                   
                               ) 
                                     
                           )
                    )
                  ),
                  ## pop up modals
                  
                  fluidRow(
                    
                    
                  )
)
