ibbtv_new_transformation <- 
  tabPanel("New Transformation", value = "ibbtv_new_transformation", hr(),
           fluidRow(
             column(12,
                    column(2, selectInput("ibbtv_new_transformation_TransformationType", "Transformation Type", choices = NULL))
                    ),
             column(12,
                    column(2, selectInput("ibbtv_new_transformation_AgrobacteriumStrains", "Agrobacterium Strains", choices = NULL)),
                    column(2, offset = 3, textInput("ibbtv_new_transformation_VirusIndexed", "Virus Indexed")),
                    column(2, numericInput("ibbtv_new_transformation_NumberOfCultures", "Number of Cultures", value = 0))
                    )
           )
    
  )