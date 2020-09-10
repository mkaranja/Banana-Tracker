search_PTC <- 
  tabPanel("Search Plant Tissue Culture", value = "search_PTC", hr(),
           div(id = "searchPTC_form",
             fluidRow(
               column(2, selectInput("searchPTC_ID", "Identity", choices = c(""), multiple = FALSE)),
               column(2, selectInput("searchPTC_VectorID", "Vector ID", choices = c(""), multiple = FALSE)),
               column(1, br(), actionBttn("searchPTC_LoadData", "",icon = icon("arrow-right", lib="font-awesome"), style = "fill", size = "sm", color = "primary")),
               column(2, textInput("searchPTC_PlantSelection", "Plant Selection")),
               column(4, selectInput("searchPTC_PromoterGene", tags$p(style="color:red","Promoter-Gene"), choices = c(""), multiple = FALSE)), # data source not clear (inventory table has separate fields for promoter and gene)
               ),
             fluidRow(
               column(3, dateRangeInput("searchPTC_DateOfStarterCulture", "Date of Starter Culture")),
               column(2),
               column(4,p("Clear The Form Before Every New Search"),
                      column(3, actionBttn("searchPTC_Search","Search", style = "fill", size = "xs", color = "primary", block=T)),
                      column(3, actionBttn("searchPTC_ClearForm","Clear Form", style = "fill", size = "xs", color = "primary", block=T)),
                      column(3, actionBttn("searchPTC_Culture","Culture", style = "fill", size = "xs", color = "primary", block=T)),
                      column(3, actionBttn("searchPTC_DeleteSelected","Delete Selected", style = "fill", size = "xs", color = "primary", block=T))
                      ),
               column(3, br(), awesomeCheckbox(inputId = "search_deletedPTC", label = "Search Deleted Plant Tissue Cultures", status = "info", value = FALSE))# should it be plant tissue culture?
               ),
             fluidRow(
               column(10,br(),
                      uiOutput("searchPTC_Table_Output"), br(),
                      column(8, rHandsontableOutput("searchPTC_Culture_Table")),br(),br(),br(),
                      uiOutput("searchPTC_Culture_Output")
                      )
             )), hr(),
           fluidRow(
             column(6),
             column(6,
                    column(4),
                    column(3, actionBttn("searchPTC_FormToPicture", "Form to Picture", style = "jelly", size = "xs", color = "success", block=T)),
                    column(3, actionBttn("searchPTC_ControlForm", "Control Form", style = "jelly", size = "xs", color = "warning", block=T)),
                    column(2, actionBttn('searchPTC_Exit', "Exit",style = "jelly", size = "xs", color = "danger", block=T))
             )
           )
  ) 