new_CSC <- 
  tabPanel("New CSC", value = "new_CSC",
         br(), br(), 
         fluidRow(
           column(4,
                  column(5, br(), "Parent Identity"),
                  column(6, selectizeInput("new_CSC_ParentIdentity", "", choices = NULL, multiple = F, width = "100%")),
                  
                  column(5, br(), "Identity"),
                  column(6, textInput("new_CSC_Identity", "", width = "100%")),
                  
                  column(5, br(), "Identity Type"),
                  column(6, textInput("new_CSC_IdentityType", "", width = "100%"))
           ),
           column(1, br(),
                  actionBttn("new_CSC_GetData", "Get Data", size= "xs", style = "fill", color = "primary", block = T),br(),br(),
                  actionBttn("new_CSC_GenerateIdentity", "Generate Identity", size= "xs", style = "fill", color = "primary", block = T)
           ),
           column(1, br(),br(), selectizeInput("new_CSC_Year", "Year", choices = NULL, multiple = F, width = "100%")),
           column(4, offset = 1,
                  conditionalPanel(condition = "input.new_CSC_ReadyToCulture=='Yes'",
                                   column(5, br(), "Number of Cultures"),
                                   column(7, numericInput("new_CSC_NumberOfCultures", "", value = NULL)),
                                   
                                   column(5, br(), "Date of Culture"),
                                   column(7, dateInput("new_CSC_DateOfCultures", "")),
                                   
                                   column(5, br(), "Cultured By"),
                                   column(7, textInput("new_CSC_CulturedBy", "")),
                                   
                                   column(5, br(), "Comments"),
                                   column(7, textAreaInput("new_CSC_Comments1", ""))
                  )
           )
         ),
         fluidRow(
           column(4,
                  column(5, br(), "Cultivar"),
                  column(6, selectizeInput("new_CSC_Cultivar", "", choices = NULL, multiple = F, width = "100%")),
                  
                  column(5, br(), "Cultivar Confirmed"),
                  column(6, selectizeInput("new_CSC_CultivarConfirmed","", choices = c("Yes","No"), multiple = F, width = "100%"))
           ),
           column(6)#, br(), br(),textAreaInput("new_CSC_Comments2", "Comments", width = "100px")
           
         ),
         fluidRow(
           column(4,
                  column(5, br(), "Source"),
                  column(6, selectizeInput("new_CSC_Source", "", choices = NULL, multiple = F)),
                  
                  column(5, br(), "Virus Indexed"),
                  column(6, selectizeInput("new_CSC_VirusIndexed", "", choices = c("Yes", "No"), multiple = F))
           ),
           column(2, offset = 3, actionBttn("new_CSC_SaveStarterCulture", "Save Starter Culture", style = "fill", size = "xs", color = "primary"))
         ),
         fluidRow(
           column(4,
                  column(5, br(),br(), "Permit Type"),
                  column(6, br(),selectizeInput("new_CSC_PermitType1", "", choices = NULL, multiple = F))
           ),
           column(8, 
                  column(2, textInput("new_CSC_PermitType2", "", width = "100%")),
                  column(2, textInput("new_CSC_PermitType3", "", width = "100%")),
                  column(2, textInput("new_CSC_PermitType4", "", width = "100%")),
                  column(2, textInput("new_CSC_PermitType5", "", width = "100%")),
                  column(2, textInput("new_CSC_PermitType6", "", width = "100%"))
           )
         ),
         fluidRow(
           column(4,
                  column(5, br(),br(), "Permit Number"),
                  column(6, br(),numericInput("new_CSC_PermitNumber1", "", value = NULL))
           ),
           column(8, 
                  column(2, textInput("new_CSC_PermitNumber2", "",  width = "100%")),
                  column(2, textInput("new_CSC_PermitNumber3", "",  width = "100%")),
                  column(2, textInput("new_CSC_PermitNumber4", "",  width = "100%")),
                  column(2, textInput("new_CSC_PermitNumber5", "",  width = "100%")),
                  column(2, textInput("new_CSC_PermitNumber6", "",  width = "100%"))
           )
         ),
         fluidRow(
           column(4,
                  column(5, br(), "CSC Initial Culture Date"),
                  column(6, dateInput("new_CSC_CSCInitialCultureDate", "")),
                  
                  column(5, br(), "Media"),
                  column(6, selectizeInput("new_CSC_Media", "", choices = NULL, multiple = F)),
                  
                  column(5, br(), "Address"),
                  column(6, selectizeInput("new_CSC_Address", "", choices = NULL, multiple = T)),
                  
                  column(5, br(), "Lab Book Number"),
                  column(6, numericInput("new_CSC_LabBookNumber", "", value = NULL)),
                  
                  column(5, br(), "Page Number"),
                  column(6, numericInput("new_CSC_PageNumber", "", value = NULL)),
                  
                  column(5, br(), "Ready to Culture?"),
                  column(6, radioGroupButtons(inputId = "new_CSC_ReadyToCulture",label = "",
                                              choices = c("Yes", "No"),individual = TRUE, size ="sm",status = "info",
                                              checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
           ),
           column(3, br(),br(),
                  conditionalPanel(condition = "input.new_CSC_ReadyToCulture=='Yes'",
                                   p("Select the Fields Recurred to be Exported to Excel"),
                                   column(8,
                                          awesomeCheckboxGroup(inputId = "select_fields_to_be_exported_to_excel",label = "",
                                                               choices = NULL, selected = NULL)),
                                   column(4, actionBttn("new_CSC_SelectFieldToExportToExcel", "Export to Excel", style = "fill", size = "xs", color = "primary"))
                                   
                  )
           ),
           column(4)
         ),
         fluidRow(
           column(2, offset = 6,
                  actionBttn("new_CSC_ClearForm", "Clear Form", style = "fill", size = "xs", color = "primary")
           )
           
         )
                                   
                                   
)