
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

administration <- 
  tabPanel("ADMINISTRATION",# value = "administration",
       br(), br(),
       useShinyjs(),
       useShinyalert(),
       tabsetPanel(type = "pills",
         tabPanel("Edit User Information", value = "edit_user_info",
                  column(8, br(), br(),
                         panel_div(class_type = "default",
                                   content = tags$div(
                                     div(id = "edit_user_info_Form",
                                        fluidRow(
                                           column(4, offset = 1, uiOutput("edit_user_info_UserName_Output")),
                                           column(3, br(), actionBttn("edit_user_info_DisplayUserInfo", "Display User Information", style = "jelly",
                                                                color = "primary", size = "xs"))
                                           ),br(),br(),
                                        fluidRow(
                                             column(4, offset = 1,
                                                    uiOutput("edit_user_info_Details_Output")
                                                    ),
                                             column(4, br(),# br(),br(),
                                                    uiOutput("edit_user_info_Projects_Output")
                                                    )
                                        ), hr(), br(), # user details in the server
                                        fluidRow(
                                           column(10, offset = 1,
                                             column(3,
                                                    actionBttn("edit_user_info_UpdateUserInfo", "Update User Info", color = "primary", style = "jelly", block = T, size="xs")),
                                             column(3,
                                                    actionBttn("edit_user_info_DeleteUser", "Delete User", color = "warning", style = "jelly", block = T, size="xs")),
                                             column(3,
                                                    actionBttn("edit_user_info_ResetPassword", "Reset Password", color = "primary", style = "jelly", block = T, size="xs")),
                                             column(3,
                                                    actionBttn("edit_user_info_ClearForm", "Clear Form", color = "warning", style = "jelly", block = T, size="xs"))
                                           )
                                        )
                                         )
                                     )
                         )
                        )
                ),
         tabPanel("Add New User", value = "add_new_user",
                  
                  fluidRow(
                    column(8, br(), br(),
                           
                           panel_div(class_type = "default",
                              content = tags$div(
                                div(id = "add_new_user_Form",
                                fluidRow(
                                     column(5,offset = 1,  
                                       textInput("add_new_user_FirstName", "First Name"),
                                       textInput("add_new_user_LastName", "Last Name"),
                                       textInput("add_new_user_UserName", "User Name"),
                                       selectizeInput("add_new_user_PrivilegeLevel", "Priviledge Level", choices = c("Admin", "Normal"), selected='Normal', multiple = F)
                                     ),
                                     column(4, ofoffset = 1,
                                            h5("Working Projects"),
                                            awesomeCheckbox(inputId = "add_new_user_Project1",label = "IBBTV",value = FALSE, status = "info"),
                                            awesomeCheckbox(inputId = "add_new_user_Project2",label = "IBXW",value = FALSE, status = "info"),
                                            awesomeCheckbox(inputId = "add_new_user_Project3",label = "IBSV",value = FALSE, status = "info"),
                                            awesomeCheckbox(inputId = "add_new_user_Project4",label = "Nematode",value = FALSE, status = "info")
                                      )
                                     ),hr(), br(),
                                 fluidRow(
                                     column(8, offset = 2,
                                            column(4,
                                                   actionBttn("add_new_user_CreateNewUser", "Create New User", color = "primary", style = "jelly", block = T, size="xs")),
                                            column(2),
                                            column(4,
                                                   actionBttn("add_new_user_ClearForm", "Clear Form", color = "primary", style = "jelly", block = T, size="xs"))
                                     )
                                       
                                ),
                                tableOutput("txt3")
                                )        
                                     
                           )
                    ))
                  )
              )
       )
)
