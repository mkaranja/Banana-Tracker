


output$userdetails_Output <- renderUI({
  
  cur_user <-  tbl(pool, "tblUserInformation") %>%
    collect %>%
    dplyr::filter(UserName == input$userName)
  
  div(
    fluidRow(
      use_waitress(),
      column(4, offset = 1,
             disabled(textInput("user_details_UserName", "User Name", value = input$userName)),
             disabled(textInput("user_details_FirstName", "First Name", value = cur_user$FirstName)),
             disabled(textInput("user_details_LastName", "Last Name", value = cur_user$LastName)), br(),
      ),
      column(4, br(),br(),br(),
             div(id = "reset_pwd",
               passwordInput("user_details_NewPassword", "New Password"),
               passwordInput("user_details_ConfirmNewPassword", "Confirm New Password")
             ),
             actionBttn("user_details_ChangePassword", "Change Password", color = "primary", style = "jelly", size="xs")
      )
    )
  )
})

waitress <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)


observeEvent(input$user_details_ChangePassword,{
  req(
    input$user_details_NewPassword,
    input$user_details_ConfirmNewPassword
  )
  if((input$user_details_NewPassword == input$user_details_ConfirmNewPassword)){
    pwd <- hashPassword(input$user_details_NewPassword)# password_store
    user <- input$user_details_UserName
    
     # waitress$notify()
     # for(i in 1:10){
     #   waitress$inc(1) # increase by 10%
     #   Sys.sleep(.3)
     # }
    
    sql <- "UPDATE tblUserInformation SET Password = ?pw WHERE UserName = ?id1;"
    query <- sqlInterpolate(pool, sql, pw = pwd, id1 = user)
    dbExecute(pool, query)
    
    shinyalert(title = "Success!", text = "Password has been updated", type = "success")
    reset("reset_pwd")
    # waitress$close() # hide when done
  } else {
    showNotification(tags$h5(style = "font-color:red;","! Passwords must match"))
  }
  
})
# valid_pass <-function(pass1, pass2){
#   if(!(pass1 == pass2)){
#     "Password don't match!"
#   }else{
#     NULL
#   }
# }
# 
# observe(input$user_details_ConfirmNewPassword, {
#   pass1 <- isolate(input$user_details_NewPassword)
#   pass2 <- isolate(input$user_details_ConfirmNewPassword)
#   
#   valid_pass(pass1, pass2)
#   
# })