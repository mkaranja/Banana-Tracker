
status <- reactiveVal(value = NULL)
# check if a cookie is present and matching our super random sessionid  
# observe({
#   js$getcookie()
#   if (!is.null(input$jscookie) && input$jscookie == sessionid) {
#     status(paste0('in with sessionid ', input$jscookie))
#   }
#   else {
#     status('out')
#   }
# })

# observeeven will execute only if butLogin is pressed.
observeEvent(input$butLogin, {
  #browser()  #: for debug mode test
  req(input$username, input$pwInp)  #Make sure username and passowrd are entered#
  query <- sqlInterpolate(pool,"select * from tblUserInformation where UserName=?user;",user=input$username)
  
  user_data <- dbGetQuery(pool,query)
  
  if(nrow(user_data) > 0){ # If the active user is in the DB then logged in
    if(password_verify(user_data[1, "Password"], input$pwInp)){
      
      user$name <- user_data[1, "UserName"]
      loggedIn(TRUE)
      
      # # generate a sessionid and store it in your database
     # js$setcookie(sessionid)
      
      #print(paste("- User:", user$name, "logged in"))
      #removeModal()  ## remove the modal
      toggleModal(session, "window", toggle = "close")
      output$App_Panel <- renderUI({
        span(
          strong(paste(user$name, "|")), 
          actionLink(inputId = "logout", "Logout")
        )
        
      })
    }
  } else {
    loggedIn(FALSE)
    status('out, cause you don\'t know the password secret123 for user admin.')
  } 
})

output$login_status <- renderUI({
  if(input$butLogin == 0){
    return(NULL)
  } else {
    if(!loggedIn()){
      return(span("The Username or Password is Incorrect", style = "color:red"))
    }
  }
})

observeEvent(input$logout, {
  user$name <- NULL
  user$id <- NULL
  loggedIn(FALSE)
  js$reset2()
  #stopApp()
  #print("- User: logged out")
  
  status('out')
  js$rmcookie()
})
##