USER <- reactiveValues(Logged = FALSE , session = session$user) 
status <- reactiveVal(value = NULL)

# check if a cookie is present and matching our super random sessionid  
observe({
  js$getcookie()
  if (!is.null(input$jscookie) && input$jscookie == sessionid) {
    status(paste0('in with sessionid ', input$jscookie))
  }
  else {
    status('out')
  }
})

# control login
observeEvent(input$login , {
  Username <- isolate(input$userName)
  Password <- isolate(input$passwd)
  query <- sqlInterpolate(pool,"select * from tblUserInformation where UserName=?user;", user=input$userName)
  userInfo <- dbGetQuery(pool,query)
  
  if (nrow(userInfo)>0) {
    if(verifyPassword(userInfo[1, "Password"], input$passwd)){
      toggleModal(session, "window", toggle = "close")
      USER$Logged <- TRUE
      USER$name <- Username 
     
      js$setcookie(sessionid)
    } 
  } else {
    USER$pass <- "The Username or Password is Incorrect!"
    status('out')
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    USER$pass
  }  
})

# control logout
observeEvent(input$logout , {
  USER$Logged <- FALSE
  USER$pass <- ""
  status('out')
  js$rmcookie()
})

observe({
  js$getcookie()
  if (!is.null(input$jscookie) && input$jscookie == sessionid) {
    status(paste0('in with sessionid ', input$jscookie))
  }
  else {
    status('out')
  }
})