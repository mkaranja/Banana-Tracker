
existing_users <- reactive({
  usersinfo
})
UV <- reactiveValues()

## ---------------- Add New User ---------------------

# 1. add to db

observeEvent(input$add_new_user_CreateNewUser,{
  req(
    input$add_new_user_FirstName, 
    input$add_new_user_LastName, 
    input$add_new_user_UserName, 
    input$add_new_user_PrivilegeLevel
  )
   
  UV$Data <- existing_users()
  users <- UV$Data
  
  user_exists <- (input$add_new_user_UserName %in% users$UserName)
  
  if(user_exists == T){
    shinyalert("Oops!", "User Name Exists", type = "error")
    updateTextInput(session = session,inputId = "add_new_user_UserName", value = "")
  } 
  #req(validation_result)
  
  if(user_exists == F){
  new_user <- data.frame(FirstName = input$add_new_user_FirstName, 
                         LastName = input$add_new_user_LastName, 
                         UserName = input$add_new_user_UserName,
                         PrivilegeLevel = input$add_new_user_PrivilegeLevel, 
                         Project1 = ifelse(input$add_new_user_Project1==TRUE, "YES", "NO"),
                         Project2 = ifelse(input$add_new_user_Project2==TRUE, "YES", "NO"), 
                         Project3 = ifelse(input$add_new_user_Project3==TRUE, "YES", "NO"),
                         Project4 = ifelse(input$add_new_user_Project4==TRUE, "YES", "NO"),
                         Password = password_store("xyz123")
                         )
  add_new_user <- new_user[!(new_user$UserName %in% users$UserName),]
  
  try(expr = dbWriteTable( conn = pool,   name = "tblUserInformation", value = add_new_user, overwrite = F, append = T))
  shinyalert("Success!", "User added", type = "success")
 # shinyjs::js$refresh()
  }
})



# 2. clear form
observeEvent(input$add_new_user_ClearForm, {
  reset("add_new_user_Form")
})


## ---------- Edit User Information -----------------------

output$edit_user_info_UserName_Output <- renderUI({
  dt <- UV$Data <- existing_users()
  selectInput("edit_user_info_UserName", "User Name", choices = c(dt$UserName), selected = NULL, multiple = F, width = "100%")
})

output$edit_user_info_Details_Output <- renderUI({
  req(input$edit_user_info_UserName)
  
 dt <- UV$Data <- existing_users() %>%
   filter(UserName == input$edit_user_info_UserName)
 dt$PrivilegeLevel <- trimws(dt$PrivilegeLevel)
 
 div(
   textInput("edit_user_info_FirstName", "First Name", value = dt$FirstName),
   textInput("edit_user_info_LastName", "Last Name", value = dt$LastName),
   selectInput("edit_user_info_PrivilegeLevel", "Priviledge Level", choices = c("Admin", "Normal"), selected = dt$PrivilegeLevel)
 )
})

output$edit_user_info_Projects_Output <-renderUI({
  req(input$edit_user_info_UserName)
  
  UV$Data <- existing_users() 
  df <- UV$Data
  dt <- df %>%
    filter(UserName == input$edit_user_info_UserName)
  dt$Project1 <- trimws(dt$Project1); dt$Project2 <- trimws(dt$Project2); dt$Project3 <- trimws(dt$Project3); dt$Project4 <- trimws(dt$Project4)
  
  dt[dt=="NO"] <- FALSE
  dt[dt=="YES"] <- TRUE
  dt$Project1 = as.logical(dt$Project1); dt$Project2 = as.logical(dt$Project2); dt$Project3 = as.logical(dt$Project3); dt$Project4 = as.logical(dt$Project4)
  div(
    awesomeCheckbox(inputId = "edit_user_info_Project1",label = "IBBTV",value = dt$Project1, status = "info"),
    awesomeCheckbox(inputId = "edit_user_info_Project2",label = "IBXW",value = dt$Project2, status = "info"),
    awesomeCheckbox(inputId = "edit_user_info_Project3",label = "IBSV",value = dt$Project3, status = "info"),
    awesomeCheckbox(inputId = "edit_user_info_Project4",label = "Nematode",value = dt$Project4, status = "info")
  )
  
})
## commit changes


observeEvent(input$edit_user_info_UpdateUserInfo,{
  req(input$edit_user_info_UserName)
  
  col1 = "FirstName"; col2 = "LastName"; col3 = "PrivilegeLevel"; col4 = "Project1"; col5 = "Project2"; col6 = "Project3"; col7 = "Project4"
  val1 = input$edit_user_info_FirstName; val2 = input$edit_user_info_LastName; val3 = input$edit_user_info_PrivilegeLevel; val4 = ifelse(input$edit_user_info_Project1 == TRUE, "YES", "NO");
  val5 = ifelse(input$edit_user_info_Project2 == TRUE, "YES", "NO"); val6 = ifelse(input$edit_user_info_Project3 == TRUE, "YES","NO"); val7 = ifelse(input$edit_user_info_Project4 == TRUE, "YES","NO")
  
  id=input$edit_user_info_UserName; tbl <- "tblUserInformation"
  
  query <- glue::glue_sql("UPDATE {`tbl`} SET
                             {`col1`} = {val1}, {`col2`} = {val2}, {`col3`} = {val3}, {`col4`} = {val4}, {`col5`} = {val5}, {`col6`} = {val6}, {`col7`} = {val7}
                             WHERE UserName = {id}
                             ", .con = pool)
    dbExecute(pool, query)
  
  UV$Data <- existing_users()
  UV$Data
  
  shinyjs::js$refresh()
})


observeEvent(input$edit_user_info_DeleteUser,{
  req(input$edit_user_info_UserName)
  id=input$edit_user_info_UserName
  
  del_user <- UV$Data <- existing_users() %>%
    dplyr::filter(UserName == id)
  
  sql <- "DELETE FROM tblUserInformation WHERE UserName = ?id;"
  query <- sqlInterpolate(pool, sql, id = id)
  dbExecute(pool, query)
  
  # Save deleted user details
  
  try(expr = dbWriteTable( conn = pool,   name = "tblDeletedUserInformation", value = del_user, overwrite = F, append = T))
  
  UV$Data <- existing_users()
  UV$Data
  
  shinyjs::js$refresh()
})
 observeEvent(input$edit_user_info_ResetPassword,{
   req(input$edit_user_info_UserName)
   
   userN=input$edit_user_info_UserName
   pwd=password_store("xyz123")
   
   sql <- "UPDATE tblUserInformation SET Password = ?pw WHERE UserName = ?id1;"
   query <- sqlInterpolate(pool, sql, pw = pwd, id1 = userN)
   dbExecute(pool, query)
   
   UV$Data <- existing_users()
   UV$Data
   
   shinyjs::js$refresh()
 })

observeEvent(input$edit_user_info_ClearForm,{
  reset("edit_user_info_Form")
  updateTextInput(session = session,inputId = "edit_user_info_UserName", value = "")
  updateTextInput(session = session,inputId = "edit_user_info_FirstName", value = "")
  updateTextInput(session = session,inputId = "edit_user_info_LastName", value = "")
  updateSelectInput(session = session,inputId = "edit_user_info_PrivilegeLevel", value = "")
  
  updateAwesomeCheckbox(inputId = "edit_user_info_Project1",label = "IBBTV",value = NULL)
  updateAwesomeCheckbox(inputId = "edit_user_info_Project2",label = "IBXW",value = NULL)
  updateAwesomeCheckbox(inputId = "edit_user_info_Project3",label = "IBSV",value = NULL)
  updateAwesomeCheckbox(inputId = "edit_user_info_Project3",label = "Nematode",value = NULL)
})


