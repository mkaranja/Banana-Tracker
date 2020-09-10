

assigned_projects <- reactive({
  username <- input$userName
  user_projects <- tbl(pool, "tblUserInformation") %>% collect() %>%
    dplyr::filter(UserName == username)
  
   pjs <- stringi::stri_remove_empty(c(ifelse(grepl("YES",user_projects$Project1[1])==T, "IBBTV",''),
            ifelse(grepl("YES",user_projects$Project2[1])==T, "IBXW",''),
            ifelse(grepl("YES",user_projects$Project3[1])==T, "IBSV",''),
            ifelse(grepl("YES",user_projects$Project4[1])==T, "Nematode",'')
           ))
   pjs
})

output$select_project_specific_bttn <- renderUI({
  div(br(), br(),
  radioGroupButtons(inputId = "project_selected",label = "", size = "lg",width = "75%",#status = "primary",
    choices = assigned_projects(), individual = TRUE, justified = TRUE, 
    checkIcon = list(yes = icon("ok", lib = "glyphicon"))
  ),br())
})

## 1. ------------DATA FORM---------------------------------------
source("tabs/server/project_specific/data_form.R", local=T)

## 2. ------------ADMINISTRATION -------------------------------------
source("tabs/server/project_specific/administration.R", local=T)

## 3. ------------REPORTS --------------------------------------------
source("tabs/server/project_specific/reports.R", local=T)

## 4. ------------USER DETAILS --------------------------------------
source("tabs/server/project_specific/user_details.R", local=T)
