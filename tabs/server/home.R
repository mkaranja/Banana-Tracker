
observeEvent(input$control_form,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "controlform")
})

observeEvent(input$project_form,{
  updateTabsetPanel(session = session, inputId = "navbar", selected = "projectselection")
})