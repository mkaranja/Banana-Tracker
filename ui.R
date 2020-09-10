
shinyUI(fluidPage(
  fluidPage(
    tagList(
      div(style="padding: 1px 0px; width: '100%'",titlePanel(title="", windowTitle = "Banana Tracker")),
      tags$head(
        tags$script(src = "js/js.cookie.js"),
        tags$style(HTML(".shiny-notification {position:fixed; top: calc(30%); left: calc(50%); } "))
      ),
      useShinyjs(), # Include shinyjs in the UI
      extendShinyjs(text = jsCode),
      header = tagList(
        extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory")),
        extendShinyjs(text = jsrefresh, functions = c("refresh")),
        shinyjs::extendShinyjs(text = jsResetCode),
        extendShinyjs(text = jsprint, functions = c("winprint")),
        extendShinyjs(text = jsclose, functions = c("closeWindow"))
      ),
      
      tags$style(type = "text/css", ".datepicker{z-index: 1100 !important;}"),
      #shinyjs::inlineCSS(appCSS),
      
      login_panel,
      uiOutput("mainApp"),
      
      HTML(paste("<script>var parent = document.getElementsByClassName('navbar-nav');
           parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\"><strong>",
                 uiOutput("logged_panel"),"</strong></a></li></ul>' );</script>"))
    
    )
  )
))