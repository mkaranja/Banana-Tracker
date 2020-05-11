panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

library(shiny)

# Define UI for application that draws a histogram
function(request) {
    fluidPage(
        tagList(
            div(style="padding: 1px 0px; width: '100%'",titlePanel(title="", windowTitle = "Banana Tracker")),
            tags$head(
                tags$script(src = "js/js.cookie.js"),
                tags$style(HTML(".shiny-notification {position:fixed; top: calc(30%); left: calc(50%); } "))#,
               # tags$style(HTML(".form-group {margin-bottom: 0 !important;}"))
            ),
            useShinyjs(), # Include shinyjs in the UI
            
            
            header = tagList(
                extendShinyjs("www/app-shinyjs.js", functions = c("updateHistory")),
                extendShinyjs(text = jsrefresh, functions = c("refresh")),
                shinyjs::extendShinyjs(text = jsResetCode),                      # Add the js code to the page
                extendShinyjs(text = jsprint, functions = c("winprint")),
                extendShinyjs(text = jsclose, functions = c("closeWindow"))
            ),
            tags$style(type = "text/css", ".datepicker{z-index: 1100 !important;}"),
            shinyjs::inlineCSS(appCSS),
            
            # setBackgroundImage(
            #      src = "https://api.time.com/wp-content/uploads/2019/11/gettyimages-459761948.jpg?w=800&quality=85"
            #  ),
            
            ## Display login details 
           
            user_panel, # R/user_panel_ui.R
            
            # Main App
            
            uiOutput("main_app_Output"), # R/main_app.R --------------------- User based
           
            HTML(paste("<script>var parent = document.getElementsByClassName('navbar-nav');
                        parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\"><strong>",
                       uiOutput("out_id"),"</strong></a></li><li class=\"disabled\"><a href=\"#\"><strong>",
                       uiOutput('App_Panel'),"</strong></a></ul>' );</script>"))
            
            )
    
        )
}