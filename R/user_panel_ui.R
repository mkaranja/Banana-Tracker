user_panel <- bsModalNoClose("window", "Window",
               title="Log in",size='small',
               
               textInput('username', 'Username'),
               passwordInput('pwInp', 'Password'),
               actionButton('butLogin', 'Login', class = 'btn action-button btn-success', icon = icon('sign-in')),
               
               uiOutput("login_status"),hr(),
               
               tags$head(tags$style("#window .modal-footer{display:none}
                                      .modal-header .close{display:none}"),
                         tags$script("$
                                       $(document).ready(function(){
                                       $('#window').modal();
                                       });
                                       ")
               ))