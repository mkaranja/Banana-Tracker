login_panel <- bsModalNoClose("window", "Window",
               title="Login using your username",size='small',
               textInput('userName', 'Username'),
               passwordInput('passwd', 'Password'),
               actionButton('login', 'Login', width = "100%",
                            class = 'btn action-button btn-success', 
                            icon = icon('sign-in')
                            ),
               textOutput("pass"),
               tags$head(tags$style("#pass{color: red;")),
               
               tags$head(tags$style("#window .modal-footer{display:none}
                                      .modal-header .close{display:none}"),
                         tags$script("$
                                       $(document).ready(function(){
                                       $('#window').modal();
                                       });
                                       ")
               ))