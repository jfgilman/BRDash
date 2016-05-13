library(shiny);library(shinydashboard)

### Title:

header <- dashboardHeader(title = "Bayesian Reliability")

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Failure Probability", tabName = "FP", icon = icon("fa fa-circle")),
    menuItem("Failure Rate", tabName = "FR", icon = icon("fa fa-circle")),
    menuItem("About", tabName = "about", icon = icon("fa fa-info-circle"))
  )
)


### Dashboard:
body <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "FR",
            
            fluidRow(

              box(width = 4, height = 520,
                  radioButtons('prior', 'Prior', c("NonInformative:
                                                   Gamma(.001, .001)" = "NI",
                                                   "Informative:    
                                                   Gamma(3,3)" = "five",
                                                   "Informative:
                                                   Gamma(10,10)" = "ten",
                                                   "Informative:
                                                   Gamma(0.5,5)" = "pfivefive")),
                  
                  wellPanel(
                    selectInput("CD_choice", 'Choose Data Entry Method', 
                                choices = c("Input Summary", "Upload Data"))
                  ),
                  
                  wellPanel(
                    uiOutput("CD_choice")
                  )

              ),

              mainPanel(
                box(width = 12,
                 title = "Plots",
                 solidHeader = TRUE, status = "primary",
                 plotOutput(outputId = "plot1"),
                 checkboxInput("CI", "Show 95% Credible Interval"),
                 textOutput('m'),
                 textOutput('qsl'),
                 textOutput('qsu'))

           )),
           fluidRow(
             box(width = 12,title = "Prediction",
                 solidHeader = T, background = "maroon",collapsible = T,collapsed = T,
                 plotOutput(outputId = "plot2"))
           )
            
           ),
    
    tabItem(tabName = "FP",
            fluidRow(
              
              box(width = 4,
                  radioButtons('prior2', 'Prior', c("NonInformative:
                                                    Beta(1, 1)" = "one",
                                                   "NonInformative:
                                                    Beta(0.5, 0.5)" = "half",
                                                   "Informative:
                                                   Beta(10, 10)" = "ten",
                                                   "Informative:
                                                   Beta(2,5)" = "twofive",
                                                   "Informative:
                                                   Beta(5,2)" = "fivetwo")),
                  
                  wellPanel(
                    selectInput("CD_choice2", 'Choose Data Entry Method', 
                                choices = c("Input Summary", "Upload Data"))
                  ),
                  
                  wellPanel(
                    uiOutput("CD_choice2")
                  )
            ),
              mainPanel(
                box(width = 12,
                    title = "Plots",
                    solidHeader = TRUE, status = "primary",
                    plotOutput('plot3'),
                    checkboxInput("CI2", "Show 95% Credible Interval"),
                    textOutput('m2'),
                    textOutput('qsl2'),
                    textOutput('qsu2'))
                  
              )
              
            )
    ),

    tabItem(tabName = "about",
            fluidPage(
              box(width = 20,status = "success",
                   shiny::includeMarkdown("BTforUITI.rmd"))
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)