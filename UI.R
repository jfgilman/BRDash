library(shiny);library(shinydashboard)

### Title:

header <- dashboardHeader(title = "Bayesian Reliability")

### SideBar:
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Proportion Failed", tabName = "PF", icon = icon("fa fa-circle")),
    menuItem("Time to Failure", tabName = "TtF", icon = icon("fa fa-circle")),
    menuItem("About", tabName = "about", icon = icon("fa fa-info-circle"))
  )
)


### Dashboard:
body <- dashboardBody(
  
  
  
  ### Tabintes:
  
  tabItems(
    
    ### TAB 1
    tabItem(tabName = "PF",
            
            fluidRow(
              
              # Sample size slider
              box(width = 4, height = 520,
                  radioButtons('prior', 'Prior', c("Gamma(.001, .001)" = "one",
                                                   "Gamma(0.5, 0.5)" = "half",
                                                   "Gamma(5,5)" = "five",
                                                   "Gamma(10,10)" = "ten",
                                                   "Gamma(0.5,5)" = "pfivefive")),
                  
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
                 textOutput('m'),
                 textOutput('qsl'),
                 textOutput('qsu'))

           )),
           fluidRow(
             box(width = 12,title = "Prediction",
                 solidHeader = T, status = "warning",
                 plotOutput(outputId = "plot2"))
           )
            
           ),
    
    # TAB 2 = dashboard:
    
    tabItem(tabName = "TtF",
            fluidRow(
              
              box(width = 4,
                  radioButtons('prior2', 'Prior', c("Beta(1, 1)" = "one",
                                                   "Beta(0.5, 0.5)" = "half",
                                                   "Beta(10, 10)" = "ten",
                                                   "Beta(2,5)" = "twofive",
                                                   "Beta(5,2)" = "fivetwo")),
                  sliderInput("n", 
                              "Number of observations:", 
                              value = 10,
                              min = 1, 
                              max = 50),
                  sliderInput("s", 
                              "Number of Successes:", 
                              value = 2,
                              min = 0, 
                              max = 50)
              ),
              mainPanel(
                plotOutput('plot3'),
                verbatimTextOutput('summary')
                  
              )
              
            )
    ),
    
    # TAB 3 = About
    tabItem(tabName = "about",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("BayesianTutorial.rmd"))
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)