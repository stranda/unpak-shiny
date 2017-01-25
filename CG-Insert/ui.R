##ui.R
###This file defines the user-interface for the CG-Insert R-Shiny App
####Created by: Hassam Solano-Morel (hassamsolano@gmail.com)

library(shiny)

shinyUI(fluidPage(
  #Add blue theme
  theme = "bootstrap.css",
  # Application title
  titlePanel("Conserved Groups/Insertion Location"),
  
  # Application inputs/outputs
  sidebarLayout(
    sidebarPanel(
      shiny::radioButtons("dataType",
                          label = "Choose the Type of Data",
                          choices = c("Conserved Groups","Insert Location"),
                          selected = "Conserved Groups"), 
      
      shiny::uiOutput('phenotypesBox'),
      shiny::uiOutput('experimentsBox'),
      shiny::checkboxInput(
        inputId = 'log',
        label = "Use Log10 Scale (Y-axis)",
        value = FALSE
      ),
      shiny::checkboxInput(
        inputId = "splitByTreatment",
        label = "Show by Treatment(When Available)",
        value = FALSE
      ),
      radioButtons(
        inputId = "linemeans",
        "Report line means?",
        c("Yes" = "yes",
          "No" = "no"),
        selected = 'no'
      ),
      shiny::radioButtons(
        inputId = "correct",
        label = "Correct for among facility\nvariation using",
        choices = c(
          "Nothing (raw data)" = "none",
          "Means of all plants" = "all",
          "Means of phytometers" = "phyt"
        )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("graph", height = 700))
  )
  
))
