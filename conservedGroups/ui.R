
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  theme = "bootstrap.css", 

  # Application title
  titlePanel("Conserved Groups"),

  # Application inputs/outputs
  sidebarLayout(
    sidebarPanel(
      shiny::uiOutput('phenotypesBox'),
      shiny::uiOutput('experimentsBox'),
      shiny::checkboxInput(inputId = 'log',
                           label = "Use Log10 Scale",
                           value = FALSE),
      shiny::checkboxInput(inputId = "splitByTreatment",
                           label = "Show by Treatment(When Available)",
                           value = FALSE)
       
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("graph",height = 700)
      
    )
  )
))
