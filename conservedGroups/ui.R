
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
                           label = "Use Log10 Scale (Y-axis)",
                           value = FALSE),
      shiny::checkboxInput(inputId = "splitByTreatment",
                           label = "Show by Treatment(When Available)",
                           value = FALSE),
      radioButtons("linemeans", "Report line means?",
                   c("Yes" = "yes",
                     "No" = "no"), selected = 'no'),
      shiny::radioButtons(inputId = "correct", 
                          label = "Correct for among facility\nvariation using",
                          choices = c("Nothing (raw data)" = "none",
                                      "Means of all plants" = "all",
                                      "Means of phytometers" = "phyt"))
      ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("graph",height = 700)
      
    )
  )
))
