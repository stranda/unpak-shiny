source("../global.R")
# Define UI for dataset viewer application
shinyUI(
  navbarPage("Experiments in unPAK database",
             tabPanel(
                  dataTableOutput("linktable")
                  )
        )
)