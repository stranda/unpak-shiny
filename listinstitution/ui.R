source("../global.R")

# Define UI for dataset viewer application
shinyUI(
        navbarPage("Institutions in unPAK database",
                  tabPanel(
                  dataTableOutput("linktable")
                  )
        )
)
