source("../global.R")

# Define UI for dataset viewer application
shinyUI(
        navbarPage("Phenotypes in unPAK database",
                  tabPanel(
                  dataTableOutput("linktable")
                  )
        )
)
