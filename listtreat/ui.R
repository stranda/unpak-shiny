source("../global.R")

# Define UI for dataset viewer application
shinyUI(
        navbarPage("Treatments in unPAK database",
                  tabPanel(
                  dataTableOutput("linktable")
                  )
        )
)
