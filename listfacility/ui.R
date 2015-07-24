source("../global.R")
# Define UI for dataset viewer application
shinyUI(
  navbarPage("Facilities in unPAK database",
                  tabPanel(
                  dataTableOutput("linktable")
                  )
             )
        )
