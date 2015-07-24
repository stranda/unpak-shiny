source("../global.R")


# Define UI for dataset viewer application
shinyUI(
#pageWithSidebar(
  navbarPage("Current counts of objects in unPAK database",
  # Application title
            tabPanel(
            dataTableOutput("linktable")
      )
  )
)