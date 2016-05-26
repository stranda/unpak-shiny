source("../global.R")
library(ggplot2)

# Define UI for dataset viewer application

shinyUI(
  navbarPage("Phenotypes from line id",
    tabPanel("Histogram",
      sidebarLayout(
        sidebarPanel(
          textInput("line", "Enter an accession:", "CS70000"),
          textInput("line2", "Enter a second accession:", ""),
          uiOutput('phenos'),
          sliderInput('bins','Number of Bins',min=1, max = 50, value = 1, step = 1),

          radioButtons("linemeans", "Report line means?",
                        c("Yes" = "yes",
                          "No" = "no"), selected = 'no'),
          radioButtons("correct", "Correct for among facility\nvariation using",
                        c("Nothing (raw data)" = "none",
                          "Means of all plants" = "all",
                          "Means of phytometers" = "phyt"
                          )
                       ),
          actionButton("go", "Go")
       ),
        # Render the results
        mainPanel(
          htmlOutput("msg"),
          downloadLink('downloadPDF','Download PNG of plot below'),
          plotOutput('hist', height = 800)
        ), 
      # Close Sidebar Layout
      )
    ),
  # Second tab for data table
    tabPanel("Phenotypic values, treatments, facilities:",
        downloadLink('downloadData','Download CSV of phenotypes for selected line'),
        dataTableOutput("overview")
    )
  )
)
