source("../global.R")
library(ggplot2)

# Define UI for dataset viewer application

shinyUI(
  navbarPage("Phenotypes from Gene identifier",
    tabPanel("Histogram",
      sidebarLayout(
        sidebarPanel(
          textInput("gene", "Enter a locus (ATxGxxxxx):", "AT2G30580"),
          uiOutput('phenos'),
          radioButtons("linemeans", "Report line means?",
                        c("Yes" = "yes",
                          "No" = "no"), selected = 'no'),
          radioButtons("correct", "Correct for among facility\nvariation using",
                        c("Nothing (raw data)" = "none",
                          "Means of all plants" = "all",
                          "Means of phytometers" = "phyt"
                          )
                       )
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
