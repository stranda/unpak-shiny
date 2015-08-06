source("../global.R")
library(ggplot2)

# Define UI for dataset viewer application
shinyUI(
  navbarPage("Phenotypes from line id",
    tabPanel("Histogram",
      sidebarLayout(
        sidebarPanel(
          textInput("line", "Enter an accession:", "CS70000"),
          checkboxInput('multi', "Add a second line?"),
          uiOutput('line2'),
          uiOutput('phenos'),
          sliderInput('bins','BinSize',min=0.5, max = 20, value = 1, step = 0.5),
#           uiOutput('expts'),
#           uiOutput('treats'),
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
