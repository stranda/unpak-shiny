
library(ggplot2)
shinyUI(
  fluidPage(
    titlePanel("Phenotypes from line id"),
    sidebarLayout(fluid=F, position="left",
                  sidebarPanel(
                    textInput("line", "Enter an accession:", "CS70000"),
                    radioButtons("includeall", "Include all phenotypes?",
                                 c("No" = "no",
                                   "Yes" = "yes")),
                    radioButtons("linemeans", "Report line means?",
                                 c("Yes" = "yes",
                                   "No" = "no")),
                    radioButtons("correct", "Correct for among facility\nvariation using",
                                 c("Nothing (raw data)" = "none",
                                   "Means of all plants" = "all",
                                   "Means of phytometers" = "phyt"
                                 ))            
                  ),
                  mainPanel(
                        textOutput('alldata')
                  )
    )
  )
)
