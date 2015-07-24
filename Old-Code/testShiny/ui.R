source("global.R")
library(ggplot2)

#con <- dbConnect(MySQL(),dbname="unpak",user="unpak",password="thaliana")
#get some phenotype names
#lines <- dim(unique(dbGetQuery(con,"SELECT IP.Accession_idAccession FROM IndividualPlant IP JOIN Observation O ON IP.idIndividualPlant = O.IndividualPlant_idIndividualPlant")))

# Define UI for dataset viewer application
shinyUI(navbarPage("Phenotypes from line id", 
                   tabPanel("Plots",
                            sidebarLayout(
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
                                               "Means of phytometers" = "phyt")
                                             ) 
                              ),
                              mainPanel(
                                htmlOutput("msg"),
                                plotOutput("exptplot",height="3000",width="1000")
                                )
                            )
                   ),
                   tabPanel("Downloads",
                            downloadLink('downloadData','Download CSV of phenotypes for selected line')
                            ,
                            br()
                            ,
                            downloadLink('downloadPDF','Download PDF of plot below'))
)
)