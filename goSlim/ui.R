#Partial Authorship by Nick Levitt
source("../global.R")
library(ggplot2)
library(RColorBrewer)

dbInfo = read.table('../../dbInfo.txt')
con <- dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
#get unique slims
slims <- dbGetQuery(con,"SELECT distinct(GoSlim) FROM GeneOntology")
slims <- as.array(slims[,1])
slims <- slims[2:length(slims)]

phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
phenoname <- phenotbl$name
phenoname <- phenoname[!(phenoname%in%phenotypes.to.exclude)]
phenoname <- c(phenoname[which(phenoname=="fruitnum")],phenoname[-which(phenoname=="fruitnum")])
# expers = c('All')
# expttbl <- unique(dbGetQuery(con,"SELECT E.name FROM Experiment E JOIN IndividualPlant IP ON IP.Experiment_idExperiment = E.idExperiment JOIN Observation O ON IP.idIndividualPlant = O.IndividualPlant_idIndividualPlant"))
# expers = append(expers,expttbl)
dbDisconnect(con)


# Define UI for dataset viewer application
shinyUI(navbarPage("Find lines based on Processes", 
                   tabPanel("Histograms",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("pheno", "Choose a phenotype:", 
                                            choices = phenoname),
                                uiOutput('expers'),
                                uiOutput('slims'),
                                radioButtons("linemeans", "Report line means?",
                                             c("Yes" = "yes",
                                               "No" = "no"), selected = 'no'),
                                checkboxInput('log', 'Base-10 Log Transform Y-axis?', value = FALSE),
                                
                                radioButtons("correct", "Correct for among facility\nvariation using",
                                             c("Nothing (raw data)" = "none",
                                               "Means of all plants" = "all",
                                               "Means of phytometers" = "phyt"
                                             )
                                )
                              ),
                              
                              
                              #Render the results
                              mainPanel(
                                fluidRow(
                                  column(12,
                                          downloadLink('downloadPlot','Download PNG of plot below'),
                                          fluidRow(
                                            column(12,
                                                   plotOutput("slimBox", height = 700))
                                         )
#                                          fluidRow(
#                                            column(12,
#                                                   h3("Statistics on Selected Samples"),tableOutput("stats"))
#                                          )
                                  ) 
                                )
                              )
                        )
                   ),
                   
                   
                   tabPanel("Lines",
                            dataTableOutput('slimData')
                   )
                   
))
