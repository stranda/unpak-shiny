#Partial Authorship by Nick Levitt
source("global.R")
library(ggplot2)
library(RColorBrewer)

dbInfo = read.table('../../dbInfo.txt')

con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
#get some phenotype names
phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
#the next line only allow for experiments with phenotypes
expttbl <- unique(dbGetQuery(con,"SELECT E.name FROM Experiment E JOIN IndividualPlant IP ON IP.Experiment_idExperiment = E.idExperiment JOIN Observation O ON IP.idIndividualPlant = O.IndividualPlant_idIndividualPlant"))
treattbl <- dbGetQuery(con,"SELECT * FROM Treatment")
phenoname <- phenotbl$name
phenoname <- phenoname[!(phenoname%in%phenotypes.to.exclude)]
phenoname <- c(phenoname[which(phenoname=="fruitnum")],phenoname[-which(phenoname=="fruitnum")])

treatname <- treattbl$name
treatname <- c(treatname[which(treatname=="control")],treatname[-which(treatname=="control")])
dbDisconnect(con)

# Define UI for dataset viewer application
shinyUI(navbarPage("Find lines based on phenotype", 
                   tabPanel("Histograms",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("pheno", "Choose a phenotype:", 
                                            choices = phenoname),
                                
#                                 selectInput("expt", "Choose an experiment:", 
#                                             choices = c("All",sort(expttbl$name))),
                                uiOutput('experiments'),
                                
                                selectInput("treat", "Choose a treatment:", 
                                            choices = c("All",treatname)),
                                
                                #            radioButtons("includeall", "Include 'minor' phenotypes",
                                #                         c("Yes" = "yes",
                                #                           "No" = "no")),
                                
                                radioButtons("linemeans", "Report line means?",
                                             c("Yes" = "yes",
                                               "No" = "no")),
                                
                                sliderInput("range", "Range of x-axis to sample",
                                            min = 0, max = 1, value = c(0.1,0.2)),
                                
                                
                                sliderInput("breaks", "Number of bins",
                                            min = 1, max = 100, value = 30, step = 5)
                                
                              ),
                              
                              
                              #Render the results
                              mainPanel(
                                textOutput("msg"),                                
                                downloadLink('downloadData','Download CSV of selection'),
                                
                                fluidRow(
                                  column(12,
                                         "",
                                         fluidRow(
                                           column(6,
                                                  plotOutput("hist")),
                                           column(6,
                                                  plotOutput('selectHist'))
                                         ),
                                         fluidRow(
                                           column(12,
                                                  h3("Statistics on Selected Samples"),tableOutput("stats"))
                                         )
                                        ) 
                                  )
                                )
                              )
                            
                            ),
                   
                   
                   tabPanel("Lines",
                            htmlOutput("linktable")
                            )

))
