source("../global.R")
library(ggplot2)

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
shinyUI(
#pageWithSidebar(
  navbarPage("Find lines based on T-DNA Ratio",
  tabPanel("Histograms",                   
    sidebarLayout(
      sidebarPanel(
#         selectInput("Phenotype", "Choose a phenotype:", 
#                     choices = c('AreaRatio','EndoArea', 'TDNAArea')),
        sliderInput("range", "Range of x-axis to sample",
                    min = 0, max = 1, value = c(0.1,0.2))
        
        ),
#Render the results
      mainPanel(
        textOutput("msg"),
                
        fluidRow(
          column(12,
                 downloadLink('downloadPlot', 'Download PNG of plots'),
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
                   downloadLink('downloadData','Download CSV of selection'),
                   dataTableOutput("linktable"))
          )
        )
