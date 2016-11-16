
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
options(max.print=100)
library(shiny)
source("global.R")
source("adjust-pheno.R")
library(ggplot2)
dbInfo = read.table('dbInfo.txt')

CONSERVEDGROUPSQUERY <- "SELECT DISTINCT IndependentDataCache.ConservedGroup FROM IndependentDataCache"
PHENOTYPETABLESQUERY <- "SELECT * FROM Phenotype"
EXPERIMENTQUERY <- "SELECT DISTINCT Experiment.name FROM Experiment"
GENERALDATAQUERY <- paste ('SELECT IDC.id AS "PlantID", IP.Accession_idAccession AS "Accession", IDC.ConservedGroup, E.name AS "Expt",F.name AS Facility,T.name AS "Treatment", P.name AS "Phenotype", O.value AS "Value"',
                           'FROM IndependentDataCache IDC',
                             'JOIN IndividualPlant IP',
                              'ON IDC.id = IP.idIndividualPlant',
                             'JOIN Observation O',
                              'ON IDC.id = O.IndividualPlant_idIndividualPlant',
                             'JOIN Experiment E',
                              'ON IP.Experiment_idExperiment = E.idExperiment',
                             'JOIN Facility F',
                              'ON IP.Facility_idFacility = F.idFacility',
                             'JOIN Treatment T',
                              'ON O.Treatment_idTreatment = T.idTreatment',
                             'JOIN Phenotype P',
                               'ON O.Phenotype_idPhenotype = P.idPhenotype',
                            'WHERE IDC.ConservedGroup IS NOT NULL;',
                           sep = ' ')



shinyServer(function(input, output,session) {

  #Open database connection
  con = dbConnect(MySQL(),
                  dbname=toString(dbInfo[[1]]),
                  user=toString(dbInfo[[2]]),
                  password=toString(dbInfo[[3]])
                  )

  #Query all needed data
  allData <- data.frame(dbGetQuery(con, GENERALDATAQUERY))

  #Query phenotypes
  phenosTable <- dbGetQuery(con, PHENOTYPETABLESQUERY)
  phenos <- phenosTable$name
  availPhenotypes <- phenos[!(phenos %in% phenotypes.to.exclude)]
  
  #Get available experiments 
    #Store experiment names as a list
  availExperiments<- c(unique(allData$Expt))

  #Create experiment  select box
  output$experimentsBox <- renderUI(
    selectizeInput(inputId = 'expt',
                   label='Experiment',
                   choices=c("All",availExperiments)
                   ))
  
  #Create phenotype select box
  output$phenotypesBox <- renderUI( 
    
    selectizeInput(inputId = 'pheno',
                   label= "Phenotypes",
                   choices = availPhenotypes,
                   selected = availPhenotypes[1],
                   multiple = TRUE))
  
  #Plot data on boxplots (Reactive)
  output$graph <- renderPlot({
    df = allData
    
    df = df[which(df$Phenotype == input$pheno),]
    if(input$expt != "All"){
      df = df[which(df$Expt == input$expt), ]
    }    
    
    p <- ggplot(df,aes(ConservedGroup, Value))
    
    out <- p + geom_boxplot(aes(fill = ConservedGroup)) + 
      ylab(input$pheno) + 
      xlab('Conserved Group') + 
      geom_jitter(width = 0.1)
    
    #Check if data should be split by treatment
    if(input$splitByTreatment){
      out <- out + facet_grid(Treatment ~ .)
    }
    
    #Check log10 scale has been selected z
    if(input$log){
      out + scale_y_log10()
    }
    else{
      out
    }
  })
  
  
  #Close database connetion
   session$onEnded(dbDisconnect(con))
  })
