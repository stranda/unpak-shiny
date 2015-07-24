library(shiny)
library(datasets)
library(unpakR)
con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")

#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

    values <- reactive({
        query <- paste("SELECT O.value, Pl.Accession_idAccession, T.name FROM Observation O",
                       " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                       " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                       " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                       " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                       " WHERE E.name = '",input$expt,"' AND",
                       " Ph.name = '",input$pheno,"'",
                    sep="")
        
          obstbl <- dbGetQuery(con,query)
     if (input$linemeans=="yes")
         {
             ret <- with(obstbl,aggregate(cbind(value),by=list(Accession=Accession_idAccession,Treatment=name),mean))
         } else {
             ret <- obstbl
             ret$Treatment=ret$name
         }
     ret[,c("value","Treatment")]
     
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  # The output$summary depends on the datasetInput reactive expression, 
  # so will be re-executed whenever datasetInput is re-executed 
  # (i.e. whenever the input$dataset changes)
  output$summary <- renderPrint({
    dataset <- values()$value
    summary(dataset)
  })

 output$bwp <- renderPlot(
     {
#         dat <- as.data.frame(values())
         boxplot(value~Treatment,ylab="",data=values())
     })

})
