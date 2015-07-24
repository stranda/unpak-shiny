source("../global.R")

#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {

 values <- reactive({
   con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
   query <- paste("SELECT * FROM Observation",
                  sep="")
   numobs <- dim(unique(dbGetQuery(con,query)))[1]
   query <- paste("SELECT * FROM IndividualPlant",
                  sep="")
   ptab <- dbGetQuery(con,query)
   numplants <- dim(ptab)[1]
   numlines <- length(unique(ptab$Accession_idAccession))
   query <- paste("SELECT * FROM Experiment",
                  sep="")
   numexp <- dim(dbGetQuery(con,query))[1]
   query <- paste("SELECT * FROM Facility",
                  sep="")
   numfac <- dim(dbGetQuery(con,query))[1]
   query <- paste("SELECT * FROM Phenotype",
                  sep="")
   numpheno <- dim(dbGetQuery(con,query))[1]

   query <- paste("SELECT * FROM TDNARatio",
                  sep="")
   numgeno <- dim(dbGetQuery(con,query))[1]


     dbDisconnect(con)

data.frame(Number.of=c("phenotypic observations","plants grown","lines","Experiments","Facilities","Phenotypes","T-DNA ratios"),
           n=c(numobs, numplants, numlines, numexp, numfac, numpheno,numgeno))
   
  })
  

 output$linktable <- renderDataTable(
     {
         df <- values()
         df
     })
 
})
