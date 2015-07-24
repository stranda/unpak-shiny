source('global.R')
library(ggplot2)


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  
  values <- reactive({
    
    cdata <- session$clientData
    tmp <- strsplit(cdata$url_search,"&")[[1]]
    tmp <- tmp[grep("line",tmp)]
    if(length(tmp)>0) {
      url_line=strsplit(tmp,"=")[[1]][2]
      updateTextInput(session=session,inputId="line",value=url_line)
    }
    
    con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
    lines <- unique(dbGetQuery(con,"SELECT idAccession FROM ACCession"))
    if (toupper(input$line) %in% lines$idAccession) #this tests the text input against the db before running query (insulate from sql inject)
    {
      query <- paste("SELECT O.value, Ph.name, Pl.Accession_idAccession, T.name, E.name, F.name, Pl.idIndividualPlant",
                     " FROM Observation O",
                     " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                     " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                     " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                     " JOIN Facility F ON F.idFacility = Pl.Facility_idFacility",
                     " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                     #" WHERE Pl.Accession_idAccession = '",toupper(input$line),"'",
                     sep="")
      
      obstbl <- dbGetQuery(con,query)
      names(obstbl) <- c("value","phenotype","line","treatment","experiment","facility","individualPlant")
      if (input$includeall!="yes") obstbl <- obstbl[!(obstbl$phenotype%in%phenotypes.to.exclude),]
      udf <- unique(obstbl[obstbl$line==input$line,c("experiment","phenotype","treatment")])
      obstbl <- merge(obstbl,udf)
      
      if (dim(obstbl)[1]>0)
      {
        ret <- obstbl
        ret <- ret[complete.cases(ret),]
      } else {
        ret <- NULL
      }
    } else {
      ret <- NULL
    }
    dbDisconnect(con)
    ret
  })
  
  output$alldata = renderText ({
     data = values()
     unique(data$phenotype)

  })


})

