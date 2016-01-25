source("../global.R")
dbInfo = read.table('../../dbInfo.txt')

#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
    values <- reactive({
        if (input$expt=="All"){expt=" "} else {expt=paste0(" WHERE E.name = '",input$expt,"'")}
      con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
      query <- paste("SELECT Pl.idIndividualPlant, Pl.Accession_idAccession, T.name, E.name, F.Name,",
                       " Ph.name, O.value",
                       " FROM Observation O",
                       " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                       " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                       " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                       " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                       " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility",
                       expt,
                       sep="")

        obstbl <- dbGetQuery(con,query)
        dbDisconnect(con)
        names(obstbl) <- c("PlantID","Accession","treatment","experiment","facility","Phenotype","value")
        if (input$linemeans=="yes")
            {
                ret <- with(obstbl,aggregate(cbind(value),by=list(Accession=Accession,
                                                              Experiment=experiment,
                                                              Facility=facility,
                                                              Phenotype=Phenotype,
                                                              Treatment=treatment),mean))
            } else {
                ret <- obstbl
                ret$Treatment=ret$treatment
                ret$Experiment=ret$experiment
                ret$Facility=ret$facility
            }
        if (input$wide=="yes")
            {
               names(ret)[grep("Phenotype",names(ret))] <- "variable"
               tmp <- cast(ret,fun.aggregate=mean)
               ret <- tmp
            }
        ret
    })
    
    output$msg <- renderText(
        {
            df <- values()
            if (dim(df)[1]<1)
                {
                    paste("No data present")
                } else {
                    paste(paste(dim(df)[1],"rows and",dim(df)[2],"columns of data ready to download\n"),paste())
                }
        })
    

 output$downloadData <- downloadHandler(
     filename = function() {
         paste("phenotypes",Sys.Date(),".csv",sep="")
     },
     content = function(file) {
         df <- values()
         write.csv(file=file,row.names=F,df)
     }
     )
     
 })
