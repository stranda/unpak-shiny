#Partial Authorship by Nick Levitt
source("../global.R")
library(ggplot2)
library(gridExtra)
dbInfo = read.table('../../dbInfo.txt')


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {
  allData = reactive({
    expt= " "
    treat=" "
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
    query <- paste("SELECT O.value, Pl.Accession_idAccession, T.name, E.name, F.Name FROM Observation O",
                   " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                   " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                   " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                   " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                   " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility",
                   " WHERE",
                   expt,
                   treat,
                   " Ph.name = '",input$pheno,"'",
                   sep="")
    
    obstbl <- dbGetQuery(con,query)
    
    
    names(obstbl) <- c("value","Accession","treatment","experiment","facility")
    
    ret <- obstbl
    ret$Treatment=ret$treatment
    ret$Experiment=ret$experiment
    ret$Facility=ret$facility
    ret <- ret[order(ret$Accession,ret$Experiment,ret$Treatment,ret$Facility),c("value","Treatment","Experiment","Facility","Accession")]
    
    
    #             ret <- ret[complete.cases(ret),]
    
    
    
    dbDisconnect(con)
    ret
  })
  
  
  values <- reactive({
    if (is.null(input$expt)){expt=" "} else if (input$expt=="All"){expt=" "} else {expt=paste0(" E.name = '",input$expt,"' AND")}
    
    if (is.null(input$treats)){treat=" "} else if (input$treats=="All"){treat=" "} else {treat=paste0(" T.name = '",input$treats,"' AND")}
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
    query <- paste("SELECT O.value, Pl.Accession_idAccession, T.name, E.name, F.Name FROM Observation O",
                   " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                   " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                   " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                   " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                   " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility",
                   " WHERE",
                   expt,
                   treat,
                   " Ph.name = '",input$pheno,"'",
                   sep="")
    
    obstbl <- dbGetQuery(con,query)
    
    
    names(obstbl) <- c("value","Accession","treatment","experiment","facility")
    
    if (dim(obstbl)[1]>0)
    {
      if (input$linemeans=="yes")
      {
        ret <- with(obstbl,aggregate(cbind(value),by=list(Accession=Accession,Treatment=treatment),mean))
        ret <- ret[order(ret$value),c("value","Treatment","Accession")]
      } else {
        ret <- obstbl
        ret$Treatment=ret$treatment
        ret$Experiment=ret$experiment
        ret$Facility=ret$facility
        ret <- ret[order(ret$Accession,ret$Experiment,ret$Treatment,ret$Facility),c("value","Treatment","Experiment","Facility","Accession")]
      }
      
      #             ret <- ret[complete.cases(ret),]
      
    } else {
      ret <- data.frame(value=NA,Treatment=NA,Experiment=NA,Accession=NA)
    }
    
    dbDisconnect(con)
    dvalues <<- ret
    ret
  })
  
  output$msg <- renderText(
    {
      df <- values()
      if (dim(df[complete.cases(df),])[1]==0)
      {
        paste("This combination of experiment (",input$expt,"), phenotype (",input$pheno,") and/or treatment (",input$treat,") does not exist in the database.  Try another combination")
      } else {
        paste("Experiment (",input$expt,"), Phenotype (",input$pheno,"), Treatment (",input$treats,")")
      }
    })
  
  output$experiments = renderUI({
    df = allData()
    poss = unique(df$Experiment)
    
    selectInput("expt", "Choose an experiment:", 
                choices = c("All",sort(poss))) 
  }) 
  
  output$treats = renderUI({
    df = allData()
    if (input$expt != 'All') {
      df = df[which(df$Experiment == input$expt),]
    }
    poss = unique(df$Treatment)
    selectInput("treats", "Choose a treatment:", 
                choices = c("All",sort(poss))) 
  })

  
  dvalues %>%
    ggvis(~value) %>%
    layer_histograms(width =  input_slider(1, 100, step = 5, label = "width")) %>%
    bind_shiny("ggvis", "ggvis_ui")


  
  output$all = renderDataTable({
    dvalues
  })
})
