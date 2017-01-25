#Partial Authorship by Nick Levitt
source("../global.R")
library(ggplot2)
library(gridExtra)

#### Define server logic required to summarize and view the selected dataset
dbInfo = read.table('../../dbInfo.txt')

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

   if (input$treats=="All"){treat=" "} else {treat=paste0(" T.name = '",input$treats,"' AND")}
     con <- dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
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
                  choices = c(sort(poss),"All"), selected="control") 
  }) 

buildHist = function(df, select) {
  
  if(length(df) == 0) {    
  }
  
  else {
    
    
    bins = input$breaks
    if (bins == 0) {
      bins = 1
    }
    
    bins = max(df$value) / bins
    
    left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
    right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
    
    #          hist(df$value,xlab=paste("Value of",input$pheno),main=paste0("Exp:",input$expt,", Treat:",input$treat,", Pheno:",input$pheno),breaks = input$breaks)
    #          rug(df$value)
    #          abline(v=left,col="green",lwd=2)
    #          abline(v=right,col="red",lwd=2)
    
    xxx = ggplot(data=df,aes(value))

    if(!select) {
      xxx + geom_histogram(binwidth=bins, aes(fill=Treatment)) + geom_rug()  + geom_vline(xintercept=left,  colour='red',
                 linetype="dashed", size=1) +
      geom_vline(xintercept=right,  colour='blue',
                 linetype="dashed", size=1) + ggtitle("Distribution for all samples")
    }
    else {
      xxx + geom_histogram(binwidth=bins, aes(fill=Treatment)) + geom_rug() + ggtitle("Distribution for selected samples")
    }
  }
}



 
 output$hist <- renderPlot( {
   df <- as.data.frame(values())
   plot = buildHist(df, FALSE)
   plot
  })

buildSelect = function(df) {
  
}

  output$selectHist = renderPlot({
    df <- values()
    
    
    left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
    right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
    
    df <- df[(df$value>=left)&(df$value<=right),]
    plot = buildHist(df, TRUE)
    plot
  
    
  })

  output$stats = renderTable({
    df <- values()    
    left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
    right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
    
    df <- df[(df$value>=left)&(df$value<=right),]
    
    validate(
      need(length(df$value) > 0, "Please select data using the sliders on the left")
    )
    
    numSamples = length(df$value)
    allTreats = c('control','highwater','osmocote')
    treats = df$Treatment
    treatPercent = c()
    for (i in 1:length(allTreats)) {
       treatPercent[i] = (length(which(treats==allTreats[i])) / numSamples) * 100
    }  
    Min = range(df$value)[1]
    Max = range(df$value)[2]
    Mean = mean(df$value)
    StanD = sd(df$value)    
    stats = data.frame(numSamples, Min, Mean, Max, StanD,treatPercent[1],treatPercent[2],treatPercent[3])
    names(stats) = c('Number of Samples','Min','Mean', 'Max', 'Standard Deviation',paste('Percent',allTreats[1]),paste('Percent',allTreats[2]),paste('Percent',allTreats[3]))   
    stats 
  })
  

  output$linktable <- renderDataTable({
    df <- values()
    left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
    right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
    df <- df[(df$value>=left)&(df$value<=right),]
    url.root = paste0(sep = '',"http://",session$clientData$url_hostname,":",
                      session$clientData$url_port,'/findline/?line=')
    df$Accession <- paste0("<a href='",url.root, df$Accession, "' target='_blank'>",df$Accession,"</a>")
    df
  }, escape = FALSE)


  output$downloadPlot = downloadHandler(
    filename = function() {
      paste("phenorange",Sys.Date(),"-",input$expt,"exp,",input$pheno,"phenos,",input$treats,"treats.png")
    },
    content = function(file) {
      df <- values()
      left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
      right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)    
      selectdf <- df[(df$value>=left)&(df$value<=right),]
      plot1 = buildHist(df, FALSE)
      plot2 = buildHist(selectdf, TRUE)
      png(file)
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 600, units = "in")
      plots = grid.arrange(plot1, plot2, ncol=1)
      dev.off()
      
    }
  )

 output$downloadData <- downloadHandler(
                                        filename = function() {
                                          paste("phenorange",Sys.Date(),"-",input$expt,"exp,",input$pheno,"phenos,",input$treats,"treats.csv")                                        },
                                        content = function(file) {
                                          df <- values()
                                          left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
                                          right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
                                          
                                          df <- df[(df$value>=left)&(df$value<=right),]

                                          write.csv(file=file,df)
                                        }
                                        )
 
})
