#Partial Authorship by Nick Levitt
source("global.R")
library(ggplot2)
dbInfo = read.table('../../dbInfo.txt')



makeHTMLtable <- function (df,session)
{
  #   url.root <- "http://107.170.146.165:3838/findline/?line="
  url.root = paste0(sep = '',"http://",session$clientData$url_hostname,":",
                    session$clientData$url_port,'/findline/?line=')
  df$Accession <- paste0("<a href='",url.root, df$Accession, "' target='_blank'>",df$Accession,"</a>")
  cols <- dim(df)[2]
  rows <- dim(df)[1]
  for (j in 1:cols)
  {
    if (is.numeric(df[,j])) df[,j] <- as.character(round(df[,j],3))
    if (is.factor(df[,j])) df[,j] <- as.character(df[,j],3)
  }
  
  str <- "<table border = 1>"
  str <- paste0(str,"<tr><th>",paste(names(df),collapse="</th><th>"),"</tr>\n")
  for (i in 1:rows) {
    str <- paste0(str, "<tr><td>",paste(df[i,],collapse="</td><td>"),"</tr>\n")
  }
  str    
}


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
#                     " Ph.name = '",input$pheno,"'",
                     " Ph.name = 'fruitnum'",
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
      
      if (input$treat=="All"){treat=" "} else {treat=paste0(" T.name = '",input$treat,"' AND")}
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
      ret
  })
  
  output$msg <- renderText(
      {
          df <- values()
          if (dim(df[complete.cases(df),])[1]==0)
              {
                  paste("This combination of experiment (",input$expt,"), phenotype (",input$pheno,") and/or treatment (",input$treat,") does not exist in the database.  Try another combination")
              } else {
                  paste("Experiment (",input$expt,"), Phenotype (",input$pheno,"), Treatment (",input$treat,")")
              }
      })
  
  output$experiments = renderUI({
      df = allData()
      poss = unique(df$Experiment)
      
      selectInput("expt", "Choose an experiment:", 
                  choices = c("All",sort(poss)))
      
      
  }) 
  
  output$testText = renderText({
      x = input$testSelect
      x
  })
  
  output$hist <- renderPlot( {
                                df <- as.data.frame(values())
                                
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
                                    xxx + geom_histogram(binwidth=bins, aes(fill=Treatment)) + geom_rug() + 
                                        geom_vline(xintercept=left,  colour='red',
                                                   linetype="dashed", size=1) +
                                            geom_vline(xintercept=right,  colour='blue',
                                                       linetype="dashed", size=1)
                                    
                                }
                            })
  
  
  
  
  output$selectHist = renderPlot({
      df <- values()
      
      
      left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
      right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
      
      df <- df[(df$value>=left)&(df$value<=right),]
      validate(
          need(length(df$value) > 0, "Please select data using the sliders on the left")
          )
      
      bins = input$breaks
      if (bins == 0)
          bins = 1
      bins = max(df$value) / bins
      
      xxx = ggplot(data=df,aes(value))
      xxx + geom_histogram(binwidth=bins, aes(fill=Treatment)) + geom_rug()
      
      
  })
  
  output$rangetable <- renderTable(
      {
          df <- values()
          
          left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
          right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
          
          df <- df[(df$value>=left)&(df$value<=right),]
          df
      })
  
  output$linktable <- renderText(
      {
          df <- values()
          
          left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
          right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
          
          df <- df[(df$value>=left)&(df$value<=right),]
          makeHTMLtable(df,session)
      })
  
  output$numSamples = renderText({
      df <- values()
      left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
      right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
      df <- df[(df$value>=left)&(df$value<=right),]
      numSamples = length(df$value)
      
  })
  
  output$stats = renderTable(
      {
          df <- values()
          
          left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
          right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
          
          df <- df[(df$value>=left)&(df$value<=right),]
          
          validate(
              need(length(df$value) > 0, "Please select data using the sliders on the left")
              )
          
          numSamples = length(df$value)
          
          
                                        #     if(numSamples == 0) {
                                        #       stats = data.frame("There are no samples that meet this criteria")
                                        #       names(stats) = "WARNING!"
                                        #       row.names(stats) = NULL
                                        #     }
          
          
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
  
  output$downloadData <- downloadHandler(
      filename = function() {
          paste("phenorange",Sys.Date(),".csv",sep="")
      },
      content = function(file) {
          df <- values()
          left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
          right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)
          
          df <- df[(df$value>=left)&(df$value<=right),]
          
          write.csv(file=file,df)
      }
      )
  
})
