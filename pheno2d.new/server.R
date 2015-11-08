#Partial Authorship by Nick Levitt
source("../global.R")
library(ggplot2)

# Function that returns the x and y range of the brushed selection.
xy_range_str <- function(e) {
  if(is.null(e)) return("NULL")
  c(round(e$xmin, 2), round(e$xmax, 2), round(e$ymin, 2),round(e$ymax, 2))
}

dbInfo = read.table('../../dbInfo.txt')


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {
  # Query for all the data of the two selected phenotypes 
  allData <- reactive({
    expt= " "
    treat=" "
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
    query <- paste("SELECT O.value, Pl.Accession_idAccession, T.name, E.name, F.Name, Ph.name FROM Observation O",
                   " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                   " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                   " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                   " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                   " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility",
                   " WHERE",
                   expt,
                   treat,
                   " (Ph.name = '",input$pheno1,"'  OR Ph.name = '",input$pheno2,"')",
                   sep="")
    
    obstbl <- dbGetQuery(con,query)
    names(obstbl) <- c("value","Accession","treatment","experiment","facility","phenotype")
    if (dim(obstbl)[1]>0)
    {
      #             if (input$linemeans=="yes")
      if (TRUE)
      {
        ret <- with(obstbl,aggregate(cbind(value),by=list(Accession=Accession,Treatment=treatment,Experiment=experiment,Phenotype=phenotype),mean))
        ret <- ret[order(ret$value),c("value","Phenotype","Experiment","Treatment","Accession")]
      } else {
        ret <- obstbl
        ret$Treatment=ret$treatment
        ret$Experiment=ret$experiment
        ret$Facility=ret$facility
        ret$Phenotype=ret$phenotype
        ret <- ret[order(ret$Accession,ret$Experiment,ret$Treatment,ret$Facility),c("value","Phenotype","Treatment","Experiment","Facility","Accession")]
      }
      ret <- ret[complete.cases(ret),]
      
    } else {
      ret <- NULL  #data.frame(value=NA,Phenotype=NA,Treatment=NA,Experiment=NA,Accession=NA)
    }
    dbDisconnect(con)
    ret
  })
  
  # Query of the data filtered by selected experiment and treatment
  values <- reactive({
   if (input$expt=="All"){expt=" "} else {expt=paste0(" E.name = '",input$expt,"' AND")}
   if (input$treat=="All"){treat=" "} else {treat=paste0(" T.name = '",input$treat,"' AND")}
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
    query <- paste("SELECT O.value, Pl.Accession_idAccession, T.name, E.name, F.Name, Ph.name FROM Observation O",
                    " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                    " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                    " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                    " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                    " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility",
                    " WHERE",
                    expt,
                    treat,
                    " (Ph.name = '",input$pheno1,"'  OR Ph.name = '",input$pheno2,"')",
                    sep="")

     obstbl <- dbGetQuery(con,query)
     names(obstbl) <- c("value","Accession","treatment","experiment","facility","phenotype")
     if (dim(obstbl)[1]>0)
         {
#             if (input$linemeans=="yes")
             if (TRUE)
                 {
                     ret <- with(obstbl,aggregate(cbind(value),by=list(Accession=Accession,Treatment=treatment,Experiment=experiment,Phenotype=phenotype),mean))
                     ret <- ret[order(ret$value),c("value","Phenotype","Experiment","Treatment","Accession")]
                 } else {
                     ret <- obstbl
                     ret$Treatment=ret$treatment
                     ret$Experiment=ret$experiment
                     ret$Facility=ret$facility
                     ret$Phenotype=ret$phenotype
                     ret <- ret[order(ret$Accession,ret$Experiment,ret$Treatment,ret$Facility),c("value","Phenotype","Treatment","Experiment","Facility","Accession")]
                   }
             ret <- ret[complete.cases(ret),]

         } else {
             ret <- NULL  #data.frame(value=NA,Phenotype=NA,Treatment=NA,Experiment=NA,Accession=NA)
         }
     dbDisconnect(con)
     ret
  })
  
  # Renders of User interface to select which experiment to filter by.
  output$experiments = renderUI({
    df = allData()
    var1 <- input$pheno1
    var2 <- input$pheno2
    names(df)[grep("Phenotype",names(df))] <- "variable"
    df <- cast(df)
    if (ncol(df) == 4) {
      poss = unique(df$Experiment)
    }
    else {
      indx1 = which(is.na(df[,4]))
      indx2 = which(is.na(df[,5]))
      df = df[-c(indx1,indx2),]
      poss = unique(df$Experiment)
    }
    selectInput("expt", "Choose an experiment:", 
                choices = c("All",sort(poss)))
  }) 


# Renders the User interface for selecting which treatment to use
output$treat = renderUI({
  df = allData()
  var1 <- input$pheno1
  var2 <- input$pheno2
  names(df)[grep("Phenotype",names(df))] <- "variable"
  df <- cast(df)
  if (ncol(df) != 4) {
    indx1 = which(is.na(df[,4]))
    indx2 = which(is.na(df[,5]))
    df = df[-c(indx1,indx2),]
  }

  
  if (input$expt != 'All') {
    df = df[which(df$Experiment == input$expt),]
  }
  poss = unique(df$Treatment)
  selectInput("treat", "Choose a treatment:", 
              choices = c("All",sort(poss))) 
})

# Renders the error message if the combination of phenotype/line/experiment does not exist in the DB, 
# otherwise outputs a message giving the selected filters.
output$msg <- renderText(
{
  df <- values()
  if (dim(df[complete.cases(df),])[1]==0)
  {
    paste("This combination of experiment (",input$expt,"), phenotypes (",input$pheno1,", ",input$pheno2,") and/or treatment (",input$treat,") does not exist in the database.  Try another combination")
  } else {
    paste("Experiment (",input$expt,"), Phenotypes (",input$pheno1,", ",input$pheno2,"), Treatment (",input$treat,")")
  }
})
 
output$urlText = renderText({
  df <- values()
  url.root = paste(sep = "", "HOST = ",session$clientData$url_hostname,": PORT = ",session$clientData$url_port,'/findline/?line=')

})

 output$scatter <- renderPlot(
     {       
       df <- values()
       var1 <- input$pheno1
       var2 <- input$pheno2
       names(df)[grep("Phenotype",names(df))] <- "variable"
       df <- cast(df)  
       phenoOne = input$pheno1
       phenoTwo = input$pheno2
       xxx = ggplot(df,aes_string(x = phenoOne, y = phenoTwo))
       xxx + geom_point(aes(color = Treatment)) + ggtitle('Phenotypic Distribution')
     })

output$displaySelectInfo <- renderText({
  selected = xy_range_str(input$plot_brush)
  
  validate(
    need(selected != "NULL", "Click and drag to select data")
  )
  

  paste0(
    "Selected Range: xmin=", selected[1], " xmax=", selected[2], 
         " ymin=", selected[3], " ymax=", selected[4])

})

output$statText = renderText({
  selected = xy_range_str(input$plot_brush)
  
  validate(
    need(selected != "NULL" & ncol(df) > 4, "")
  )
  
  "Simple Statistics"
})



output$stats = renderTable(
{
  
  selected = xy_range_str(input$plot_brush)
  
  
  df <- values()
  var1 <- input$pheno1
  var2 <- input$pheno2
  names(df)[grep("Phenotype",names(df))] <- "variable"
  df <- cast(df)


  
  selected = xy_range_str(input$plot_brush)
  left = selected[1]
  right = selected[2]
  top = selected[3]
  bot = selected[4]
  
  x = df[,var1]
  y = df[,var2]

  xindex = which(x > left & x < right )
  yindex = which(y < bot & y > top)
  bothindex = which(xindex %in% yindex)
  
  df = df[xindex[bothindex],]
#   if(ncol(df) == 4)
#     numSamples = 0

  validate(
    need(selected != "NULL" & ncol(df) > 4, "")
    )

  
  phenoOneValues = as.data.frame(df[,5])
  phenoTwoValues = as.data.frame(df[,4])
  # NEW CODE  
  numSamples = nrow(phenoOneValues)
  

  

  

    allTreats = c('control','highwater','osmocote')
    treats = df$Treatment
    treatPercent = c()
    for (i in 1:length(allTreats)) {
      treatPercent[i] = (length(which(treats==allTreats[i])) / numSamples) * 100
    }
    
    
    phenoOneMin = range(phenoOneValues)[1]
    phenoOneMax = range(phenoOneValues)[2]
    phenoOneMean = mean(df[,5])
    phenoOneStanD = sd(df[,5])
    phenoOneName = input$pheno1
    
    phenoTwoMin = range(phenoTwoValues)[1]
    phenoTwoMax = range(phenoTwoValues)[2]
    phenoTwoMean = mean(df[,4])
    phenoTwoStanD = sd(df[,4])
    phenoTwoName = input$pheno2
    
    stats = range(phenoOneValues)
    
    stats = data.frame(numSamples, phenoOneMin, phenoOneMean, phenoOneMax, phenoOneStanD, phenoTwoMin, phenoTwoMean, phenoTwoMax, phenoTwoStanD, treatPercent[1],treatPercent[2],treatPercent[3])
    names(stats) = c('Number of Samples',paste(phenoOneName,'Min'),paste(phenoOneName,'Mean'), paste(phenoOneName,'Max'), paste(phenoOneName,'Standard Deviation'),paste(phenoTwoName,'Min'),paste(phenoTwoName,'Mean'), paste(phenoTwoName,'Max'), paste(phenoTwoName,'Standard Deviation'),paste('Percent',allTreats[1]),paste('Percent',allTreats[2]),paste('Percent',allTreats[3]))
    
  
    stats

})

output$correlation = renderText({
  df <- values()
  var1 <- input$pheno1
  var2 <- input$pheno2
  names(df)[grep("Phenotype",names(df))] <- "variable"
  df <- cast(df)
  
  
  
  selected = xy_range_str(input$plot_brush)
  left = selected[1]
  right = selected[2]
  top = selected[3]
  bot = selected[4]
  
  x = df[,var1]
  y = df[,var2]
  
  xindex = which(x > left & x < right )
  yindex = which(y < bot & y > top)
  bothindex = which(xindex %in% yindex)
  
  df = df[xindex[bothindex],]
  
  validate(
    need(  ncol(df) > 4, "Select Different Phenotypes to see Statistics")
  ) 
  
  validate(
    need(selected != "NULL", "No Data Selected")
  ) 
  

  

    cor = cor(df[,4:5])
    cor = cor[2]
    cor = round(cor, 4)
    cor = toString(cor)
    cor = paste('Corelation Between Selected Samples:',cor, sep=' ')
    
    cor
})

  output$selectedTable <- renderDataTable({
        df = values()        
        var1 <- input$pheno1
        var2 <- input$pheno2       
        df <- values()
        names(df)[grep("Phenotype",names(df))] <- "variable"
        df <- cast(df)
        selected = xy_range_str(input$plot_brush)        
        validate(
          need(selected != "NULL", "Please select data to see individual lines")
        )        
        left = selected[1]
        right = selected[2]
        top = selected[3]
        bot = selected[4]      
        x = df[,var1]
        y = df[,var2]        
        xindex = which(x > left & x < right )
        yindex = which(y < bot & y > top)
        bothindex = which(xindex %in% yindex)
        df = df[xindex[bothindex],]
        url.root = paste0(sep = '',"http://",session$clientData$url_hostname,":",
                          session$clientData$url_port,'/findline/?line=')
        df$Accession <- paste0("<a href='",url.root, df$Accession, "' target='_blank'>",df$Accession,"</a>")
        df    
    }, escape = FALSE)

  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("linePDF",Sys.Date(),".png",sep="")
    },
    content = function(file) {
      df <- values()
      var1 <- input$pheno1
      var2 <- input$pheno2
      names(df)[grep("Phenotype",names(df))] <- "variable"
      df <- cast(df)  
      phenoOne = input$pheno1
      phenoTwo = input$pheno2
      xxx = ggplot(df,aes_string(x = phenoOne, y = phenoTwo))
      xxx = xxx + geom_point(aes(color = Treatment)) + ggtitle('Phenotypic Distribution') 
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 600, units = "in")
      ggsave(file, plot = xxx, device = device)
    }
  )

 output$downloadData <- downloadHandler(
                                        filename = function() {
                                          paste("phenorange",Sys.Date(),".csv",sep="")
                                        },
                                        content = function(file) {
                                          df = values()        
                                          var1 <- input$pheno1
                                          var2 <- input$pheno2       
                                          df <- values()
                                          names(df)[grep("Phenotype",names(df))] <- "variable"
                                          df <- cast(df)
                                          selected = xy_range_str(input$plot_brush)              
                                          left = selected[1]
                                          right = selected[2]
                                          top = selected[3]
                                          bot = selected[4]      
                                          x = df[,var1]
                                          y = df[,var2]        
                                          xindex = which(x > left & x < right )
                                          yindex = which(y < bot & y > top)
                                          bothindex = which(xindex %in% yindex)
                                          df = df[xindex[bothindex],]
                                          write.csv(file=file,df)
                                        }
                                        )
 
})
