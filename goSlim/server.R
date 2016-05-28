source("../global.R")
source('adjust-pheno.R')
library(ggplot2)
load('../allSlimData.rda')
dbInfo = read.table('../../dbInfo.txt')

#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {

  
  possSlims = function() {
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
    slims <- dbGetQuery(con,"SELECT distinct(GoSlim) FROM GeneOntology")
    slims <- as.array(slims[,1])
    dbDisconnect(con) 
    return(slims[2:length(slims)])
    
  } 
  
  possPhenos = function() {
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
    phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
    phenoname <- phenotbl$name
    phenoname <- phenoname[!(phenoname%in%phenotypes.to.exclude)]
    phenoname <- c(phenoname[which(phenoname=="fruitnum")],phenoname[-which(phenoname=="fruitnum")])
    dbDisconnect(con) 
    return(phenoname)
  
  }
  
  output$slims = renderUI({
    df = allData()
    poss = unique(df$slim)
    selectizeInput("slims", "Choose slim(s):", 
                   choices = c(poss),selected = poss[1],multiple=TRUE)  
  })
  
  allData <- reactive({
    df = as.data.frame(alldf[[which(possPhenos() == input$pheno)]])
    df
    })
  
  buildSlimDf = reactive({
    df = allData()
    df = df[which(df$slim == input$slims),]
    })
  
  output$expers = renderUI({
    df <- allData()
    df = df[which(df$slim == input$slims),]
    exper = c('All')
    exper = append(exper,unique(df$experiment))
    selectizeInput("expers", "Choose an Experiment:", 
                   choices = exper,selected = exper[1],multiple=FALSE) 
  })
  
  
  buildFinalData = function() {
    
    df <- allData()
    df = df[which(df$slim == input$slims),]
    if (input$expers != 'All') {
      df = df[which(df$experiment == input$expers),]
    }
    df <- df[!is.na(df$value),] #don't mess with NAs
    
    if (input$correct == "phyt")  df = phytcorrect(df, input$phenos, c("experiment","facility","treatment", "slim"), 'line')
    if (input$correct == "all")  df  =  allcorrect(df, input$phenos, c("experiment","facility","treatment"), 'line')
    
    if (input$linemeans == 'yes') { #get means per line instead of actual observations
      df <- df%>%group_by(line,experiment,treatment,phenotype, slim)%>%summarise(value=mean(value,na.rm=T))
    }
    df    
  }
  
  output$slimBox <- renderPlot({
    df = buildFinalData()
    df$treatment <- as.factor(df$treatment)
    p <- ggplot(df, aes(slim, value))
#    out = p + geom_boxplot(aes(fill = factor(treatment))) + facet_grid(slim ~ .) + ylab(input$pheno) + xlab('Treatment')
    out = p + geom_boxplot(aes(fill = slim)) + facet_grid(treatment ~ .) + ylab(input$pheno) + xlab('GO Slim') + geom_jitter(width=0.1)
    if (input$log == TRUE) {
      out + scale_y_log10()
    }
    else {
      out
    }
  })

  output$slimData = renderDataTable({
    buildFinalData()
  })
})
