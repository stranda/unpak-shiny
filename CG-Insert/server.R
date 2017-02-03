##server.R
### This file defines the logic for the CG-Insert R-Shiny App
####Created by: Hassam Solano-Morel (hassamsolano@gmail.com)

options(max.print = 100)

##All import statements listd on /scripts/global.r

#Contain mySQL queries for retrieving respective data
source("scripts/Conserved.R")
source("scripts/InsertLoc.R")

#Contains: getConnection, phenotypes.to.exclude
source("scripts/global.R")


shinyServer(function(input, output, session) {
  
  #Used to retrieve available phenotypes 
  PHENOTYPETABLESQUERY <- "SELECT * FROM Phenotype"
  
  #Get database connection
  dbInfo = read.table('dbInfo.txt')
 
  group <- c(dbname=toString(dbInfo[[1]]),
    user=toString(dbInfo[[2]]),
    password=toString(dbInfo[[3]]))

  con <- getConnection(group)
  
  
  #Query all needed data into dataframe
  allData <- reactive({
    if (input$dataType == "Conserved Groups") {
      data.frame(dbGetQuery(con, getConservedData()))
    }
    else if (input$dataType == "Insert Location") {
      data.frame(dbGetQuery(con, getInsertData()))
    }
  })
  
  observeEvent(input$action1, {
    session$sendCustomMessage(type = 'alertMessage',
                              message = 'Conserved Groups message')
  })
  
  observeEvent(input$action2, {
    session$sendCustomMessage(type = 'alertMessage',
                              message = 'Insertion Location message')
  })
  
  
  #Query phenotypes
  phenosTable <- dbGetQuery(con, PHENOTYPETABLESQUERY)
  phenos <- phenosTable$name
  availPhenotypes <- phenos[!(phenos %in% phenotypes.to.exclude)]
  
  #Get available experiments & Store experiment names as a list
  availExperiments <- reactive({
    c(unique(allData()$Expt))
  })
  
  #Create experiment select box`
  output$experimentsBox <- renderUI(selectizeInput(
    inputId = 'experimentsBox',
    label = 'Experiment',
    choices = c("All", availExperiments())
  ))
  
  #Create phenotype select box
  output$phenotypesBox <- renderUI(
    selectizeInput(
      inputId = 'phenotypesBox',
      label = "Phenotypes",
      choices = availPhenotypes,
      selected = availPhenotypes[2],
      multiple = TRUE
    )
  )
  
  #Prepare data according to user input
  buildFinalData = function() {
    df <- allData()
    
    #Chosen experiment
    if (input$experimentsBox != "All") {
      df = df[which(df$Expt == input$experimentsBox),]
    }
    
    #Ignore NAs
    df <- df[!is.na(df$value), ]
    df <- df[!is.nan(df$value), ]
    
    #Chosen phenotypes
    df = df[which(df$phenotype %in% input$phenotypesBox), ]
    
    
    #Chosen correction type
    if (input$correct == "phyt") {
      if (input$dataType == "Conserved Groups")
        df = phytcorrect(
          df,
          input$phenotypesBox,
          c("Expt", "Facility", "Treatment", "ConservedGroup"),
          'Accession'
        )
      else if (input$dataType == "Insert Location")
        df = phytcorrect(
          df,
          input$phenotypesBox,
          c("Expt", "Facility", "Treatment", "InsertLocation"),
          'Accession'
        )
    }
    
    if (input$correct == "all") {
      if (input$dataType == "Conserved Groups")
        df = allcorrect(
          df,
          input$phenotypesBox,
          c("Expt", "Facility", "Treatment", "ConservedGroup"),
          'Accession'
        )
      else if (input$dataType == "Insert Location")
        df = allcorrect(
          df,
          input$phenotypesBox,
          c("Expt", "Facility", "Treatment", "InsertLocation"),
          'Accession'
        )
    }
    
    #Chosen report line means
    if (input$linemeans == 'yes') {
      if (input$dataType == "Conserved Groups")
        df <-
          df %>% group_by(Accession, Expt, Treatment, phenotype, ConservedGroup) %>%
          summarise(value = mean(value, na.rm = T))
      else if (input$dataType == "Insert Location")
        df <-
          df %>% group_by(Accession, Expt, Treatment, phenotype, InsertLocation) %>%
          summarise(value = mean(value, na.rm = T))
    }
    
    return(df)
  }
  
  #Plot data on boxplots (Reactive)
  output$graph <- renderPlot({
    df = buildFinalData()
    
    if (input$dataType == "Conserved Groups") {
      p <- ggplot(df, aes(ConservedGroup, value))
      
      out <- p + geom_boxplot(aes(fill = ConservedGroup)) +
        ylab(input$phenotypesBox) +
        xlab('Conserved Group') +
        geom_jitter(width = 0.1)
    }
    else if (input$dataType == "Insert Location") {
      p <- ggplot(df, aes(InsertLocation, value))
      
      out <- p + geom_boxplot(aes(fill = InsertLocation)) +
        ylab(input$phenotypesBox) +
        xlab('Insert Location') +
        geom_jitter(width = 0.1)
    }
    
    
    #Check for multiple selected phenotypes
    if (input$splitByTreatment && length(input$phenotypesBox) > 1) {
      out <- out + facet_grid(Treatment ~ phenotype, scales = "free")
      
    }
    
    else if (!input$splitByTreatment &&
             length(input$phenotypesBox) > 1) {
      out <- out + facet_grid(. ~ phenotype, scales = "free")
      
    }
    
    #Check if data should be split by treatment
    else if (input$splitByTreatment) {
      out <- out + facet_grid(Treatment ~ ., scales = "free")
      
    }
    
    
    #Check log10 scale has been selected z
    if (input$log) {
      df[, 'value'] <- df[, 'value'] + 1

      out + scale_y_log10(limits = c(1, NA))
    }
    else{
      out
    }
  })
  
})
