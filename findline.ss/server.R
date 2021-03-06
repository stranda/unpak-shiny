source("../global.R")
#source('adjust-pheno.R')
library(ggplot2)
dbInfo = read.table('../../dbInfo.txt')

#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  allLines <- reactive({
    
    cdata <- session$clientData
    tmp <- strsplit(cdata$url_search,"&")[[1]]
    tmp <- tmp[grep("line",tmp)]
    if(length(tmp)>0) {
      url_line=strsplit(tmp,"=")[[1]][2]
      updateTextInput(session=session,inputId="line",value=url_line)
    }
    
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
    lines <- unique(dbGetQuery(con,"SELECT idAccession FROM Accession"))

      query <- paste("SELECT O.value, Ph.name, Pl.Accession_idAccession, T.name, E.name, F.name, Pl.idIndividualPlant",
                     " FROM Observation O",
                     " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                     " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                     " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                     " JOIN Facility F ON F.idFacility = Pl.Facility_idFacility",
                     " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                     sep="")
      
      obstbl <- dbGetQuery(con,query)
      names(obstbl) <- c("value","phenotype","line","treatment","experiment","facility","individualPlant")
      
#       udf <- unique(obstbl[obstbl$line==input$line,c("experiment","phenotype","treatment")])
#       obstbl <- merge(obstbl,udf)
      
      if (dim(obstbl)[1]>0)
      {
        ret <- obstbl
        ret <- ret[complete.cases(ret),]
      } else {
        ret <- NULL
      }
      
    cons<-dbListConnections(MySQL())
    for(con in cons)
      dbDisconnect(con) 
    poss <<- sort(unique(ret$phenotype))
    allData <- melt(ret, id = c('line','experiment','treatment','facility','phenotype','individualPlant'))
    allData <<- cast(allData, line+experiment+treatment+facility+individualPlant ~ phenotype)
    lineNames <<- unique(allData$line)
    allData
  })
  

  
  # Renders the User interface for selecting which phenotype to use
  output$phenos = renderUI({
    allLines()
    selectInput("phenos", "Choose a phenotype:", 
                choices = c(poss),selected = poss[9])  
  })
    
  # This gives the warning message if the line is not in the DB, or links to it if it is
  output$msg <- renderText({
      url.root <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="
      allLines()
      if (!input$line %in% lineNames)
         {
             paste("This line",input$line,"is not in the db  Try another one")
          } else {
              HTML(paste0("Go to TAIR for Line: <a href='",url.root,input$line,"' target='_blank'>",input$line,"</a>"))         
          }
  })
 
  # This renders the interactive data table of the selected values
  output$overview = renderDataTable({
    input$go
    isolate({
      df = allData
      inputLines = c(input$line,input$line2)
      col = which(names(df) == input$phenos)
      df = cbind(df[,1:5],df[,col])
      df <- df[!is.na(df[,6]),]
      names(df)[6] = 'value'
      df[which(df$line %in% inputLines),]
            })
  } , options = list(lengthMenu = c(50, 100, 500), pageLength = 50))

  # This function, given a dataframe, builds the histograms, breaking them up by experiment and treatment pairs.
  buildHist = function(df) {
    inputLines = c(input$line,input$line2)
    df = allData
    if(input$correct == "none") {
      col = which(names(df) == input$phenos)
      df = cbind(df[,1:5],df[,col])
      df <- df[!is.na(df[,6]),] #don't mess with NAs
      
      names(df)[6] = 'value'
      if (input$linemeans == 'yes') { #get means per line instead of actual observations
        df <- df%>%group_by(line,experiment,treatment)%>%summarise(value=mean(value,na.rm=T))
      }
      
      
      linedf = df[which(df$line %in% inputLines),]
      
      numBins = (range(df$value)[2] - range(df$value)[1]) / input$bins
      
      ggplot(data = df, aes(value, fill = treatment)) + 
        geom_histogram(binwidth = ((range(df$value)[2] - range(df$value)[1]) / input$bins)) + scale_x_continuous() + scale_colour_brewer(type="qual", palette=8) +
        geom_vline(data = linedf, aes(xintercept = value,color = line), linetype = 'dashed', show_guide = T) +
        facet_wrap(~ experiment + treatment, scales = 'free', ncol = 1)
    }
    
    
    else if (input$correct == "phyt") {
      df = phytcorrect(df, input$phenos, c("experiment","facility","treatment"), 'line')
      if (input$linemeans == 'yes') { 
        df <- df%>%group_by(line,experiment,treatment)%>%summarise(adjval=mean(adjval,na.rm=T))
      }
      linedf = df[which(df$line %in% inputLines),]
      ggplot(data = df, aes(adjval, fill = treatment)) + 
        geom_histogram(binwidth = input$bins) + scale_x_continuous() +
        geom_vline(data = linedf, aes(xintercept=adjval, color = line), linetype = 'dashed', show_guide = T) +
        facet_wrap(~ experiment + treatment, scales = 'free', ncol = 1)
      
    }
    
    else if(input$correct == 'all') {

      df = allcorrect(df, input$phenos, c("experiment","facility","treatment"), 'line')

      
      if (input$linemeans == 'yes') { 
        df <- df%>%group_by(line,experiment,treatment)%>%summarise(adjval=mean(adjval,na.rm=T))
      }
      linedf = df[which(df$line %in% inputLines),]
      
      ggplot(data = df, aes(adjval, fill = treatment)) + 
        geom_histogram(binwidth = input$bins) + scale_x_continuous() +
        geom_vline(data = linedf, aes(xintercept=adjval, color = line), linetype = 'dashed', show_guide = T) +
        facet_wrap(~ experiment + treatment, scales = 'free', ncol = 1)
    }
}
  # This renders the histograms
  output$hist = renderPlot({
    if (input$go == 0)
      return()
    input$go
    isolate({
      df = allData
      buildHist(df)  
    })
  })
  
#   output$test = renderText({
#     df = allLines()
#     df = melt(df, id = c('line','experiment','treatment','facility','phenotype','individualPlant'))
#     df = cast(df, line+experiment+treatment+facility+individualPlant ~ phenotype)
#     inputLines = c(input$line, input$line2)
#     col = which(names(df) == input$phenos)
#     df = cbind(df[,1:5],df[,col])
#     df <- df[!is.na(df[,6]),] #don't mess with NAs
#     names(df)[6] = 'value'
#     linedf = df[which(df$line %in% inputLines),]
#     linedf
#     inputLines
#   })
  
  # This renders the data download link
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("linedata",Sys.Date(),".csv",sep="")
    },
    content = function(file) {
      df = allData
      df = allData
      inputLines = c(input$line,input$line2)
      col = which(names(df) == input$phenos)
      df = cbind(df[,1:5],df[,col])
      df <- df[!is.na(df[,6]),]
      names(df)[6] = 'value'
      df[which(df$line %in% inputLines),]
      write.csv(file=file,df)
    }
  )
    
  # This renders the PDF download link -- needs some work.
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("linePDF",Sys.Date(),".png",sep="")
    },
    content = function(file) {
      df = allData
      hist = buildHist(df)  
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 600, units = "in")
      ggsave(file, plot = hist, device = device)
    }
  )
})
