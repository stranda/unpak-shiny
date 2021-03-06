source("../global.R")
source('adjust-pheno.R')
library(ggplot2)
dbInfo = read.table('../../dbInfo.txt')

#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
  allData <- reactive({    
    cdata <- session$clientData
    tmp <- strsplit(cdata$url_search,"&")[[1]]
    tmp <- tmp[grep("line",tmp)]
    if(length(tmp)>0) {
      url_line=strsplit(tmp,"=")[[1]][2]
      updateTextInput(session=session,inputId="line",value=url_line)
    }
    
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
#    lines <- unique(dbGetQuery(con,"SELECT idAccession FROM Accession"))
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
#    allData <- melt(ret, id = c('line','experiment','treatment','facility','phenotype','individualPlant'))
#    allData <- cast(allData, line+experiment+treatment+facility+individualPlant ~ phenotype)
#    lineNames <- unique(allData$line)
#    allData
    ret
  })

  allLineNames <- reactive({
      ln <- unique(allData()$line)
      if (length(ln)>0) {ln} else {""}
  })

  focalLines <- reactive({
       con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
       lines <- unique(unlist(c(dbGetQuery(con,paste("SELECT Accession_idAccession FROM GeneAccession WHERE Gene_idGene='",input$gene,"'",sep="")))))
       cons<-dbListConnections(MySQL())
       for(con in cons)
           dbDisconnect(con) 
       if (length(lines)>0) {lines} else {NA}
   })

  
  poss <- reactive({
      con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
      phenos <- sort(unique(unlist(c(dbGetQuery(con,"SELECT name FROM Phenotype")))))
      cons<-dbListConnections(MySQL())
      for(con in cons)
          dbDisconnect(con)

      
      
      
      if (length(phenos)>0) {phenos} else {""}
  })
  
  
  # Renders the User interface for selecting which phenotype to use
  output$phenos = renderUI({
     selectizeInput("phenos", "Choose phenotype(s):", 
                choices = c(poss()),selected = poss()[poss()=="fruitnum"],multiple=TRUE)  
  })
    
  # This gives the warning message if the line is not in the DB, or links to it if it is
  output$msg <- renderText({
      url.root <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="
      fl <- focalLines() %in% allLineNames()
      if ((is.na(focalLines())|(sum(as.numeric(!fl))>0)))
          {
              paste("This gene",input$line,"is not in the db, Try another one")
          } else {
              for (ln in focalLines())
                  HTML(paste0("Go to TAIR for Line: <a href='",url.root,ln,"' target='_blank'>",ln,"</a>"))         
          }
  })

### This function, given a dataframe, builds the histograms, breaking them up by experiment and treatment pairs.
  buildFinalData = function() {

    df <- allData()
    df <- df[df$phenotype %in% input$phenos,]
    df <- df[!is.na(df$value),] #don't mess with NAs

    if (input$correct == "phyt")  df = phytcorrect(df, input$phenos, c("experiment","facility","treatment"), 'line')
    if (input$correct == "all")  df  =  allcorrect(df, input$phenos, c("experiment","facility","treatment"), 'line')
    
    if (input$linemeans == 'yes') { #get means per line instead of actual observations
        df <- df%>%group_by(line,experiment,treatment,phenotype)%>%summarise(value=mean(value,na.rm=T))
    }
    df    
}
  
  # This renders the histograms
  output$hist = renderPlot({
      df <- buildFinalData()
      linedf = df[df$line %in% focalLines(),]
      lineSub <- unique(linedf[,c("experiment","phenotype","treatment")])
      df <- merge(df,lineSub,all.x=F)
      ggplot(data = df, aes(value, fill = treatment)) + 
          geom_histogram() +
              scale_colour_brewer(type="qual", palette=8) +
                  geom_vline(data = linedf, aes(xintercept = value, color = line), linetype = 'solid', show_guide = T) +
                      facet_wrap(~ phenotype + experiment + treatment, scales = 'free', ncol = 1)
  })
  
                             
  # This renders the interactive data table of the selected values
  output$overview = renderDataTable({
      df = buildFinalData()
      inputLines = focalLines()
      df <- df[df$line%in%inputLines,]
      df <- df[!is.na(df$value),]
      df
  } , options = list(lengthMenu = c(50, 100, 500), pageLength = 50))
  

  # This renders the data download link
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("linedata",Sys.Date(),".csv",sep="")
    },
    content = function(file) {
      df = buildFinalData()
      inputLines = focalLines()
      df <- df[df$line%in%inputLines,]
      df <- df[!is.na(value),]
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
