source("../global.R")
library(ggplot2)


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session) {

 values <- reactive({
     con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
     query <- paste("SELECT * FROM TDNARatio",
                    sep="")

     obstbl <- dbGetQuery(con,query)
     names(obstbl)[1] <- "Accession"
     dbDisconnect(con)
     obstbl <- obstbl[,-dim(obstbl)[2]]
     obstbl
  })
  
 output$msg <- renderText(
     {
         df <- values()
         if (dim(df[complete.cases(df),])[1]==0)
             {
               "No data in TDNARatio table"
             } else {
               paste (dim(df)[1],"lines with TDNA ratio data")
             }
     })
 
 buildHist = function(df, select) {
   
   left <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[1])+min(log(df$AreaRatio,base=10))
   right <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[2])+min(log(df$AreaRatio,base=10))
   pheno = input$Phenotype
   if (!select) {
     xxx = ggplot(data = df, aes(log(base = 10,AreaRatio)))
     xxx + geom_histogram() + geom_rug() + 
       geom_vline(xintercept=left,  colour='red',
                  linetype="dashed", size=1) +
       geom_vline(xintercept=right,  colour='blue',
                  linetype="dashed", size=1) + ggtitle("Distribution for all samples")
   }
   else {
     df <- df[(log(df$AreaRatio,base=10)>=left)&(log(df$AreaRatio,base=10)<=right),]
     
     validate(
       need(nrow(df) > 0, "Please select data using the sliders on the left")
     )
     
     xxx = ggplot(data=df,aes(log(base = 10,AreaRatio)))
     xxx + geom_histogram() + geom_rug() + ggtitle("Distribution for selected samples")
   }
 }
 
  output$hist <- renderPlot({
    df <- values()
    buildHist(df, FALSE)
  })

  output$selectHist = renderPlot({
    df <- values()
    buildHist(df, TRUE) 
})

  output$downloadPlot = downloadHandler(
    filename = function() {
      paste("TDNARange",Sys.Date(),".png",sep="")
    },
    content = function(file) {
      df <- values()

      plot1 = buildHist(df, FALSE)
      plot2 = buildHist(df, TRUE)
      png(file)
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 600, units = "in")
      plots = grid.arrange(plot1, plot2, ncol=1)
      dev.off()
      
    }
  )
 output$linktable <- renderDataTable(
     {
         df <- values()

         left <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[1])+min(log(df$AreaRatio,base=10))
         right <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[2])+min(log(df$AreaRatio,base=10))

         df <- df[(log(df$AreaRatio,base=10)>=left)&(log(df$AreaRatio,base=10)<=right),]
         df
     })

 output$stats = renderTable ({
   df = values()
   left <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[1])+min(log(df$AreaRatio,base=10))
   right <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[2])+min(log(df$AreaRatio,base=10))
   df <- df[(log(df$AreaRatio,base=10)>=left)&(log(df$AreaRatio,base=10)<=right),]
   
   validate(
     need(nrow(df) > 0, "Please select data using the sliders on the left")
   )
   numSamples = nrow(df)
   avgEndo = mean(df[,3])
   avgTDNA = mean(df[,4])
   avgRatio = mean(df[,5])
   
   statistics = data.frame(numSamples,avgEndo,avgTDNA,avgRatio)
    names(statistics) = c('Num Samples', 'Avg EndoArea', 'Avg TDNAArea', 'Avg AreaRatio')
   
   statistics

   
 })

 output$downloadData <- downloadHandler(
                                        filename = function() {
                                          paste("TDNARange",Sys.Date(),".csv",sep="")
                                        },
                                        content = function(file) {
                                          df <- values()
                                          left <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[1])+min(log(df$AreaRatio,base=10))
                                          right <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[2])+min(log(df$AreaRatio,base=10))
                                          
                                          df <- df[(log(df$AreaRatio,base=10)>=left)&(log(df$AreaRatio,base=10)<=right),]

                                          write.csv(file=file,df)
                                        }
                                        )
 
})


