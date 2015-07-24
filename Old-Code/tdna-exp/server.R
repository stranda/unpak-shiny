source("global.R")

makeHTMLtable <- function (df)
{
    url.root.tair <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="
    url.root.cloud <- "http://172.245.61.151:3838/findline/?line="
    url.root <- "http://107.170.146.165:3838/findline/?line="
    df$Accession <- paste0("<a href='",url.root.tair,df$Accession,"' target='_blank'>",df$Accession,"</a>")
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
shinyServer(function(input, output) {

 values <- reactive({
     con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
     query <- paste("SELECT * FROM TDNARatio",
                    sep="")

     obstbl <- dbGetQuery(con,query)
     names(obstbl)[1] <- "Accession"

     tdnatbl <- obstbl[,-dim(obstbl)[2]]

     if (input$expt=="All"){expt=" "} else {expt=paste0("WHERE E.name = '",input$expt,"'")}
     
     query <- paste("SELECT Pl.Accession_idAccession,  E.name  FROM IndividualPlant Pl",
                    " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment ",
                    expt,
                    sep="")

     obstbl <- dbGetQuery(con,query)
     names(obstbl)[1] <- c("Accession","Experiment")
     dbDisconnect(con)
#     ret <- merge(obstbl[,c("Accession","Experiment",)],tdnatbl)
     ret <- tdnatbl[tdnatbl$Accession %in% obstbl$Accession,]
     ret
  })
  
 output$msg <- renderText(
     {
         df <- values()
         paste(dim(df))
         if (dim(df[complete.cases(df),])[1]==0)
             {
               "No data in TDNARatio table"
             } else {
               paste (dim(df)[1],"lines with TDNA ratio data")
             }
     })
 
 output$hist <- renderPlot(
     {
         df <- values()

         left <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[1])+min(log(df$AreaRatio,base=10))
         right <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[2])+min(log(df$AreaRatio,base=10))

         hist(log(base=10,df$AreaRatio),xlab="log10(Ratio)",main=paste0("Distribution of end-point pcr ratios TDNA/PetC"))
         rug(log(df$AreaRatio,base=10))
         abline(v=left,col="green",lwd=2)
         abline(v=right,col="red",lwd=2)
     })

 output$linktable <- renderText(
     {
         df <- values()

         left <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[1])+min(log(df$AreaRatio,base=10))
         right <- (abs(diff(range(log(df$AreaRatio,base=10))))*input$range[2])+min(log(df$AreaRatio,base=10))

         df <- df[(log(df$AreaRatio,base=10)>=left)&(log(df$AreaRatio,base=10)<=right),]
         makeHTMLtable(df)
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
