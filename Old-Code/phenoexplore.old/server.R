source("global.R")

makeHTMLtable <- function (df)
{
    url.root.old <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="
    url.root.cloud <- "http://172.245.61.151:3838/findline/?line="
    url.root <- "http://107.170.146.165:3838/findline/?line="
    df$Accession <- paste0("<a href='",url.root,df$Accession,"' target='_blank'>",df$Accession,"</a>")
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
     query <- paste("SELECT O.value, Pl.Accession_idAccession, T.name FROM Observation O",
                    " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                    " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                    " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                    " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                    " WHERE E.name = '",input$expt,"' AND",
                          " Ph.name = '",input$pheno,"' AND",
                          " T.name = '",input$treat,"'",
                    sep="")

     obstbl <- dbGetQuery(con,query)
     if (dim(obstbl)[1]>0)
         {
             if (input$linemeans=="yes")
                 {
                     ret <- with(obstbl,aggregate(cbind(value),by=list(Accession=Accession_idAccession,Treatment=name),mean))
                 } else {
                     ret <- obstbl
                     ret$Treatment=ret$name
                     ret$Accession=ret$Accession_idAccession
                 }
             ret <- ret[,c("value","Treatment","Accession")]
             ret <- ret[complete.cases(ret),]
             ret <- ret[order(ret$value),]
         } else {
             ret <- data.frame(value=NA,Treatment=NA,Accession=NA)
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
 
 output$hist <- renderPlot(
     {
         df <- values()

         left <- (abs(diff(range(df$value)))*input$range[1])+min(df$value)
         right <- (abs(diff(range(df$value)))*input$range[2])+min(df$value)

         hist(df$value,xlab="",main=paste(input$expt,input$treat,input$pheno))
         rug(df$value)
         abline(v=left,col="green",lwd=2)
         abline(v=right,col="red",lwd=2)
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
         makeHTMLtable(df)
     })
 
})
