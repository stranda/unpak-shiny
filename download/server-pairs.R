source("global.R")
dbInfo = read.table('../../dbInfo.txt')


makeHTMLtable <- function (df)
{
    url.root.old <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="
    url.root.cloud <- "http://172.245.61.151:3838/findline/?line="
    url.root <- "http://107.170.89.221:3838/findline/?line="
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
        if (input$expt=="All"){expt=" "} else {expt=paste0(" WHERE E.name = '",input$expt,"'")}
      con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
      query <- paste("SELECT Pl.idIndividualPlant, Pl.Accession_idAccession, T.name, E.name, F.Name,",
                       " Ph.name, O.value",
                       " FROM Observation O",
                       " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                       " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                       " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                       " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                       " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility",
                       expt,
                       sep="")

        obstbl <- dbGetQuery(con,query)
        dbDisconnect(con)
        names(obstbl) <- c("PlantID","Accession","treatment","experiment","facility","Phenotype","value")
        if (input$linemeans=="yes")
            {
                ret <- with(obstbl,aggregate(cbind(value),by=list(Accession=Accession,
                                                              Experiment=experiment,
                                                              Facility=facility,
                                                              Phenotype=Phenotype,
                                                              Treatment=treatment),mean))
            } else {
                ret <- obstbl
                ret$Treatment=ret$treatment
                ret$Experiment=ret$experiment
                ret$Facility=ret$facility
            }
        if (input$wide=="yes")
            {
               names(ret)[grep("Phenotype",names(ret))] <- "variable"
               tmp <- cast(ret)
               ret <- tmp
            }
        ret <- ret[,!(names(ret)%in%c("treatment","experiment","facility"))]
        ret
    })
    
    output$msg <- renderText(
        {
            df <- values()
            if (dim(df)[1]<1)
                {
                    paste("No data present")
                } else {
                    paste(dim(df)[1],"rows and",dim(df)[2],"columns of data ready to download")
                }
        })
    

    output$downloadData <- downloadHandler(
     filename = function() {
         paste("phenotypes",Sys.Date(),".csv",sep="")
     },
        content = function(file) {
            df <- values()
            write.csv(file=file,row.names=F,df)
        }
        )
    
    output$pairplot <- renderPlot(
        {
            df <- values()
            if ("Phenotype" %in% names(df))
                {
                    names(df)[grep("Phenotype",names(df))] <- "variable"
                    df <- cast(df)
                }
            pairs(df[,!(names(df) %in% c("PlantID","Experiment","Facility","Treatment","Accession"))])
        })

     
     
 })
