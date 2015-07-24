source("global.R")
source("distexp.R")


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output, session) {
    
    values <- reactive({

        cdata <- session$clientData
        tmp <- strsplit(cdata$url_search,"&")[[1]]
        tmp <- tmp[grep("line",tmp)]
        if(length(tmp)>0) {
            url_line=strsplit(tmp,"=")[[1]][2]
            updateTextInput(session=session,inputId="line",value=url_line)
        }

        con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
         lines <- unique(dbGetQuery(con,"SELECT idAccession FROM Accession"))
         if (toupper(input$line) %in% lines$idAccession) #this tests the text input against the db before running query (insulate from sql inject)
             {
                 query <- paste("SELECT O.value, Ph.name, Pl.Accession_idAccession, T.name, E.name, F.name FROM Observation O",
                                " JOIN IndividualPlant Pl ON O.IndividualPlant_idIndividualPlant = Pl.idIndividualPlant",
                                " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                                " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                                " JOIN Facility F ON F.idFacility = Pl.Facility_idFacility",
                                " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                                        #                            " WHERE Pl.Accession_idAccession = '",toupper(input$line),"'",
                                sep="")
                 
                 obstbl <- dbGetQuery(con,query)
                 names(obstbl) <- c("value","phenotype","line","treatment","experiment","facility")
                 if (input$includeall!="yes") obstbl <- obstbl[!(obstbl$phenotype%in%phenotypes.to.exclude),]
                 udf <- unique(obstbl[obstbl$line==input$line,c("experiment","phenotype","treatment")])
                 obstbl <- merge(obstbl,udf)

                 if (dim(obstbl)[1]>0)
                     {
                         ret <- obstbl
                         ret <- ret[complete.cases(ret),]
                     } else {
                         ret <- NULL
                     }
             } else {
                 ret <- NULL
             }
         dbDisconnect(con)
         ret
     })
    
 output$msg <- renderText(
     {
         url.root <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="
         df <- values()
         if (is.null(df))
             {
                 paste("This line",input$line,"is not in the db  Try another one")
             } else {
                 HTML(paste0("Go to TAIR for Line: <a href='",url.root,input$line,"' target='_blank'>",input$line,"</a>"))
                 
             }
     })
 

 output$overview <- renderTable(
     {
         df <- values()
         df <- df[df$line==toupper(input$line),]
#         len <- with(df,aggregate(cbind(n=value),
#                                  by=list(phenotype=phenotype,experiment=experiment,facility=facility),
#                                  length))
#         df <- merge(df,len,all.x=T)
#         df <- df[order(df$phenotype,df$experiment,df$treatment,df$facility),]
#         unique(df[,c("line","phenotype","experiment","treatment","facility","n")])
         unique(df[,c("line","phenotype","experiment","treatment","facility","value")])
     })

 plot.dimensions <- function()
     {
         df <- values()
         if (is.null(df)) {c(1,1)} else
             {
                 numplots <- dim(unique(df[,c("experiment","phenotype","treatment")]))[1]
                 if ((numplots%%3)==0)
                     {
                         cols=3
                     } else {
                         cols=2
                     }
                 rows <- ifelse((numplots%%cols)==0,numplots%/%cols,(numplots%/%cols)+1)
                 c(rows,cols)
             }
     }
 
 output$exptplot <- renderPlot(
     {
          df <- values()
          par(mfrow=plot.dimensions())
          for (e in unique(df$experiment))
              distexp(df,input$line,expt=e,linemeans=input$linemeans=="yes",correct=input$correct)
          
     },height=function(){plot.dimensions()[1]*250},width=function(){plot.dimensions()[2]*300})
})
