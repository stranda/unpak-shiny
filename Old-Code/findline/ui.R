source("../global.R")
library(ggplot2)

#con <- dbConnect(MySQL(),dbname="unpak",user="unpak",password="thaliana")
#get some phenotype names
#lines <- dim(unique(dbGetQuery(con,"SELECT IP.Accession_idAccession FROM IndividualPlant IP JOIN Observation O ON IP.idIndividualPlant = O.IndividualPlant_idIndividualPlant")))

# Define UI for dataset viewer application
shinyUI(
 fluidPage(
   titlePanel("Phenotypes from line id"),
   sidebarLayout(fluid=F, position="left",
     sidebarPanel(
       textInput("line", "Enter an accession:", "CS70000"),
       radioButtons("includeall", "Include all phenotypes?",
                    c("No" = "no",
                      "Yes" = "yes")),
       radioButtons("linemeans", "Report line means?",
                    c("Yes" = "yes",
                      "No" = "no")),
       radioButtons("correct", "Correct for among facility\nvariation using",
                    c("Nothing (raw data)" = "none",
                      "Means of all plants" = "all",
                      "Means of phytometers" = "phyt"
                      ))            
       ),
    
                                       #Render the results
    mainPanel(
      htmlOutput("msg")
      ,
      downloadLink('downloadData','Download CSV of phenotypes for selected line')
      ,
      br()
      ,
      downloadLink('downloadPDF','Download PDF of plot below')
      ,
      tabsetPanel(
        tabPanel("Plot", plotOutput("exptplot",height="auto",width="auto")),
        tabPanel("Phenotypic values, treatments, facilities:", tableOutput("overview"))
        
        )
      )
    ),
   title="Phenotypes from line id",
   theme="bootstrap.css"
   )
  )
