#Partial Authorship by Nick Levitt
source("../global.R")
library(ggplot2)

dbInfo = read.table('../../dbInfo.txt')
con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
#get some phenotype names
phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
phenoname <- phenotbl$name
phenoname <- phenoname[!(phenoname%in%phenotypes.to.exclude)]
phenoname <- c(phenoname[which(phenoname=="fruitnum")],phenoname[-which(phenoname=="fruitnum")])
dbDisconnect(con)

# NEW UI WITH NAVBAR
shinyUI(
  navbarPage("Phenotype Explore",
             tabPanel("Scatter Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pheno1", "Choose phenotype 1:", 
                                      choices = phenoname),                               
                          selectInput("pheno2", "Choose phenotype 2:", 
                                      choices = phenoname),                                
                          uiOutput('experiments'),
                           uiOutput('treat')
                         ),                                 
                        #Render the results
                         mainPanel(
                          textOutput("msg"),
                          textOutput("debug"),  
                          tabPanel("Phenotypic Distribution",
                                   downloadLink('downloadPDF','Download PNG of plot below'),
                                   plotOutput("scatter", brush = "plot_brush"),
                                   h3(verbatimTextOutput('displaySelectInfo')),
                                   textOutput('SelectInfo'),
                                   textOutput('SelectInfo2'),
                                   h3(textOutput('correlation')),
                                   h3(textOutput("statText")),
                                   div(tableOutput("stats"), style = "font-size:80%")
                                    )                            
                        )
                      )           
              ),    
             tabPanel("Lines",
                      downloadLink('downloadData','Download CSV of selection'),
                       dataTableOutput("selectedTable")
            )                   
  )
)