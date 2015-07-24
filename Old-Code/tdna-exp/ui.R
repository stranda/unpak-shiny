source("global.R")

con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
#get some phenotype names
phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
#the next line only allow for experiments with phenotypes
expttbl <- unique(dbGetQuery(con,"SELECT E.name FROM Experiment E"))
dbDisconnect(con)


# Define UI for dataset viewer application
shinyUI(
#pageWithSidebar(
  fluidPage(theme="bootstrap.css",
  # Application title
    titlePanel("Find lines based on T-DNA Ratio"),
                                        #                   
    sidebarLayout(
      sidebarPanel(

          selectInput("expt", "Choose an experiment:", 
                    choices = c("All",sort(expttbl$name))),
        
          sliderInput("range", "Range of x-axis to sample",
                      min = 0, max = 1, value = c(0.1,0.2))
        
        ),
        
  
#Render the results
      mainPanel(
        textOutput("msg"),
        
        downloadLink('downloadData','Download CSV of selection'),
        
        tabsetPanel(
          tabPanel("tDNA Distribution", plotOutput("hist")), 
          tabPanel("Lines", htmlOutput("linktable"))
          )
        )
      )
    )
  )
