source("../global.R")

con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
#the next line only allow for experiments with phenotypes
expttbl <- unique(dbGetQuery(con,"SELECT E.name FROM Experiment E JOIN IndividualPlant IP ON IP.Experiment_idExperiment = E.idExperiment JOIN Observation O ON IP.idIndividualPlant = O.IndividualPlant_idIndividualPlant"))
dbDisconnect(con)


# Define UI for dataset viewer application
shinyUI(
#pageWithSidebar(
  fluidPage(theme="bootstrap.css",
  # Application title
    titlePanel("Download phenotypic data by experiment"),
                                        #                   
    sidebarLayout(
      sidebarPanel(
        selectInput("expt", "Choose an experiment:", 
                    choices = c("All",sort(expttbl$name))),
        
        radioButtons("linemeans", "Download line means? (choosing no gives raw data for each plant grown)",
                     c("No" = "no",
                       "Yes" = "yes")),

          radioButtons("wide", "Wide format? (different phenotypes in different columns, otherwise values with indicator column)",
                     c("Yes" = "yes",
                       "No" = "no"))
        ),
        
  
#Render the results
      mainPanel(
          textOutput("msg"),
          downloadLink('downloadData','Download CSV file of data')
#          ,
#          plotOutput("pairplot")
        )
      )
    )
  )
