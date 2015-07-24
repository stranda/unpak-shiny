source("global.R")

con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
#get some phenotype names
phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
#the next line only allow for experiments with phenotypes
expttbl <- unique(dbGetQuery(con,"SELECT E.name FROM Experiment E JOIN IndividualPlant IP ON IP.Experiment_idExperiment = E.idExperiment JOIN Observation O ON IP.idIndividualPlant = O.IndividualPlant_idIndividualPlant"))
treattbl <- dbGetQuery(con,"SELECT * FROM Treatment")
phenoname <- phenotbl$name
phenoname <- phenoname[!(phenoname%in%phenotypes.to.exclude)]
phenoname <- c(phenoname[which(phenoname=="fruitnum")],phenoname[-which(phenoname=="fruitnum")])


treatname <- treattbl$name
treatname <- c(treatname[which(treatname=="control")],treatname[-which(treatname=="control")])
dbDisconnect(con)


# Define UI for dataset viewer application
shinyUI(
    pageWithSidebar(  
  # Application title
        headerPanel("Find lines based on phenotype"),
        sidebarPanel(
            selectInput("pheno", "Choose a phenotype:", 
                        choices = phenoname),
            
            selectInput("expt", "Choose an experiment:", 
                        choices = sort(expttbl$name)),
            
            selectInput("treat", "Choose a treatment:", 
                        choices = treatname),
            
#            radioButtons("includeall", "Include 'minor' phenotypes",
#                         c("Yes" = "yes",
#                           "No" = "no")),

            radioButtons("linemeans", "Report line means?",
                         c("Yes" = "yes",
                           "No" = "no")),
            
            sliderInput("range", "Range of x-axis to sample",
                        min = 0, max = 1, value = c(0.1,0.2))
            ),
        
  
#Render the results
        mainPanel(
            textOutput("msg"),
            tabsetPanel(
                tabPanel("Phenotypic Distribution", plotOutput("hist")), 
                tabPanel("Lines", htmlOutput("linktable"))
                )
            )
        )
    )
