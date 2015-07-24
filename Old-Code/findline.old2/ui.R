source("global.R")

#con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
#get some phenotype names
#lines <- dim(unique(dbGetQuery(con,"SELECT IP.Accession_idAccession FROM IndividualPlant IP JOIN Observation O ON IP.idIndividualPlant = O.IndividualPlant_idIndividualPlant")))

# Define UI for dataset viewer application
shinyUI(
    pageWithSidebar(  
  # Application title
        headerPanel("Phenotypes from line id"),
        sidebarPanel(
            textInput("line", "Enter an accession:", "CS70000"),
            radioButtons("includeall", "Include all phenotypes?",
                         c("No" = "no",
                           "Yes" = "yes")),
            radioButtons("linemeans", "Report line means?",
                         c("Yes" = "yes",
                           "No" = "no")),
            radioButtons("correct", "Correct for among facility variation using",
                         c("Nothing (raw data)" = "none",
                           "Means of all plants" = "all",
                           "Means of phytometers" = "phyt"
                           ))            
            ),
        
  
#Render the results
        mainPanel(
            htmlOutput("msg")
            ,
            tabsetPanel(
                tabPanel("Plot", plotOutput("exptplot",height="auto",width="auto")),
                tabPanel("Phenotypic values, treatments, facilities:", tableOutput("overview"))

                )
            )
        )
    )

