library(shiny)
library(unpakR)

#set up a connection to database
con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
#get some phenotype names
phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
expttbl <- dbGetQuery(con,"SELECT * FROM Experiment")

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Test Database"),
  
  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
      textInput("caption", "Caption:", "Data Summary"),
      
      selectInput("pheno", "Choose a phenotype:", 
                  choices = phenotbl$name),
      
      selectInput("expt", "Choose an experiment:", 
                  choices = expttbl$name),
      
      radioButtons("linemeans", "Report line means?",
                   c("Yes" = "yes",
                     "No" = "no"))
      
#    numericInput("obs", "Number of observations to sample:", 10)
  ),
  
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("caption")), 
    
    verbatimTextOutput("summary"), 
    
    plotOutput("bwp")
  )
))
