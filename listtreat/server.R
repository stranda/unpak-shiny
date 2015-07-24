source("../global.R")


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  values <- reactive({
    con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
    query <- paste("SELECT * FROM Treatment")
    
    obstbl <- dbGetQuery(con,query)
    if (dim(obstbl)[1]>0)
      {
        ret <- obstbl
      } else {
        ret <- NA
      }
    dbDisconnect(con)
    ret
  })

  output$linktable <- renderDataTable(
                                 {
                                   df <- values()
                                   df
                                 })
  
})
