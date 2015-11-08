source("../global.R")
dbInfo = read.table('../../dbInfo.txt')


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  values <- reactive({
    con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
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
