source("../global.R")
makeHTMLtable <- function (df)
{
    url.root.old <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="
    url.root.cloud <- "http://172.245.61.151:3838/findline/?line="
    url.root <- "http://linum.cofc.edu:3838/findline/?line="

    cols <- dim(df)[2]
    rows <- dim(df)[1]
    for (j in 1:cols)
        {
            if (is.numeric(df[,j])) df[,j] <- as.character(round(df[,j],3))
            if (is.factor(df[,j])) df[,j] <- as.character(df[,j],3)
        }

    str <- "<table border = 1>"
    str <- paste0(str,"<tr><th>",paste(names(df),collapse="</th><th>"),"</tr>\n")
    for (i in 1:rows) {
      str <- paste0(str, "<tr><td>",paste(df[i,],collapse="</td><td>"),"</tr>\n")
    }
    str    
}


#### Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  values <- reactive({
    con <- dbConnect(MySQL(),dbname="unpak",user="unpak-R",password="thaliana")
    query <- paste("SELECT * FROM Facility")
    
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
  
  output$msg <- renderText({
    paste("")
  })
  
  
  output$linktable <- renderDataTable(
{
  df <- values()
  df
})
  
})
