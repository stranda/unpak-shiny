library(shiny)
# library(xtable)
library(unpakR)
library(reshape)
library(RMySQL)
library(dplyr)
library(adjustPhenotypes)
library(ggplot2)

# options(bitmapType='cairo')
type <- getOption("bitmapType")

makeHTMLtable <- function (df,session) {
  url.root = paste0(sep = '',"http://",session$clientData$url_hostname,":",
                    session$clientData$url_port,'/findline/?line=')
  df$Accession <- paste0("<a href='",url.root, df$Accession, "' target='_blank'>",df$Accession,"</a>")
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


phenotypes.to.exclude <- c("FruitLength1","FruitLength2","FruitLength3","FruitLength4",
                           "FruitLength5","FruitLength6","FruitLength7","FruitLength8",
                           "germinants.31d","germinants.41d","germinants.7d","germinants.14d",
                           "seeds.sown","basalfruit.length","midfruit.length","upperfruit.length")

getConnection <- function(group) {
  if (!exists('.connection', where=.GlobalEnv)) {
    .connection <<- dbConnect(MySQL(), group=group)
  } else if (class(try(dbGetQuery(.connection, "SELECT 1"))) == "try-error") {
    dbDisconnect(.connection)
    .connection <<- dbConnect(MySQL(), group=group)
  }  
  return(.connection)
}
