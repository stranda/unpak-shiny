makeHTMLtable <- function (df)
{
    url.root <- "http://arabidopsis.org/servlets/Search?type=germplasm&search_action=search&pageNum=1&search=Submit+Query&germplasm_type=individual_line&taxon=1&name_type_1=gene_name&method_1=2&name_1=&name_type_2=germplasm_phenotype&method_2=1&name_2=&name_type_3=germplasm_stock_name&method_3=4&name_3="

    df$Accession <- paste0("<a href='",url.root,df$Accession,"' target='_blank'>",df$Accession,"</a>")
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
