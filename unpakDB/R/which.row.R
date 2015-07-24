#
# takes the name of a table, attribute, and vector of possible matches
#
which.match <- function(con,table="Accession",field="idAccession",match=c("SALK_CS60000","SALK_152970C"))
  {
    df <- dbGetQuery(con,paste("SELECT ",field," FROM ",table," WHERE ",field," IN ('",paste(match,collapse="','"),"')",sep=""))
    if(dim(df)[1]>0)
      {
        which(match%in%df[,names(df)==field[1]])
      } else
        {
          NULL
        }
  }
