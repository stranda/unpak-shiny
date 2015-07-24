#
# inserts a phenotype into the database
#
insert.phenotype <- function(con,
                              phenotype="",
                              description="",
                              units="")
{
  if (is.null(phenotype))
    {
      print("need to specify an phenotype")
      NULL
    } else {
      if(is.null(which.match(con, "Phenotype","name",phenotype)))
        {
          phen.id <-
            max(as.numeric(unlist(dbGetQuery(con,paste("SELECT idPhenotype FROM Phenotype")))))+1
          df <- dbGetQuery(con,"SELECT * FROM Phenotype")
          df <- df[1,]
          df$idPhenotype <- phen.id
          df$name <- phenotype
          df$description <- description
          df$MeasurementUnit_name<- units
          sqlquery <- paste("insert into Phenotype ",table2values(df),sep="")
          dbGetQuery(con,sqlquery)
#          print(sqlquery)
        } else {
          print("phenotype already exists")
          NULL
        }
    }
}
