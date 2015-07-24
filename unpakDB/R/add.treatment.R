#
# inserts a facility into the database
#
insert.treatment <- function(con,
                             treat="control",
                             description="no manipulation"
                            )
{
  if (is.null(treat))
    {
      print("need to specify a treatment")
      NULL
    } else {
      if(is.null(which.match(con, "Treatment","name",treat)))
        {
          treat.id <-
            max(as.numeric(unlist(dbGetQuery(con,paste("SELECT idTreatment FROM Treatment")))))+1
          df <- dbGetQuery(con,"SELECT * FROM Treatment")
          df <- df[1,]
          df$idTreatment <- treat.id
          df$name <- treat
          df$description <- description
          sqlquery <- paste("insert into Treatment ",table2values(df),sep="")
          dbGetQuery(con,sqlquery)
#          print(sqlquery)
        } else {
          print("treatment already exists")
          NULL
        }
    }
}
