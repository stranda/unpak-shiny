#
# inserts a facility into the database
#
insert.institution<- function(con,
                              institution="College of Charleston",
                              abbreviation="C of C",
                              state="SC",
                              city="Charleston",
                              dateJoined="2011-04-01",
                              dateLeft="0000-00-00",
                              country="US",
                              description=""
                            )
{
  if (is.null(institution))
    {
      print("need to specify an insitution")
      NULL
    } else {
      if(is.null(which.match(con, "Institution","name",institution)))
        {
          inst.id <-
            max(as.numeric(unlist(dbGetQuery(con,paste("SELECT idInstitution FROM Institution")))))+1
          df <- dbGetQuery(con,"SELECT * FROM Institution")
          df <- df[1,]
          df$idInstitution <- inst.id
          df$name <- institution
          df$abbreviation <- abbreviation
          df$state <- state
          df$city <- city
          df$dateJoined <- dateJoined
          df$dateLeft <- dateLeft
          df$country <- country
          df$description <- description
          sqlquery <- paste("insert into Institution ",table2values(df),sep="")
          dbGetQuery(con,sqlquery)
#          print(sqlquery)
        } else {
          print("institution already exists")
          NULL
        }
    }
}
