#
# inserts a facility into the database
#
insert.experiment <- function(con,
                            experiment,
                            description="",
                            startDate="",
                              endDate="",
                              embargoed=1,
                              treatment="")
  {
    if (is.null(experiment))
      {
        print("need to specify an experiment")
        NULL
      } else {
        if(!is.null(which.match(con, "Treatment","name",treatment)))
          {
            if (is.null(which.match(con, "Experiment","name",experiment)))
              {
                treat.id <-
                  as.numeric(unlist(dbGetQuery(con,paste("SELECT idTreatment FROM Treatment WHERE name='",treatment,"'",sep=""))))
                
                exp.ids <-
                  max(as.numeric(unlist((dbGetQuery(con,paste("SELECT idExperiment FROM Experiment"))))))+1
                df <- dbGetQuery(con,"SELECT * FROM Experiment")
                df <- df[1,]
                df$idExperiment=exp.ids
                df$name <- experiment
                df$startDate <- startDate
                df$endDate <- endDate
                df$description <- description
                df$embargoed <- embargoed
                df$Treatment_idTreatment <- treat.id
                sqlquery <- paste("insert into Experiment ",table2values(df),sep="")
                dbGetQuery(con,sqlquery)

              } else {
                print("experiment already exists")
                NULL
            }
          } else {
            print("treatment does not exist")
            NULL
          }
      }
  }
