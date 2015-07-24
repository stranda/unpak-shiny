#
# inserts a facility into the database
#
insert.facility <- function(con,
                            facility,
                            description="",
                            size=1,
                            location="",
                            type="greenhouse",
                            institution="College of Charleston")
{
  if (is.null(facility))
    {
      print("need to specify a facility")
      NULL
    } else {
      if(!is.null(which.match(con, "Institution","name",institution)))
        {
          if (is.null(which.match(con, "Facility","name",facility)))
            {
              inst.id <-
                as.numeric(unlist(dbGetQuery(con,paste("SELECT idInstitution FROM Institution WHERE name='",institution,"'",sep=""))))

              fac.ids <-
                as.numeric(unlist((dbGetQuery(con,paste("SELECT idFacility FROM Facility")))))
              next.fac <- sort(fac.ids,decreasing=TRUE)[1] + 1
              active <- 1
              dbGetQuery(con,paste("insert into Facility (",paste(unpak.db.structure(con)$Facility$fields,collapse=", "),
                    ") values (",next.fac,",",inst.id,",",singquote(facility),",",singquote(description),",",
                    size,",",active,",",singquote(location),",",singquote(type),")",sep=""))
            } else {
              print("facility already exists")
              NULL
            }
        } else {
          print("institution does not exist")
          NULL
        }
    }
}
