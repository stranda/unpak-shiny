unpak.example.query <-
function(con)
  {
    dbGetQuery(con,"SELECT value,name
                    FROM Observation c
                    JOIN Phenotype a
                    ON a.idPhenotype=c.Phenotype_idPhenotype")
  }
