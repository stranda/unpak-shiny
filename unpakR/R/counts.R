#functions the count stuff in the database

num.pheno <- function(con)
  {
    dim(dbGetQuery(con,"SELECT idPhenotype FROM Phenotype"))[1]
  }

num.plants <- function(con)
  {
    dim(unique(dbGetQuery(con,"SELECT idIndividualPlant from IndividualPlant")))[1]
  }

num.observations <- function(con)
{
  dim((dbGetQuery(con,"SELECT value from Observation")))[1]
}

num.lines <- function(con)
{
  dim(unique(dbGetQuery(con,"SELECT Accession_idAccession from IndividualPlant_has_Accession")))[1]
}
