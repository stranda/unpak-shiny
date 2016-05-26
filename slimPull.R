# Written By Nick Levitt 
# Script to pull all the Gene Ontology Slim Data

# Iterates through each non-excluded phenotype and builds a dataframe of:
#   slim, line, phenotype, value, treatment, experiment, and facility.
# Stores all the dataframes as a single list and saves them as RBF called 'allSlimData'

# Use load('allSlimData') to load them in to a new app/program/workspace.
# Then, call df = alldf[[i]], with i being the index of phenotype in question, ordered as they are in <phenoname>. 

source("global.R")
dbInfo = read.table('../dbInfo.txt')
con = dbConnect(MySQL(),dbname=toString(dbInfo[[1]]),user=toString(dbInfo[[2]]),password=toString(dbInfo[[3]]))
slims <- dbGetQuery(con,"SELECT distinct(GoSlim) FROM GeneOntology")
phenotbl <- dbGetQuery(con,"SELECT * FROM Phenotype")
phenoname <- phenotbl$name
phenoname <- phenoname[!(phenoname%in%phenotypes.to.exclude)]
phenoname <- c(phenoname[which(phenoname=="fruitnum")],phenoname[-which(phenoname=="fruitnum")])
alldf = list()
slimListIndx = c()

for (i in 1:length(phenoname)) {
  query <- paste("SELECT Go.GoSlim, Ga.Accession_idAccession, Ph.name, O.value, T.name, E.name, F.Name FROM GeneOntology Go",
                 " JOIN GeneAccession Ga ON Go.Gene_idGene = Ga.Gene_idGene",
                 " JOIN IndividualPlant Pl ON Ga.Accession_idAccession = Pl.Accession_idAccession",
                 " JOIN Observation O ON Pl.idIndividualPlant = O.IndividualPlant_idIndividualPlant",
                 " JOIN Phenotype Ph ON O.Phenotype_idPhenotype = Ph.idPhenotype",
                 " JOIN Experiment E ON Pl.Experiment_idExperiment = E.idExperiment",
                 " JOIN Treatment T ON O.Treatment_idTreatment = T.idTreatment",
                 " JOIN Facility F ON Pl.Facility_idFacility = F.idFacility",
                 " WHERE Ph.name = '", phenoname[i], "'",
                 sep="")
  
  df <- dbGetQuery(con,query)
  names(df) <- c("slim","line","phenotype","value","treatment","experiment", "facility")
  name = paste(phenoname[i], '_slims', sep='')
  slimListIndx[i] = name
  alldf[[i]] = df
  print(i)
}
save(alldf, file='allSlimData.rda')
