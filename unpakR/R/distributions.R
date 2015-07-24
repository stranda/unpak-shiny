#
# various tables that summarize the distribution of database features
#

summarize.unpak <- function(con)
  {
    tmp <- dbGetQuery(con,"SELECT ip.Accession_idAccession,ph.name,ph.MeasurementUnit_name,ob.value,
                                  tr.name
                       FROM Phenotype ph
                          JOIN Observation ob ON ph.idPhenotype=ob.Phenotype_idPhenotype
                          JOIN Experiment ex ON ob.IndividualPlant_Experiment_idExperiment=ex.idExperiment
                          JOIN Treatment tr ON ex.Treatment_idTreatment=tr.idTreatment
                          JOIN IndividualPlant_has_Accession ip ON ob.IndividualPlant_idIndividualPlant=ip.IndividualPlant_idIndividualPlant 
")
    names(tmp) <- c("line","phenotype","unit","value","treatment")
    mn <- with(tmp,aggregate(cbind(mean=value),by=list(line=line,phenotype=phenotype,treatment=treatment),mean,na.rm=T))
    ph.nm <- num.pheno(con)
    
    par(mfrow=c(ceiling(ph.nm/3),3))
    for(ph in unique(as.character(mn$phenotype)))
      boxplot(mean~treatment,data=mn[mn$phenotype==ph,],main=paste("Phenotype:",ph))
  }
