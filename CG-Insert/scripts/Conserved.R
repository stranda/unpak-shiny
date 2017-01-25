##ConservedGroup Server

getConservedData <- function(){
  return(paste ('SELECT IDC.id AS "PlantID", IP.Accession_idAccession AS "Accession", IDC.ConservedGroup, E.name AS "Expt",F.name AS "Facility",T.name AS "Treatment", P.name AS "Phenotype", O.value AS "Value"',
                'FROM IndependentDataCache IDC',
                'JOIN IndividualPlant IP',
                'ON IDC.id = IP.idIndividualPlant',
                'JOIN Observation O',
                'ON IDC.id = O.IndividualPlant_idIndividualPlant',
                'JOIN Experiment E',
                'ON IP.Experiment_idExperiment = E.idExperiment',
                'JOIN Facility F',
                'ON IP.Facility_idFacility = F.idFacility',
                'JOIN Treatment T',
                'ON O.Treatment_idTreatment = T.idTreatment',
                'JOIN Phenotype P',
                'ON O.Phenotype_idPhenotype = P.idPhenotype',
                'WHERE IDC.ConservedGroup IS NOT NULL;',
                sep = ' '))
}