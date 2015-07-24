#
# take an edited exp1a datasheet and put into forms that can be used in unpak db
#

upload1a <- function(con,insert_accession=FALSE)
  {
    fpath <- system.file("extdata", "CofC_S11Farm_LineList - Sheet1.csv", package="unpakDB")
    farm <- read.csv(file=fpath,header=T,na.strings=c("."))
    farm.old <- farm
    comments <- farm$comments
    farm$description <- comments
    farm$facility <- "NGH-COFC"
#    assign plant IDS
# first find the highest plant number
    maxplantnum <- max(as.numeric(unlist(dbGetQuery(con,"SELECT idIndividualPlant FROM IndividualPlant"))))
    newmin <- maxplantnum+1
    newmax <- maxplantnum+dim(farm)[1]
    farm$plantnum <- seq(newmin,newmax)
    farm$line <- toupper(farm$accession)
    farm$treatment <- "control"
    farm$institution <- "College of Charleston"
    farm$experiment <- "Summer11 Farm"
    #########################
    #error detection:
    #
    #make sure plantnums are new
    errcode <- 0
    if (length(which.match(con,"IndividualPlant","idIndividualPlant",farm$plantnum))>0)
      {
        print("somehow there are plantnums already in the database")
        errcode <- 1
      }
    
    matched.access <- which.match(con,"Accession","idAccession",farm$line)
    if ((errcode==0)&(length(matched.access)<dim(farm)[1]))
      {
        cat("some accessions are missing from database\nchange insert_acession to TRUE to add them to db\n")
        print(farm$line[-matched.access])
        errcode <- 2
        if (insert_accession==TRUE)
          {
            ins <- unique(data.frame(idAccession=farm$line[-matched.access]))
            sqlquery <- paste("INSERT INTO Accession ",column2values(ins))
            dbGetQuery(con,sqlquery)
          }
      }
    
    matched.facility <- which.match(con,"Facility","name",farm$facility)
    if ((errcode==0)&(length(matched.facility)<dim(farm)[1]))
      {
        cat("some facilities are missing from database\ninsert them before trying to add observations\n")
        print(unique(farm$facility[-matched.facility]))
        errcode <- 3
      }

    matched.treatment <- which.match(con,"Treatment","name",farm$treatment)
    if ((errcode==0)&(length(matched.treatment)<dim(farm)[1]))
      {
        cat("some treatments are missing from database\ninsert them before trying to add observations\n")
        print(unique(farm$treatment[-matched.treatment]))
        errcode <- 4
      }

    matched.institution <- which.match(con,"Institution","name",farm$institution)
    if ((errcode==0)&(length(matched.institution)<dim(farm)[1]))
      {
        cat("some institutions are missing from database\ninsert them before trying to add observations\n")
        print(unique(farm$institution[-matched.institution]))
        errcode <- 5
      }
    
    matched.experiment <- which.match(con,"Experiment","name",farm$experiment)
    if ((errcode==0)&(length(matched.experiment)<dim(farm)[1]))
      {
        cat("some experiments are missing from database\ninsert them before trying to add observations\n")
        print(unique(farm$experiment[-matched.experiment]))
        errcode <- 6
      }

        #############################
        # ok now update the IndividualPlant table
        # the information comes from the plannum and
        # facility and experiment cols
        #############################
#        nms <- names(dbGetQuery(con,"SELECT * FROM IndividualPlant"))
        df <- data.frame(idIndividualPlant=megalong$plantnum,
                         Facility_idFacility=megalong$idFacility,
                         Experiment_idExperiment=megalong$idExperiment)
        df <- unique(df)
        dbGetQuery(con,paste("INSERT INTO IndividualPlant",table2values(df)))
        ############################

        #############################
        # ok now update the IndividualPlant_has_Accession table
        # the information comes from the plantnum and line
        # facility and experiment cols
        #############################
 #       nms <- names(dbGetQuery(con,"SELECT * FROM IndividualPlant_has_Accession"))
        df <- data.frame(idIndividualPlant=farm$plantnum,
                         Facility_idFacility=2,
                         Experiment_idExperiment=5,
                         Accession_idAccession=farm$line,
                         plantnum_experiment=farm$plant_id)

        dbGetQuery(con,paste("INSERT INTO IndividualPlant",table2values(df)))
        ############################

        ############################
        
      }
    
    
  }
