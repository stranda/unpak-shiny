#
# take an edited exp1a datasheet and put into forms that can be used in unpak db
#

upload1a <- function(con,insert_accession=FALSE)
  {
    fpath <- system.file("extdata", "expt1a1megasheet-edited-for-upload - Sheet1.csv", package="unpakR")
    mega <- read.csv(file=fpath,header=T,na.strings=c("."))
    mega.old <- mega
    comments <- paste("Flat;Row;Col:",apply(mega[,c(7,10,11,12)],1,paste,collapse="; "))
    mega <- mega[,c(2,3,6,8,9,14,15,16,17,18,19)]
    mega$description <- comments

#    assign plant IDS
# first find the highest plant number
    maxplantnum <- max(as.numeric(unlist(dbGetQuery(con,"SELECT idIndividualPlant FROM IndividualPlant"))))
    newmin <- maxplantnum+1
    newmax <- maxplantnum+dim(mega)[1]
    mega$plantnum <- seq(newmin,newmax)
    mega$line <- toupper(mega$line)

    #########################
    #error detection:
    #
    #make sure plantnums are new
    errcode <- 0
    if (length(which.match(con,"IndividualPlant","idIndividualPlant",mega$plantnum))>0)
      {
        print("somehow there are plantnums already in the database")
        errcode <- 1
      }
    
    matched.access <- which.match(con,"Accession","idAccession",mega$line)
    if ((errcode==0)&(length(matched.access)<dim(mega)[1]))
      {
        cat("some accessions are missing from database\nchange insert_acession to TRUE to add them to db\n")
        print(mega$line[-matched.access])
        errcode <- 2
        if (insert_accession==TRUE)
          {
            ins <- unique(data.frame(idAccession=mega$line[-matched.access]))
            sqlquery <- paste("INSERT INTO Accession ",column2values(ins))
            dbGetQuery(con,sqlquery)
          }
      }
    
    matched.facility <- which.match(con,"Facility","name",mega$facility)
    if ((errcode==0)&(length(matched.facility)<dim(mega)[1]))
      {
        cat("some facilities are missing from database\ninsert them before trying to add observations\n")
      print(unique(mega$facility[-matched.facility]))
        errcode <- 3
      }

    matched.treatment <- which.match(con,"Treatment","name",mega$treatment)
    if ((errcode==0)&(length(matched.treatment)<dim(mega)[1]))
      {
        cat("some treatments are missing from database\ninsert them before trying to add observations\n")
      print(unique(mega$treatment[-matched.treatment]))
        errcode <- 4
      }

    matched.institution <- which.match(con,"Institution","name",mega$institution)
    if ((errcode==0)&(length(matched.institution)<dim(mega)[1]))
      {
        cat("some institutions are missing from database\ninsert them before trying to add observations\n")
      print(unique(mega$institution[-matched.institution]))
        errcode <- 5
      }

    
    ##now lets reshape the mega dataframe
    megalong <- melt(mega,id.vars=c("experiment","treatment","line","institution","facility","description","plantnum"),measure.vars=c("days.to.bolt","diameter.at.bolt","inflorescence.height","fruitnum","aborted.fruits","avg.fruit.length"))
    for (i in c(1,2,4,5,8)) megalong[,i] <- as.character(megalong[,i])
    ##check to make sure phenotypes are present in db
    matched.phenotype <- which.match(con,"Phenotype","name",megalong$variable)
    if ((errcode==0)&(length(matched.phenotype)<dim(megalong)[1]))
      {
        cat("some phenotypes are missing from database\ninsert them before trying to add observations\n")
        print(unique(megalong$variable[-matched.phenotype]))
        errcode <- 6
      }

        
    if (errcode==0) #all clear
      {
        #convert the facility to facility ID
        tmp.df <- dbGetQuery(con,"SELECT * FROM Facility")
        tmp.df <- merge(tmp.df[,c("idFacility","name")],megalong,by.x="name",by.y="facility")
        megalong <- tmp.df[,-1] #get rid of the first column

        ##convert the experiment
        tmp.df <- dbGetQuery(con,"SELECT * FROM Experiment")
        tmp.df <- merge(tmp.df[,c("idExperiment","name")],megalong,by.x="name",by.y="experiment")
        megalong <- tmp.df[,-1] #get rid of the first column

        ##convert the institution 
        tmp.df <- dbGetQuery(con,"SELECT * FROM Institution")
        tmp.df <- merge(tmp.df[,c("idInstitution","name")],megalong,by.x="name",by.y="institution")
        megalong <- tmp.df[,-1] #get rid of the first column

               ##convert the treatment
        tmp.df <- dbGetQuery(con,"SELECT * FROM Treatment")
        tmp.df <- merge(tmp.df[,c("idTreatment","name")],megalong,by.x="name",by.y="treatment")
        megalong <- tmp.df[,-1] #get rid of the first column

                       ##convert the phenotype
        tmp.df <- dbGetQuery(con,"SELECT * FROM Phenotype")
        tmp.df <- merge(tmp.df[,c("idPhenotype","name")],megalong,by.x="name",by.y="variable")
        megalong <- tmp.df[,-1] #get rid of the first column
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
        df <- data.frame(IndividualPlant_idIndividualPlant=megalong$plantnum,
                         IndividualPlant_Facility_idFacility=megalong$idFacility,
                         IndividualPlant_Experiment_idExperiment=megalong$idExperiment,
                         Accession_idAccession=megalong$line)
        df <- unique(df)
        dbGetQuery(con,paste("INSERT INTO IndividualPlant_has_Accession",table2values(df)))
        ############################

        #############################
        # ok now update the observations
        ## important to note that I only insert observations where there are data in
        ##the original file.  Will have to be filled in with nulls/nas
        ##if wide format regenerated from the information in this table
        #############################
       # nms <- names(dbGetQuery(con,"SELECT * FROM Observation"))
        df <- data.frame(IndividualPlant_idIndividualPlant=megalong$plantnum,
                         IndividualPlant_Facility_idFacility=megalong$idFacility,
                         IndividualPlant_Experiment_idExperiment=megalong$idExperiment,
                         Phenotype_idPhenotype = megalong$idPhenotype,
                         date=rep("",length(megalong$idPhenotype)),
                         value=megalong$value,
                         comment=megalong$description)
        df <- unique(df)
        df <- df[!is.na(df$value),]
        dbGetQuery(con,paste("INSERT INTO Observation",table2values(df)))
        ############################
        
      }
    
    
  }
