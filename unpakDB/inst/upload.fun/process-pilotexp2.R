#
## read in the data from experiment 2 from the pre-unpak days
#
insert.preunpak2(con,insert_accession=FALSE)
{
    fpath <- system.file("extdata", "cofc-second arabidopsis experiment - Sheet 1.csv", package="unpakR")
    exp2 <- read.csv(file=fpath,header=T,na.strings=c(".","NA","0 n/a"))
    names(exp2)[9] <- "leaf.number"
    ###
    #    assign plant IDS
# first find the highest plant number
    maxplantnum <- max(as.numeric(unlist(dbGetQuery(con,"SELECT idIndividualPlant FROM IndividualPlant"))))
    newmin <- maxplantnum+1
    newmax <- maxplantnum+dim(exp2)[1]
    exp2$plantnum <- seq(newmin,newmax)


    exp2.long <- melt(exp2,id.vars=c("plantnum","line","treattype"),
                      measured.vars=c("germinated","biomass","fruitnum",
                        "aborted.fruits","max.silique","inflorescence.height",
                        "leaf.number","diameter.at.bolt","alive.at.harvest"))
    exp2.long <- exp2.long[!is.na(exp2.long$value),]
    for (i in c(2:4)) exp2.long[,i] <- as.character(exp2.long[,i])
    exp2.long$line <- toupper(gsub(" ","",exp2.long$line))
    exp2.long$line <- gsub("CS60000","SALK_CS60000",exp2.long$line)
    #create the experiment field.  Right now, there are different "experiments"
    #for the different treatments: 2=control, 6=highwater, 7=hinutruients
    #these experiment ids were already in the database
    exp2.long$idExperiment <- ifelse(exp2.long$treattype=="control",2,
                                     ifelse(exp2.long$treattype=="highwater",
                                            6,7))
    #Create the facility field.  The entire experiment was conducted in the old greenhouse
    exp2.long$idFacility <- 1
###
    #create the institution field.  The entire exp was conducted at cofc
    exp2.long$idInstitution <- 1
    ### check to make sure that the lines are present

    
    errcode=0
    matched.access <- which.match(con,"Accession","idAccession",exp2.long$line)
    if ((errcode==0)&(length(matched.access)<dim(exp2.long)[1]))
      {
        cat("some accessions are missing from database\nchange insert_acession to TRUE to add them to db\n")
        print(unique(exp2.long$line[-matched.access]))
        errcode <- 1
        if (insert_accession==TRUE)
          {
            ins <- unique(data.frame(idAccession=exp2.long$line[-matched.access]))
            sqlquery <- paste("INSERT INTO Accession ",column2values(ins))
            dbGetQuery(con,sqlquery)
          }
      }

    #check that the phenotypes are in the database
        ##check to make sure phenotypes are present in db
    matched.phenotype <- which.match(con,"Phenotype","name",exp2.long$variable)
    if ((errcode==0)&(length(matched.phenotype)<dim(exp2.long)[1]))
      {
        cat("some phenotypes are missing from database\ninsert them before trying to add observations\n")
        print(unique(exp2.long$variable[-matched.phenotype]))
        errcode <- 6
      }
    
    ##convert the phenotype
        tmp.df <- dbGetQuery(con,"SELECT * FROM Phenotype")
        tmp.df <- merge(tmp.df[,c("idPhenotype","name")],exp2.long,by.x="name",by.y="variable")
        exp2.long <- tmp.df[,-1] #get rid of the first column

################################################insert phenotypic data
    if (FALSE){
        #############################
        # ok now update the IndividualPlant table
        # the information comes from the plannum and
        # facility and experiment cols
        #############################
#        nms <- names(dbGetQuery(con,"SELECT * FROM IndividualPlant"))
        df <- data.frame(idIndividualPlant=exp2.long$plantnum,
                         Facility_idFacility=exp2.long$idFacility,
                         Experiment_idExperiment=exp2.long$idExperiment)
        df <- unique(df)
        dbGetQuery(con,paste("INSERT INTO IndividualPlant",table2values(df)))
        ############################

        #############################
        # ok now update the IndividualPlant_has_Accession table
        # the information comes from the plantnum and line
        # facility and experiment cols
        #############################
 #       nms <- names(dbGetQuery(con,"SELECT * FROM IndividualPlant_has_Accession"))
        df <- data.frame(IndividualPlant_idIndividualPlant=exp2.long$plantnum,
                         IndividualPlant_Facility_idFacility=exp2.long$idFacility,
                         IndividualPlant_Experiment_idExperiment=exp2.long$idExperiment,
                         Accession_idAccession=exp2.long$line)
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
        df <- data.frame(IndividualPlant_idIndividualPlant=exp2.long$plantnum,
                         IndividualPlant_Facility_idFacility=exp2.long$idFacility,
                         IndividualPlant_Experiment_idExperiment=exp2.long$idExperiment,
                         Phenotype_idPhenotype = exp2.long$idPhenotype,
                         date=rep("",length(exp2.long$idPhenotype)),
                         value=exp2.long$value,
                         comment=rep("",length(exp2.long$idPhenotype)))
        df <- unique(df)
        df <- df[!is.na(df$value),]
        dbGetQuery(con,paste("INSERT INTO Observation",table2values(df)))
        ############################

    }
}
