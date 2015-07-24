#
# take an edited exp1a datasheet and put into forms that can be used in unpak db
#

upload.exp <- function(con,csvfile="",pedantic=T,insert_accession=FALSE,commit=F)
    {
        require(reshape)
        required.fields <- c("exptid", "accession", "institution",
                             "facility", "flat", "row", "column",
                             "parent_id", "treatment")
        
        mega <- read.csv(file=csvfile,header=T,na.strings=c("."))


        
        errcode <- 0
        if (!test.names(names=names(mega),required=required.fields)) {errcode <- 1}
        if (errcode==0)
            {

                maxplantnum <- largest.plantid(con)
                newmin <- maxplantnum+1
                newmax <- maxplantnum+dim(mega)[1]
                mega$plantnum <- seq(newmin,newmax)
                
                mega$plantnum_experiment <- with(mega,paste(flat,row,column,sep="-"))                
                                        #make sure lines are all upper case
                mega$accession <- toupper(mega$accession)
                if (!("comments"%in%names(mega))) {mega$comments=""} #put in blank comments if none exist
                
                ##convert mega to long format.  Should only have columns in the required fields and then phenotypes
                mega <- melt(mega,id.vars=c(required.fields,"plantnum","comments","plantnum_experiment"),variable_name="phenotype")     
            }

        if (errcode==0)
            {
                matched.access <- which.match(con,"Accession","idAccession",mega$accession)
                if (length(matched.access)<dim(mega)[1])
                    {
                        print("some accessions are missing from database\nchange insert_acession to TRUE to add them to db")
                        print("though all of the UnPAK plants should have parents in the db")
                        print("If you run with insert_accession=T, you will see this same error once, as accessions are added")
                        print("It should disappear subsequently")
                        print(mega$line[-matched.access])
                        errcode <- 2
                        if (insert_accession==TRUE)
                            {
                                ins <- unique(data.frame(idAccession=mega$accession[-matched.access]))
                                sqlquery <- paste("INSERT INTO Accession ",column2values(ins))
                                if(is.null(dbGetQuery(con,sqlquery)))
                                    {
                                        print("inserted these accessions in the DB:")
                                        print(ins)
                                    }
                            }
                    }
            }                
        if (errcode==0)
            {
                matched.facility <- which.match(con,"Facility","name",mega$facility)
                if (length(matched.facility)<dim(mega)[1])
                    {
                        cat("some facilities are missing from database\ninsert them before trying to add observations\n")
                        print(unique(mega$facility[-matched.facility]))
                        errcode <- 3
                    }
            }
        
        if (errcode==0)
            {
                matched.treatment <- which.match(con,"Treatment","name",mega$treatment)
                if (length(matched.treatment)<dim(mega)[1])
                    {
                        cat("some treatments are missing from database\ninsert them before trying to add observations\n")
                        print(unique(mega$treatment[-matched.treatment]))
                        errcode <- 4
                    }
            }

        if (errcode==0)
            {
                matched.experiment <- which.match(con,"Experiment","name",mega$exptid)
                if (length(matched.experiment)<dim(mega)[1])
                    {
                        cat("some treatments are missing from database\ninsert them before trying to add observations\n")
                        print(unique(mega$exptid[-matched.experiment]))
                        errcode <- 24
                    }
            }
        
        if (errcode==0)
            {
                matched.institution <- which.match(con,"Institution","abbreviation",mega$institution)
                if (length(matched.institution)<dim(mega)[1])
                    {
                        cat("some institutions are missing from database\ninsert them before trying to add observations\n")
                        print(unique(mega$institution[-matched.institution]))
                        errcode <- 5
                    }
            }

        if (errcode==0)
            {
                ##check to make sure phenotypes are present in db
                matched.phenotype <- which.match(con,"Phenotype","name",mega$phenotype)
                if (length(matched.phenotype)<dim(mega)[1])
                    {
                        cat("some phenotypes are missing from database\ninsert them before trying to add observations\n")
                        print(unique(mega$phenotype[-matched.phenotype]))
                        errcode <- 6
                    }
            }

        if (errcode==0) #make sure parents in this file are present in DB; if not these are new seeds
            {
                ##assuming that there is only one parent.
                mega$parent_id <- as.character(mega$parent_id)
                mega$Parent1 <- ifelse(is.na(mega$parent_id),"new_seeds",mega$parent_id)
                if ((pedantic)&(length(which(mega$Parent1=="new_seeds"))>0))
                    {
                        errcode=17
                        stop("if pedantic is set to true, every plant has to have an existing parent")
                    }
                p1 <- which.match(con,table="IndividualPlant","plantnum_experiment",match=mega$Parent1)
                if (length(p1)!=dim(mega)[1])
                    {
                        errcode = 14
                        print("Parents listed below need to be inserted into the db before inserting this spreadsheet")
                        if (length(p1)>0)
                            {
                                print(mega$Parent1[-p1])
                            } else
                                {
                                    print(mega$Parent1)
                                }
                    }
                
                mega$Parent2 <- mega$Parent1
                
                p2 <- which.match(con,table="IndividualPlant","plantnum_experiment",match=mega$Parent1)
                if (length(p2)!=dim(mega)[1])
                    {
                        errcode = 15
                        print("Parents listed below need to be inserted into the db before inserting this spreadsheet")
                        if (length(p2)>0)
                            {
                                print(mega$Parent2[-p2])
                            } else {print(mega$Parent2)}
                    }
                
            }
        
        
    if (errcode==0) #all clear
      {
        #convert the facility to facility ID
        tmp.df <- dbGetQuery(con,"SELECT * FROM Facility")
        tmp.df <- merge(tmp.df[,c("idFacility","name")],mega,by.x="name",by.y="facility")
        mega <- tmp.df[,-1] #get rid of the first column

        ##convert the experiment
        tmp.df <- dbGetQuery(con,"SELECT * FROM Experiment")
        tmp.df <- merge(tmp.df[,c("idExperiment","name")],mega,by.x="name",by.y="exptid")
        mega <- tmp.df[,-1] #get rid of the first column

        ##convert the institution 
        tmp.df <- dbGetQuery(con,"SELECT * FROM Institution")
        tmp.df <- merge(tmp.df[,c("idInstitution","abbreviation")],mega,by.x="abbreviation",by.y="institution")
        mega <- tmp.df[,-1] #get rid of the first column

               ##convert the treatment
        tmp.df <- dbGetQuery(con,"SELECT * FROM Treatment")
        tmp.df <- merge(tmp.df[,c("idTreatment","name")],mega,by.x="name",by.y="treatment")
        mega <- tmp.df[,-1] #get rid of the first column

                       ##convert the phenotype
        tmp.df <- dbGetQuery(con,"SELECT * FROM Phenotype")
        tmp.df <- merge(tmp.df[,c("idPhenotype","name")],mega,by.x="name",by.y="phenotype")
        mega <- tmp.df[,-1] #get rid of the first column


        ###find the individual plant ids for parents of these experimental plants
        ip <- dbGetQuery(con,"SELECT * FROM IndividualPlant")
        mega <- merge(mega,ip[,c("idIndividualPlant","plantnum_experiment")],by.x="Parent1",by.y="plantnum_experiment")
        mega$Parent1.num <- mega$idIndividualPlant
        mega <- mega[,which(names(mega)!="idIndividualPlant")]
        mega <- merge(mega,ip[,c("idIndividualPlant","plantnum_experiment")],by.x="Parent2",by.y="plantnum_experiment")
        mega$Parent2.num <- mega$idIndividualPlant
        mega <- mega[,which(names(mega)!="idIndividualPlant")]
        
        #############################
        # ok now update the IndividualPlant table
        # the information comes from the plannum and
        # facility and experiment cols
        #############################
#        nms <- names(dbGetQuery(con,"SELECT * FROM IndividualPlant"))
        ##    assign plant IDS
        ## first find the highest plant number
        
        df <- data.frame(idIndividualPlant=mega$plantnum,
                         Facility_idFacility=mega$idFacility,
                         Experiment_idExperiment=mega$idExperiment,
                         Parent1=mega$Parent1.num,
                         Parent2=mega$Parent2.num,
                         Accession_idAccession=mega$accession,
                         plantnum_experiment= mega$plantnum_experiment,
                         comment=mega$comments)
        df <- unique(df)
        print(paste("created a df of dim",dim(df)[1],"to insert in IndividualPlant"))
        if (commit)
            {
                if(is.null(dbGetQuery(con,paste("INSERT INTO IndividualPlant",table2values(df)))))
                    {
                        print(paste("inserted ",dim(df)[1],"records in IndividualPlant"))
                    }
                
            } else {
                print("did not write these data to database")
            }
        
        #############################
        # ok now update the observations
        ## important to note that I only insert observations where there are data in
        ##the original file.  Will have to be filled in with nulls/nas
        ##if wide format regenerated from the information in this table
        #############################
       # nms <- names(dbGetQuery(con,"SELECT * FROM Observation"))
        df <- data.frame(IndividualPlant_idIndividualPlant=mega$plantnum,
                         Treatment_idTreatment=mega$idTreatment,
                         Phenotype_idPhenotype = mega$idPhenotype,
                         date=rep("",length(mega$idPhenotype)),
                         value=mega$value)
        
        df <- unique(df)
        df <- df[!is.na(df$value),]
        print(paste("created a df of dim",dim(df)[1],"to insert in Observations"))
        if (commit)
            {
                if(is.null(dbGetQuery(con,paste("INSERT INTO Observation",table2values(df)))))
                    {
                        print(paste("inserted ",dim(df)[1],"records in Observations"))
                    }
                
            } else {
                print("did not write these data to database")
            }
      }
  }
