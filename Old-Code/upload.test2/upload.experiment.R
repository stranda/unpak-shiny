#
# take an edited exp1a datasheet and put into forms that can be used in unpak db
#
source("setup-unpakR.R")

#########read a csv file and do initial QC screen.

read.mega <- function(fn="CofC_EXPT3PT2_ALL_DATA - Sheet1.csv",
                      classifiers=c("expt.id", "accession", "parent.id", "institution", "facility",
                           "flat", "row", "column","treatment"),
                      ignore=c("GC_locale","ko.status","expt.type","replicate"))
{
    if (FALSE) {fn="CofC_EXPT3PT2_ALL_DATA - Sheet1.csv";
                      classifiers=c("expt.id", "accession", "parent.id", "institution", "facility",
                           "flat", "row", "column","treatment","date","investigator");
                      ignore=c("expt.type","GC_locale","ko.status","replicate")}
    err <- NULL
    mega <- tryCatch({read.csv(fn,as.is=T,na.string=c("."))},
                     error=function(err){err <- c(err,"There was a significant problem reading the file. Please look at the example file and read the document on file formats")})

    if (is.null(err))
        {
            nonclass <- classifiers[!classifiers %in% names(mega)]
            if (length(nonclass)>0) {err <- c(err,paste("These columns are missing from input->",paste(nonclass,collapse=", ")))}
####    mega$treatment="control"
###fix parent id
            mega$parent.id[mega$parent.id=="new_seeds"] <- "new_seeds_0"
            splitid <- data.frame(do.call(rbind,strsplit(mega$parent.id, "_")))
            splitid$X3 <- sprintf("%04i",as.numeric(as.character(splitid$X3)))
            pids <- paste(splitid$X1,splitid$X2,splitid$X3,sep="_")
            pids[pids=="new_seeds_0000"] <- "new_seeds"
            mega$parent.id <- pids
            
            mega$accession <- toupper(gsub(" ","",mega$accession))
            
###standardize facility
            mega$facility <- paste0(toupper(mega$institution),"-",mega$facility)
            mega <- mega[,-which(names(mega)%in%ignore)]
###make sure non-classifiers are numeric
            num.names <- names(mega)[!names(mega)%in%classifiers]
            if (length(num.names)<1){err <- c(err,"there are no non-classifying (phenotype) cols")}
            for (nm in num.names)
                {
                    tmp <- 
                        tryCatch(make.numeric(mega[,grep(nm,names(mega))]),warning=function(war){("warning bad numeric data")})
                    if (length(grep("warning bad",tmp))==0)
                        {
                            mega[,grep(nm,names(mega))] <- tmp
                        } else
                            {
                                err <- c(err,paste("Phenotype",nm,"has non-numeric data"))
                            }
                }
###return
            list(err=err,
                 all=mega,
                 phenotypes=num.names,
                 classifiers=classifiers)
        } else {
            
            list(err=err,
                 all=NULL,
                 phenotypes=num.names,
                 classifiers=classifiers)
        }
         
}




upload.exp <- function(csvfile="CofC_EXPT3PT2_ALL_DATA - Sheet1.csv",pedantic=T,insert_accession=FALSE,commit=F,
                       dbname="unpak", hostname="localhost", username="unpak-R", password="thaliana")
    {
        if (FALSE) {csvfile="CofC_EXPT3PT2_ALL_DATA - Sheet1.csv";pedantic=F;insert_accession=FALSE;commit=F;
                       dbname="unpak"; hostname="localhost"; username="unpak-R"; password="thaliana"}

        unpak_db <- src_mysql(dbname=dbname,host = hostname, user = username, password = password)  #this needs improvement for security
        con <- dbConnect(MySQL(),dbname=dbname,user=username,password=password)
                
        required.fields <- c("expt.id", "accession", "institution",
                             "facility", "expt.plantnum", "flat", "row", "column",
                             "parent.id", "treatment","date","investigator")
        
        msg <- NULL #messages passed at the end
         
        megalist <- read.mega(fn=csvfile,classifiers=unique(c(required.fields)),
                                               ignore=unique(c("expt.type","GC_locale","ko.status","replicate","initials",
                                                   "comments_germination","comments_harvest","harvest.date","plant.id")))
        err <- megalist$err
        mega <- megalist$all
        phenotypes <- megalist$phenotypes
        if (length(grep("comments",phenotypes))>0)
            phenotypes <- phenotypes[-grep("comments",phenotypes)]  #remove column names that are not phenotypes nor classifiers
        required.fields <- megalist$classifiers
        msg <- c(msg,paste("These columns are interpreted as phenotypes  -->> ",paste(phenotypes,collapse=", ")))
        msg <- c(msg,paste("These columns are interpreted as classifiers  -->> ",paste(required.fields,collapse=", ")))

        if (length(err)==-0)
            {
                msg <- c(msg,"data read and initial screens passed")
                mega$accession <- toupper(mega$accession)
                if (!("comments"%in%names(mega))) {mega$comments=""} #put in blank comments if none exist
            }

        if (length(err)==0) #test whether the phenotypes in the input are actual phenotypes in the database
            {
                Phenotbl <- (tbl(unpak_db, "Phenotype"))
                phenos.absent <- (!phenotypes%in%collect(Phenotbl)$name)
                if (sum(phenos.absent)>0) {err <- c(err,paste("Phenotypes in input but not in database:",paste(phenotypes[phenos.absent],collapse=", ")))}
            }

        

        if (length(err)==0)
            {
                msg <- c(msg,"phenotypes all present in db")
                accessions <- collect(tbl(unpak_db,"Accession"))$idAccession
                no.accessions <- unique(accessions[!(mega$accession %in% accessions)])
                
                if (length(no.accessions)>0)
                    {
                        err <- c(err, paste("Accessions that are missing from the full database:",paste(no.accessions,collapse=", ")))
                    }
            }                

        if (length(err)==0)
            {
                msg <- c(msg,"accessions all present in db")

                facilities <- collect(tbl(unpak_db,"Facility"))$name
                no.facilities <- unique(facilities[!mega$facilities%in%facilities])
                if (length(no.facilities)>0)
                    {
                        err <- c(err, paste("Facilities that are missing from the full database:",paste(no.facilities,collapse=", ")))
                    }
            }

        if (length(err)==0)
            {
                msg <- c(msg,"facilities all present in db")
                treats <- collect(tbl(unpak_db,"Treatment"))$name
                none <- unique(treats[!mega$treatment%in%treats])
                if (length(none)>0)
                    {
                        err <- c(err, paste("Treatments that are missing from the full database:",paste(none,collapse=", ")))
                    }
            }

        if (length(err)==0)
            {
                msg <- c(msg,"treatments all present in db")
                exps <- collect(tbl(unpak_db,"Experiment"))$name
                none <- unique(exps[!mega$expt.id%in%exps])
                
                if (length(none)>0)
                    {
                        err <- c(err, paste("Experiments that are missing from the full database:",paste(none,collapse=", ")))
                    }
                ##   check for multiple experiment ids in this spreadsheet
                if (length(unique(mega$expt.id))>1)
                    {
                        err <- c(err, paste("More than one experiment is present in this spreadsheet:",paste(unique(mega$expt.id),collapse=", ")))
                    }

            }
        
        if (length(err)==0)
            {
                msg <- c(msg,"experiments all present in db")
                inst <- collect(tbl(unpak_db,"Institution"))$abbreviation
                none <- unique(inst[!mega$institution%in%inst])
                if (length(none)>0)
                    {
                        err <- c(err, paste("Institutions that are missing from the full database:",paste(none,collapse=", ")))
                    }
            }
        

        if (length(err)==0) #create/assign plant numbers, then melt input dataframe
            {
                msg <- c(msg,"Institutions all present in db")
                #this next line gets the largest plantid in the system
                maxplantnum <- collect(summarize(tbl(unpak_db,"IndividualPlant"),max=max(idIndividualPlant)))$max
                newmin <- maxplantnum+1

                #now need to find plants that might already be in the database
#                mega$plantnum_experiment <- with(mega,paste(flat,row,column,sep="-"))
                mega$altplantnum_experiment <- with(mega,paste(flat,row,column,sep="-"))
                mega$plantnum_experiment <- ifelse(!is.na(mega$expt.plantnum),paste(mega$institution,mega$expt.id,mega$expt.plantnum,sep="_"),
                                                   with(mega,paste(flat,row,column,sep="-")))

                ip <- tbl(unpak_db,"IndividualPlant")
                ex <- tbl(unpak_db,"Experiment")
                inst <- tbl(unpak_db,"Institution")
                fac <- tbl(unpak_db,"Facility")

                ##filter ip to a manageable size
                ##only records with same experiment as this csv.
                ###
                expt <- unique(mega$expt.id)
                idExperiment <- collect(ex %>% filter(name == expt))$idExperiment[1]
                tmp.plnum <- data.frame(ip %>%
                                            filter(Experiment_idExperiment == idExperiment)
                                        %>%
                                            left_join(ex,by=c("Experiment_idExperiment"="idExperiment"))
                                       ,stringsAsFactors=F)

                mega$tmpidx <- 1:dim(mega)[1]
                mega.pn <- merge(tmp.plnum,mega)
                mega$plantnum_experiment_old <- mega$plantnum_experiment
                mega$plantnum_experiment <- mega$altplantnum_experiment
                mega.apn <- merge(tmp.plnum,mega)
                mega.mrg <- rbind(mega.pn,mega.apn[,-45]) #remove the plantnum_experiment_old from the mega.apn df

                mega.mrg <- merge(mega,mega.mrg[,((!names(mega.mrg)%in%names(mega)) +
                                                         (names(mega.mrg)=="tmpidx"))>0],by="tmpidx",all.x=T)
                
                mega.mrg$not.found <- is.na(mega.mrg$idIndividualPlant)
                if (sum(not.found)>0)
                    {
                        mega.mrg$idIndividualPlant[mega.mrg$not.found] <- seq(newmin,sum(not.found))
                    }
                if (length(unique(mega.mrg$plantnum_experiment))<length(mega.mrg$plantnum_experiment))
                    {
                        rl.er <- rle(mega.mrg$plantnum_experiment)
                        err <- c(err, paste("Duplicate plantnumbers in this sub experiment:",paste(rl.er$values[rl.er$lengths>1],collapse=",")))
                    }

                existing.plants <- mega.mrg[!(mega.mrg$not.found),]
                new.plants <- mega.mrg[mega.mrg$not.found,]

                mega.mrg <- mega.mrg[,!names(mega.mrg)%in%c("not.found","embargoed","idExperiment","name","startDate","endDate","description","tmpidx",
                                                            "Facility_idFacility","Experiment_idExperiment","Accession_idAccession",
                                                            "altplantnum_experiment","plantnum_experiment_old","Parent1","Parent2")]

#                names(mega.mrg)[grep("idIndividualPlant",names(mega.mrg))] <- "plantnum"
                ##convert mega to long format.  Should only have columns in the required fields and then phenotypes
                mega <- melt(mega.mrg,id.vars=c(required.fields,"idIndividualPlant","comments","plantnum_experiment"),
                             variable_name="phenotype")
                mega$value <- tryCatch({make.numeric(mega$value)},
                                       warning=function(war){NA})
                if (sum(is.na(mega$value))==length(mega$value)) { #check and see if there was a warning about numeric data
                    err <- c(err,"There are columns being interpreted as phenotypes that have non-numeric data: check the list of phenotypes")
                }
                
            }      
        if (length(err)==0) #make sure parents in this file are present in DB; if not these are new seeds
            {
                msg <- c(msg,"database melted now, checking for parents")
                ##assuming that there is only one parent.
                mega$parent.id <- as.character(mega$parent.id)
                mega$Parent1 <- ifelse(is.na(mega$parent.id),"new_seeds",mega$parent.id)
                if ((pedantic)&(length(which(mega$Parent1=="new_seeds"))>0))
                    {
                        err <- c(err,paste("if pedantic is set to true, every plant has to have an existing parent in the db, otherwise plants result from new seeds to the system"))
                    }
                
#                p1 <- collect(tbl(unpak_db,"IndividualPlant"))$plantnum_experiment
                pars <- dbGetQuery(con,"SELECT * FROM IndividualPlant")
                p1 <- pars$plantnum_experiment
                none <- unique(p1[!mega$Parent1%in%p1])
                
                
                if (length(none)>0)
                    {
                        err <- c(err,paste("These parents listed below need to be inserted into the db",paste(none,collapse=", ")))
                    }
                                
                mega$Parent2 <- mega$Parent1
                
            }
    
        
        if (length(err)==0) #all clear
            {
                msg <- c(msg,"all parents are present in database")
                msg <- c(msg,"#####Starting formatting for upload")
                ##convert the facility to facility ID
                mega <- convert.to.idx(unpak_db, "Facility", mega, "idFacility", "name", "facility")

                ##convert the experiment
                mega <- convert.to.idx(unpak_db, "Experiment", mega, "idExperiment", "name", "expt.id")
                
                ##convert the institution 
                mega <- convert.to.idx(unpak_db, "Institution", mega, "idInstitution", "abbreviation", "institution")
                
                ##convert the treatment
                mega <- convert.to.idx(unpak_db, "Treatment", mega, "idTreatment", "name", "treatment")
                
                ##convert the phenotype
                mega <- convert.to.idx(unpak_db, "Phenotype", mega, "idPhenotype", "name", "phenotype")
                
                msg <- c(msg,"#####converted facility, experiment, inst, treat, and phenotype to internal representation")
###########now check and see which plants are already in the database
### caution caution.
###  dplyr limits the number of records that it downloads.  the following assumes less than the limit for number of plants
###
###                ip <- data.frame(tbl(unpak_db, "IndividualPlant"))
### went back to the raw(er) RMySQL interface
###
                
                ip <- dbGetQuery(con,"SELECT * FROM IndividualPlant")
                fac.df <- data.frame(fac)
                ip <- merge(ip,fac.df[,c("Institution_idInstitution","idFacility")],by.x="Facility_idFacility",by.y="idFacility")

                parent <- join(mega,ip[,c("idIndividualPlant","plantnum_experiment","Experiment_idExperiment","Facility_idFacility")],
                                    by=c("Parent1"="plantnum_experiment","idExperiment"="Experiment_idExperiment",
                                         "idFacility"="Facility_idFacility"))

                for (pl in unique(mega$idIndividualPlant)) #go through plants and make decision to insert or update
                    {
                        plant <- unique(mega[mega$idIndividualPlant==pl,c("idIndividualPlant","idExperiment","idFacility",
                                          "comments","accession","Parent1","Parent2","plantnum_experiment")])
                        if (pl %in% ip$idIndividualPlant) # plant in db, let's update
                            {
                                compvec <- c(plant$idIndividualPlant==ip[ip$idIndividualPlant==pl,]$idIndividualPlant,
                                             plant$idFacility==ip[ip$idIndividualPlant==pl,]$Facility_idFacility,
                                             plant$accession==ip[ip$idIndividualPlant==pl,]$Accession_idAccession,
                                             plant$idExperiment==ip[ip$idIndividualPlant==pl,]$Experiment_idExperiment,
                                             
                                             plant$plantnum_experiment==ip[ip$idIndividualPlant==pl,]$plantnum_experiment,
                                             plant$Parent1==ip[ip$idIndividualPlant==pl,]$Parent1,
                                             plant$Parent2==ip[ip$idIndividualPlant==pl,]$Parent2,
                                             plant$comments==ip[ip$idIndividualPlant==pl,]$comment)
                                             
                                query <- paste0("UPDATE IndividualPlant",
                                                "SET "
                                if (commit)
                                    {
                                        
                                    }
                            } else { #plant missing from db let's insert
                                if (commit)
                                    {
                                    }
                            }
                    }




                present <- merge(mega,ip[,c(-grep("Parent",names(ip)))],by.x=c("idInstitution","idFacility","idExperiment","plantnum_experiment"),by.y=c("Institution_idInstitution","Facility_idFacility","Experiment_idExperiment","plantnum_experiment"),all.x=T)
                
                mega.present <- present[!is.na(present$idIndividualPlant),names(mega)]
                mega.absent <- present[is.na(present$idIndividualPlant),names(mega)]
                    
                ## ## FIRST INSERT THE OBSERVATIONS FOR EXISTING PLANTS
                ## ##
                if (dim(mega.present)[1]>0)
                    {
                        msg <- c(msg,
                                 "about to update observations for plants already in database; Check the tab for existing plants")
                        mega <- mega.present
#                        mega <- merge(mega,ip[-grep("Parent",names(ip))],by.x=c("idInstitution","idFacility","idExperiment","plantnum_experiment"),by.y=c("Facility_idFacility","Experiment_idExperiment","plantnum_experiment"),all.x=T)
                        ## ###########################
                        ## update the observations
                        ## important to note that I only insert observations where there are data in
                        ## the original file.  Will have to be filled in with nulls/nas
                        ## if wide format regenerated from the information in this table
                        ## ###########################
                        ## nms <- names(dbGetQuery(con,"SELECT * FROM Observation"))
                        df <- data.frame(IndividualPlant_idIndividualPlant=mega$idIndividualPlant,
                                         Treatment_idTreatment=mega$idTreatment,
                                         Phenotype_idPhenotype = mega$idPhenotype,
                                         date=rep("0000-00-00",length(mega$idPhenotype)),
                                         value=mega$value)
                        df$date <- as.character(df$date)
                        
                        df <- unique(df)
                        df <- df[!is.na(df$value),]
                        df$newval <- TRUE
                        
                        ## Delete observations in the Observation Table that already exist (overwrite data)
                        obs <- dbGetQuery(con,"SELECT * FROM Observation")
                        
                        present <- merge(obs,df[,c("IndividualPlant_idIndividualPlant","Treatment_idTreatment","Phenotype_idPhenotype","date","newval")],all.x=T)
                        present <- present[is.na(present$newval),]
                        present <- present[,names(obs)]
                        present <- present[!is.na(present$value),]
                        msg <- c(msg,(paste("created a df of dim",dim(df)[1],"to insert in Observations")))
                        if (commit)
                            {
                                ## replace the Observation table with observations that are
                                ## present in the original table minus the new obs in df
                                df$newval <- ""
                                names(df)[which(names(df)=="newval")] <- "comment"
                                combo <- rbind(present[,names(df)],df)
                                dbWriteTable(con,"Observation",combo,row.names=F,overwrite=T)
                                msg <- c(msg,paste("inserted ",dim(df)[1],"records in Observations"))
#                                print(paste("inserted ",dim(df)[1],"records in Observations"))
                            } else {msg <- c(msg,"Data for existing plants not committed to db")}
                        msg <- c(msg,"finished routine to insert observations for plants that already exist in database")
                    }
                
                ##  ## NOW INSERT NEW PLANTS AND THEIR OBSERVATIONS
                if (dim(mega.absent)[1]>0)
                    {
                        msg <- c(msg,"about to update observations for plants novel to database")
                        ## #find the individual plant ids for parents of these experimental plants
                        mega <- mega.absent
                        mega <- merge(mega,ip[,c("idIndividualPlant","plantnum_experiment")],by.x="Parent1",by.y="plantnum_experiment")
                        mega$Parent1.num <- mega$idIndividualPlant
                        mega <- mega[,which(names(mega)!="idIndividualPlant")]
                        mega <- merge(mega,ip[,c("idIndividualPlant","plantnum_experiment")],by.x="Parent2",by.y="plantnum_experiment")
                        mega$Parent2.num <- mega$idIndividualPlant
                        mega <- mega[,which(names(mega)!="idIndividualPlant")]
                        
                        ## ###########################
                        ## ok now update the IndividualPlant table
                        ## the information comes from the plannum and
                        ## facility and experiment cols
                        ## ###########################
                        ##        nms <- names(dbGetQuery(con,"SELECT * FROM IndividualPlant"))
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
                        msg <- c(msg,paste("created a df of dim",dim(df)[1],"to insert in IndividualPlant"))
                        if (commit)
                            {
                                if(is.null(dbGetQuery(con,paste("INSERT INTO IndividualPlant",table2values(df)))))
                                    {
                                        msg <- c(msg,paste("inserted ",dim(df)[1],"records in IndividualPlant"))
                                    }
                                
                            } else {
                                 msg <- c(msg,"novel plants not committed to db")
                            }
                        
                        ## ###########################
                        ## ok now update the observations
                        ## important to note that I only insert observations where there are data in
                        ##the original file.  Will have to be filled in with nulls/nas
                        ##if wide format regenerated from the information in this table
                        ## ###########################
                        ## nms <- names(dbGetQuery(con,"SELECT * FROM Observation"))
                        df <- data.frame(IndividualPlant_idIndividualPlant=mega$plantnum,
                                         Treatment_idTreatment=mega$idTreatment,
                                         Phenotype_idPhenotype = mega$idPhenotype,
                                         date=rep("",length(mega$idPhenotype)),
                                         value=mega$value)
                        
                        df <- unique(df)
                        df <- df[!is.na(df$value),]
                        msg <- c(msg,paste("created a df of dim",dim(df)[1],"to insert in Observations"))
                        if (commit)
                            {
                                if(is.null(dbGetQuery(con,paste("INSERT INTO Observation",table2values(df)))))
                                    {
                                        msg <- c(msg,(paste("inserted ",dim(df)[1],"records in Observations")))
                                    }
                                
                            } else {
                                msg <- c(msg,("data for novel plants not written to  database"))
                            }
                        msg <- c(msg,"finished routine for inserting observations for plants that did not  exist in database")
                    }
                msg <- c(msg,"exiting from the database insertion/update routine")
            }
        list(errors=err, messages=msg)
    }
