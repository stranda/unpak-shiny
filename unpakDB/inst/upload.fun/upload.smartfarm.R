#
## read in the data from experiment 2 from the pre-unpak days
#
insert.smartfarm <- function(con,csvfile,insert_accession=FALSE,commit=FALSE)
{

  required.cols <- c("Accession", "Institution" , "Location", "Expt.type", "Expt.ID", "Plant.ID", "Tray", "Row", "Column","Parent1", "Parent2","comment" )
  
  if (is.null(csvfile))
    stop("must specify a csv file name")
  
  sf <- read.csv(file=csvfile,header=T,na.strings=c(".","NA","0 n/a"))

  if (("location"%in%names(sf))) { sf$Location <- as.character(sf$location) } else { sf$Location <- as.character(sf$facility)}
  
  #name sf cols
  sf$Accession <- toupper(as.character(sf$accession))
  sf$Institution <- as.character(sf$institution)
  sf$Expt.type <- as.character(sf$expt_type)
  if ("expt_ID" %in% names(sf)) {sf$Expt.ID <- sf$expt_ID} else {sf$Expt.ID <- sf$expt_id}
  sf$Plant.ID <- sf$plant_id
  sf$Tray <- sf$tray
  sf$Row <- sf$row
  sf$Column <- sf$column
if ("parent_plant_id" %in% names(sf)) { sf$Parent1 <- as.character(sf$parent_plant_id)} else { sf$Parent1 <- as.character(sf$parent_id)}
  sf$Parent1[sf$Parent1==""] <- NA
  sf$Parent2 <-   sf$Parent1

  sf$Location <- ifelse(sf$Location=="NGH","NGH-COFC",sf$Location)
  sf$comment <- as.character(sf$comments)
  errcode <- 0
######## is the input spreadsheet
######## in the correct format
  if (prod(required.cols %in% names(sf))==0)
    {
      errcode <- 1
      print("The following column names are expected for each smartfarm upload")
      print(required.cols)
      print ("In this case the following cols are missing from the input:")
      print(required.cols[!(required.cols %in% names(sf))])
    } else {
      sf <- sf[,which(names(sf)%in%required.cols)]
    }

  
### Is the database ready to accept the smartfarm
###

### Test to see if the accessions are present in DB
###
  if (errcode==0)
    {
      ac <- which.match(con,table="Accession","idAccession",match=sf$Accession)
      if (length(ac)!=length(sf$Accession))
        {
          if (insert_accession==T)
            {
              ins <- unique(data.frame(idAccession=as.character(sf$Accession[-ac])))
              sqlquery <- paste("INSERT INTO Accession ",column2values(ins))
              dbGetQuery(con,sqlquery)
              print("new accessions inserted (see list below).  rerun function to insert the actual data")
              print(sf$Accession[-1*ac])
            } else {
              print("These accessions are not loaded in the DB.  If they should be loaded")
              print("set the insert_accession flag to TRUE in function call")
              print(sf$Accession[-1*ac])
              errcode=10              
            }
        }
    }

### now check to see if the experiment is in the db
if (errcode==0)
  {
      
      expname <- paste(sf$Institution,sf$Expt.type,sf$Expt.ID,sep="-")
      ex <- which.match(con,table="Experiment","name",match=expname)
      if (length(ex)!=length(expname))
        {
          errcode = 12
          print("Experiment(s) listed below need to be inserted into the db before inserting this spreadsheet")
          print(unique(expname))
        }
  }

### now check to see if the facility is in the db
if (errcode==0)
  {
    facility <- paste(sf$Location)
    
    fa <- which.match(con,table="Facility","name",match=facility)
      if (length(fa)!=length(facility))
        {
          errcode = 13
          print("Facilities(s) listed below need to be inserted into the db before inserting this spreadsheet")
          print(unique(facility))
        }
  }

  ###check to see if the parents are in the Individual plant DB
  if (errcode==0)
      {
          sf$Parent1 <- ifelse(is.na(sf$Parent1),"new_seeds",sf$Parent1)
          
          p1 <- which.match(con,table="IndividualPlant","plantnum_experiment",match=sf$Parent1)
          if (length(p1)!=dim(sf)[1])
              {
                  errcode = 14
                  print("Parents listed below need to be inserted into the db before inserting this spreadsheet")
                  if (length(p1)>0)
                      {
                          print(sf$Parent1[-p1])
                      } else
                          {
                              print(sf$Parent1)
                          }
              }
          
          sf$Parent2 <- sf$Parent1
          
          p2 <- which.match(con,table="IndividualPlant","plantnum_experiment",match=sf$Parent1)
          if (length(p2)!=dim(sf)[1])
              {
                  errcode = 15
                  print("Parents listed below need to be inserted into the db before inserting this spreadsheet")
                  if (length(p2)>0)
                      {
                          print(sf$Parent2[-p2])
                      } else {print(sf$Parent2)}
              }
      }
  
  if (errcode==0) #Make parents fields that correspond to individual ids in the IndividualPlants table
      {
          ip <- dbGetQuery(con,"SELECT * FROM IndividualPlant")
          sf <- merge(sf,ip[,c("idIndividualPlant","plantnum_experiment")],by.x="Parent1",by.y="plantnum_experiment")
          sf$Parent1.num <- sf$idIndividualPlant
          sf <- sf[,which(names(sf)!="idIndividualPlant")]
          sf <- merge(sf,ip[,c("idIndividualPlant","plantnum_experiment")],by.x="Parent2",by.y="plantnum_experiment")
          sf$Parent2.num <- sf$idIndividualPlant
          sf <- sf[,which(names(sf)!="idIndividualPlant")]
          
      }



  if (errcode==0)
      {
###
###    assign plant IDS
                                        # first find the highest plant number
          maxplantnum <- max(as.numeric(unlist(dbGetQuery(con,"SELECT idIndividualPlant FROM IndividualPlant"))))
          newmin <- maxplantnum+1
          newmax <- maxplantnum+dim(sf)[1]
          sf$plantnum <- seq(newmin,newmax)

          ##now assign facility code
          fac <- dbGetQuery(con,"SELECT * FROM Facility")
          facid <- merge(data.frame(name=facility),fac,all.x=T)$idFacility
###now assign experiment code
          expt <- dbGetQuery(con,"SELECT * FROM Experiment")
          exptid <- merge(data.frame(name=expname),expt,all.x=T)$idExperiment
          df <- data.frame(idIndividualPlant=sf$plantnum,
                           Facility_idFacility=facid,
                           Experiment_idExperiment=exptid,
                           Accession_idAccession=sf$Accession,
                           Parent1=sf$Parent1.num,
                           Parent2=sf$Parent2.num,
                           plantnum_experiment=sf$Plant.ID,
                           comment=sf$comment)


          df$plantnum_experiment=cleanMySQLstring(df$plantnum_experiment)
          df$comment <- cleanMySQLstring(df$comment)
          if (commit)
              {
                  if (is.null(dbGetQuery(con,paste("INSERT INTO IndividualPlant",table2values(df)))))
                      {
                          print(paste("inserted ",dim(sf)[1],"records into the individual plant database"))
                      } else stop("error on insert")
              } else {print(paste("successfully created a df with ",dim(df)[1],"rows. NOT INSERTED"))}
      } else {stop(errcode)}
}
