#
#
# read in the megasheet for exp1a1 and exp1a2 as well as the line lists.  Make a single finel that can be uploaded into the database.
#
library(reshape)
if (!file.exists ("exp1-megasheet-with-parents.csv"))
    {

        mega <- read.csv("unpak_megasheet_olivia_compiled_withmatt_courtney_bradley_ymwedit_7_3_13 - all_datalines.csv",as.is=T,na.string=c("."))
        mega$treatment="standard GC"
        e1a1.lines <- read.csv("Expt1A_LineList_5_15_2013YMW - line_list.csv",as.is=T)
        e1a2.lines <- read.csv("Expt1A_REP_2_LineList - Expt1ARep2_Lines.csv",as.is=T)
        names(e1a2.lines)[c(1,2,6,11)] <- names(e1a1.lines)[c(1,2,6,11)]
        
        mega$accession <- toupper(mega$accession)
        mega <- mega[,-which(names(mega)%in%"total.fruit")]#remove total.fruit, its the sum of abort and good.fruit
        mega$inflorescence.height <- as.numeric(mega$inflorescence.height)
        e1a1.lines$accession <- toupper(e1a1.lines$accession)
        e1a2.lines$accession <- toupper(e1a2.lines$accession)
        e1a2.lines$exptid <- "1a2"
        
        linelist <- rbind(e1a1.lines[,names(e1a2.lines)],e1a2.lines)[,-c(1,3,4,6,8)]
        linelist$facility <- paste(toupper(linelist$institution),linelist$facility,sep="-")
        linelist$accession[linelist$accession=="CS70000"] <- "COL70000"
        
        all <- merge(mega,linelist,all.x=T)
        all <- all[,-which(names(all)%in%c("GC_locale","ko.status","expttype","replicate"))] #get rid of these columns
        noparents <- all[is.na(all$parent_id),]
        withparents <- all[!is.na(all$parent_id),]
        write.table(file="exp1-megasheet-with-parents.csv",sep=",",row.names=F,withparents)
        write.table(file="exp1-megasheet-parents-missing.csv",sep=",",row.names=F,noparents)
    } else { #there is a megasheet with parents
        withparents <- read.csv("exp1-megasheet-with-parents.csv")
    }
###
### ultimately, ignore the cases where there are no parents for individuals
###
