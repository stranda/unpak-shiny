#
# functions to take a phenotype along with classifying information to create an adjusted phenotype
#  either by the phytometers or by the means in each growth chamber/greenhouse of all plants
#

phytcorrect <- function(dat,
                        pheno="fruitnum",
                        classifier=c("experiment","facility","treatment"),
                        lineid="Accession")
    {

        dat <- dat[!is.na(dat[,pheno]),] #don't mess with NAs
        
        filter.cond <- paste0("grepl('^CS',",lineid,")")
        select.cond <- paste0(c(classifier,pheno))
        group.cond <-  paste0(c(classifier))
 ### mean all phyts by classifiers
        phytmn <- filter_(dat,filter.cond)%>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(phytmn)[grep(pheno,names(phytmn))] <- "phytval"  

### adj dat by phytometer means and store in adjdat
        select.cond <- paste0(c(lineid,pheno,classifier,"phytval"))
        adjdat <- left_join(dat,phytmn)
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat$adjval <- adjdat[,pheno]-adjdat$phytval

        #the adjusted phenotype is in the adjval column.  could change this to just put it in the
        #column given by 'pheno'
        adjdat
    }

#this one adjusts the phenotype by the means of all plants in each
#growth chamber
#

allcorrect <- function(dat,
                       pheno="fruitnum",
                       classifier=c("experiment","facility","treatment"),
                       lineid="Accession")
    {
        require(dplyr)
        dat <- dat[!is.na(dat[,pheno]),] #don't mess with NAs
        select.cond <- paste0(c(classifier,pheno))
        group.cond <-  paste0(c(classifier))
 ### mean all phyts by classifiers
        mns <- dat %>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(mns)[grep(pheno,names(mns))] <- "gcmeans"

### adj dat by phytometer means and store in adjdat
        select.cond <- paste0(c(lineid,pheno,classifier,"gcmeans"))
        adjdat <- left_join(dat,mns)
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat$adjval <- adjdat[,pheno]-adjdat$gcmeans

        #the adjusted phenotype is in the adjval column.  could change this to just put it in the
        #column given by 'pheno'
        adjdat
    }
