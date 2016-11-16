#
# functions to take a Phenotype along with classifying information to create an adjusted Phenotype
#  either by the phytometers or by the means in each growth chamber/greenhouse of all plants
#
## the "dat" dataframes each function takes are in long, or melted format.

require(dplyr)

phytcorrect <- function(dat, pheno, classifier, lineid) {

#         dat <- dat[dat$Phenotype %in% pheno,]
        dat <- dat[!is.na(dat$Value),] #don't mess with NAs
        
        filter.cond <- paste0("grepl('CS',",lineid,")")
        select.cond <- paste0(c(classifier,"Phenotype","Value", "ConservedGroup"))
        group.cond <-  paste0(c(classifier,"Phenotype", "ConservedGroup"))
 ### mean all phyts by classifiers
        phytmn <- filter_(dat,filter.cond)%>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(phytmn)[names(phytmn)=="Value"] <- "mean"  

### adj dat by phytometer means
        select.cond <- paste0(c(classifier,lineid,"Phenotype","Value", "ConservedGroup"))
        adjdat <- left_join(dat,phytmn)
        adjdat$Value <- adjdat$Value-adjdat$mean
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat
    }

#this one adjusts the Phenotype by the means of all plants in each
#growth chamber
#

allcorrect <- function(dat, pheno, classifier, lineid) {

        dat <- dat[dat$Phenotype %in% pheno,]
        dat <- dat[!is.na(dat$Value),] #don't mess with NAs
        
        select.cond <- paste0(c(classifier,"Phenotype","Value"))
        group.cond <-  paste0(c(classifier,"Phenotype"))
 ### mean all phyts by classifiers
        phytmn <- dat %>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(phytmn)[names(phytmn)=="Value"] <- "mean"  

### adj dat by phytometer means
        select.cond <- paste0(c(classifier,lineid,"Phenotype","Value"))
        adjdat <- left_join(dat,phytmn)
        adjdat$Value <- adjdat$Value-adjdat$mean
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat
        
    }
