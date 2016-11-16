#
# functions to take a phenotype along with classifying information to create an adjusted phenotype
#  either by the phytometers or by the means in each growth chamber/greenhouse of all plants
#
## the "dat" dataframes each function takes are in long, or melted format.

require(dplyr)

phytcorrect <- function(dat, pheno, classifier, lineid) {

#         dat <- dat[dat$phenotype %in% pheno,]
        dat <- dat[!is.na(dat$value),] #don't mess with NAs
        
        filter.cond <- paste0("grepl('CS',",lineid,")")
        select.cond <- paste0(c(classifier,"phenotype","value", "slim"))
        group.cond <-  paste0(c(classifier,"phenotype", "slim"))
 ### mean all phyts by classifiers
        phytmn <- filter_(dat,filter.cond)%>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(phytmn)[names(phytmn)=="value"] <- "mean"  

### adj dat by phytometer means
        select.cond <- paste0(c(classifier,lineid,"phenotype","value", "slim"))
        adjdat <- left_join(dat,phytmn)
        adjdat$value <- adjdat$value-adjdat$mean
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat
    }

#this one adjusts the phenotype by the means of all plants in each
#growth chamber
#

allcorrect <- function(dat, pheno, classifier, lineid) {

        dat <- dat[dat$phenotype %in% pheno,]
        dat <- dat[!is.na(dat$value),] #don't mess with NAs
        
        select.cond <- paste0(c(classifier,"phenotype","value"))
        group.cond <-  paste0(c(classifier,"phenotype"))
 ### mean all phyts by classifiers
        phytmn <- dat %>%
            select_(.dots=select.cond)%>%
                group_by_(.dots=group.cond) %>% summarise_each(funs(mean(.,na.rm=F)))
        names(phytmn)[names(phytmn)=="value"] <- "mean"  

### adj dat by phytometer means
        select.cond <- paste0(c(classifier,lineid,"phenotype","value"))
        adjdat <- left_join(dat,phytmn)
        adjdat$value <- adjdat$value-adjdat$mean
        adjdat <- adjdat %>% select_(.dots=select.cond)
        adjdat
        
    }
