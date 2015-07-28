source("adjust-pheno.R")

##Set up parameters
adjust <- T
linemeans <- T
phytometers <- T
phenotype <- "fruitnum"
line.identifier <- "Accession"

focal.lines <- c("SALK_075879C","SALK_006272C")

classifiers <- c("experiment","facility") #columns to take means over. this choice is what we have been doing
phenos <- read.csv("phenotypes2015-07-20.csv") #expects 'wide' not long (or melted) format
names(phenos) #have to have the names used for lineid, pheno, and classifier in the phytcorrect function


#could write a wrapper for the following if/else and make it more elegant...or combine phyt and allcorrect
if (adjust)
    {
        if (phytometers)
            {
                phenadjust <- phytcorrect(dat=phenos, pheno=phenotype, classifier=classifiers,
                                          lineid=line.identifier)
            } else {
                phenadjust <- allcorrect(dat=phenos, pheno=phenotype, classifier=classifiers,
                                         lineid=line.identifier)
            }
        
    } else {
        phenadjust <- phenos
        phenadjust$adjval <- phenadjust[,phenotype]
    }


if (linemeans) #get means per line instead of actual observations
    phenadjust <- phenadjust%>%group_by(Accession)%>%summarise(adjval=mean(adjval,na.rm=T))


######now plot
        
        hist(phenadjust$adjval, xlab=paste0(phenotype))

###### add in the vert lines for the "focal.lines"
###### I added in some logic to allow multiple accessions to be included in multiple lines
######

with(phenadjust[phenadjust$Accession%in%focal.lines,],
     abline(v=adjval,col=unclass(as.factor(as.character(Accession)))))
