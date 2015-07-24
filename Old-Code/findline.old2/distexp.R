#
# given a dataframe with phenotypes, lines, treatments, experiments and GC, plot each of the phenotypes for each of the treatments
#
distexp <- function(df,line,expt,linemeans=F,correct=c("none","all","phyt")[3])
    {
       df <- df[df$experiment==expt,]
       df <- df[,-which(names(df)=="experiment")]

       if (correct=="all")
           {
               mns <- with(df,aggregate(cbind(mn.value=value),by=list(phenotype=phenotype,facility=facility),mean,na.rm=T))
               tmp <- merge(df,mns)
               df <- tmp[,1:5]
               df$value=tmp$value-tmp$mn.value
           }
       else    if (correct=="phyt")
           {
               phydf <- df[grep("CS",df$line),] #if they start in CS, we call em phytometers
               mns <- with(phydf,aggregate(cbind(mn.value=value),by=list(phenotype=phenotype,facility=facility),mean,na.rm=T))
               tmp <- merge(df,mns)
               df <- tmp[,1:5]
               df$value=tmp$value-tmp$mn.value
           }
       else
           {
               df=df
           }
       
       if (linemeans)
           {
             df <- with(df,aggregate(cbind(value),by=list(line=line,phenotype=phenotype,treatment=treatment),mean,na.rm=T))
           }

       for (p in unique(df$phenotype))
           for (t in unique(df$treatment))
               {
                   ldf <- df[(df$phenotype==p)&(df$treatment==t),]
                   hist(ldf$value,xlab="Phenotypic value",main=paste("Expt:",expt,"Phenotype:",p,"\nTreatment:",t))
                   abline(v=ldf$value[ldf$line==line],lwd=2,col="red")
#                   rug(ldf$value[ldf$line==line],lwd=2,col="red")
               }

       
    }
