#
# inserts a facility into the database
#
singquote <- function(x){paste("'",x,"'",sep="")}
doubquote <- function(x){paste('"',x,'"',sep="")}

consingquote <- function(x,context=TRUE)
{
  if (context) 
    {
      singquote(x)
    } else {x}
}


table2values <- function(df)
  {
    nms <- names(df)
    character <- sapply(1:length(nms),function(x){is.character(df[,x])})
    factor <- sapply(1:length(nms),function(x){is.factor(df[,x])})
    if (sum(factor)>1)
      {
        df[,factor] <- apply(df[,factor],2,as.character)
      } else {
        df[,factor] <- as.character(df[,factor])
    }
    character <- factor+character
    vals <- apply(
      t(apply(df,1,function(x)
              {
                l <- length(character)
                sapply(1:l,function(i)
                       {
                         paste(consingquote(x[i],character[i]))
                       })
              }))
      ,1,function(y){paste(y,collapse=", ")})
    
    fields <- paste("(",paste(nms,collapse=", "),")",sep="")
    vals <- paste(paste("(",vals,")",sep=""),collapse=',')
    paste(fields,"VALUES", vals)
}

column2values <- function(cl)
  {
    if ((!is.data.frame(cl))|(dim(cl)[2]!=1))
      {
        print("input should be a single column dataframe")
      } else {
        nms <- names(cl)
        character <- sapply(1:length(nms),function(x){is.character(cl[,x])})
        factor <- sapply(1:length(nms),function(x){is.factor(cl[,x])})
        if (sum(factor)>1)
          {
            cl[,factor] <- apply(cl[,factor],2,as.character)
          } else {
            cl[,factor] <- as.character(cl[,factor])
          }
        character <- factor+character
        if (character==TRUE)
          {
            vals <- paste(paste("('",cl[,1],"')",sep=""),collapse=", ")
          } else {
            vals <- paste(paste("(",cl[,1],")",sep=""),collapse=", ")
          }
        
        fields <- paste("(",paste(nms,collapse=", "),")",sep="")
        paste(fields,"VALUES", vals)
}
  }


