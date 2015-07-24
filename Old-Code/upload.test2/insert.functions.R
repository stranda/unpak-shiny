###########33
#########    functions to test if observations can be inserted into a database.
############ and then insert them
##########AES 6 30 2013

#find the largest unique id for a plant in the individual plant table
largest.plantid <- function(con)
    {
        max(as.numeric(unlist(dbGetQuery(con,"SELECT idIndividualPlant FROM IndividualPlant"))))
    }

#check two vectors of names, if all of the second are not in the first, return FALSE
test.names <- function(names,required)
    {
        res <- as.logical(prod(required%in%names))
        if (!res)
            {
                print("these column names are missing from the input data")
                print(required[which(!(required%in%names))])
            }
        res
    }

print.missing <- function(vec,matched)
    {
        if (!is.null(matched))
            {
                print(unique(vec[-matched]))
            } else {
                print(unique(vec))
            }
    }


convert.to.idx <- function(db, table, df, db_idx, db_col, df_col)
    {
          tmp.df <- collect(tbl(db,table))
          tmp.df <- merge(tmp.df[,c(db_idx,db_col)],df,by.x=db_col,by.y=df_col)
          tmp.df[,-1]
    }
