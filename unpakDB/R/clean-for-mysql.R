#
#
#
cleanMySQLstring <- function(vec) #vec is a vector of character strings (or coerced to)
    {
        vec <- as.character(vec)
        vec <- gsub("'","",vec)
        vec <- gsub("`","",vec)
        vec
    }
