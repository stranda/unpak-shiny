library("unpakR")
library("dplyr")

dbname="unpak"
hostname="localhost"
username="unpak-R"
password="thaliana"
unpak_db <- src_mysql(dbname=dbname,host = hostname, user = username, password = password)  #this needs improvement for security
ip <- tbl(unpak_db,"IndividualPlant")
ex <- tbl(unpak_db,"Experiment")
res <- left_join(ip,ex,by=c("Experiment_idExperiment"="idExperiment"))

df.ip <- data.frame(res)

