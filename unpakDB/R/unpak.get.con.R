unpak.get.con <-
function()
{
  dbConnect(MySQL(),user=db.user,password=db.passwd,host=db.host,dbname="unpak")
}
