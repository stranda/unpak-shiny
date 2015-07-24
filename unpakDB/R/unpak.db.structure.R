unpak.db.structure <-
function(con=unpak.get.con())
  {
    tables <- unpak.tables(con)
    ret <- lapply(tables,function(x){list(table=x)})
    names(ret) <- tables
    lapply(ret,function(conn,x) {list(table=x$table,fields=dbListFields(conn,x$table))},conn=con)
  }
