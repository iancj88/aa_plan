ConnectToAccessDB <- function(db_file_path) {
  require(RODBC)

  db <- db_file_path

  dbq_string <- paste0("DBQ=", db_file_path)
  drvr <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

  db_connect_string <- paste0(drvr, dbq_string)

  myconn <- odbcDriverConnect(db_connect_string)
  rm(db, db_connect_string)

  return(myconn)
}

