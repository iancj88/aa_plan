MissingCodes <- function(df) {

  # get the most recently dated data from the df
  # max_date <- max(df$date)
  # df <- df[df$date == max_date,]

  # only examine Staff i.e. classified, professional, fixed-term
  # employees

  # df <- df[df$EMRJobType %in% c("Classified",
  #                               "Professional",
  #                               "Fixed-Term"),]

  # create indx of missing jcat, cupa, soc codes
  jcat_missing_indx <- (is.na(df$`JCAT Code`) | df$`JCAT Code` == "")
  cupa_missing_indx <- (is.na(df$`CUPA Code`) | df$`CUPA Code` == "")
  fsoc_missing_indx <- (is.na(df$`FED SOC Code`) | df$`FED SOC Code` == "")

  df$missing_jcat <- FALSE
  df$missing_jcat[jcat_missing_indx] <- TRUE
  df$missing_cupa <- FALSE
  df$missing_cupa[cupa_missing_indx] <- TRUE
  df$missing_fsoc <- FALSE
  df$missing_fsoc[fsoc_missing_indx] <- TRUE

  print(sprintf("Missing JCATS: %i", sum(jcat_missing_indx)))
  print(sprintf("Missing CUPAS: %i", sum(cupa_missing_indx)))
  print(sprintf("Missing SOCS:  %i", sum(fsoc_missing_indx)))
  return(df)
}
