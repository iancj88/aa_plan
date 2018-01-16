format_allEE_dates <- function(df) {

  require(lubridate)

  date_cols <- c("Current Hire Date",
                 "Campus Orig. Hire",
                 "Longevity Date",
                 "Anniversary Date",
                 "Last Work Date",
                 "Job Begin Date",
                 "Anniversary Date",
                 "Birth Date")

  date_cols_indx <- which(names(df) %in% date_cols)

  for (col in date_cols_indx) {
    df[,col] <- parse_date_time2(df[,col], "%d-%b-%y")

    # the year is stored as a two digit number making it difficult to parse
    # properly. '80' may be interpreted as 1980 or 2080. if the date was
    # parsed as the future, subtract 100 from it.
    misread_years <- which(year(df[,col]) > year(Sys.Date()))
    if (length(misread_years) > 0) {
      year(df[misread_years, col]) <- year(df[misread_years, col]) - 100
      misread_years <- NULL
    }
  }

  return(df)
}