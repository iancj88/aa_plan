reduce_df_timespan <- function(df, timespan, dttm_col, key_col) {
  require(dplyr)
  require(lubridate)

  # Basic error checking.
  # acceptable_timespans <- c("days", "weeks", "months", "years")
  # stopifnot(dttm_col %in% names(df),
  #           key_col %in% names(df),
  #           timespan %in% acceptable_timespans)

  # depending on timespan, wipe out less significant date/time values
  if (timespan == "days") {
    hour(df[,dttm_col]) <- 0
    minute(df[,dttm_col]) <- 0
    second(df[,dttm_col]) <- 0
  } else if (timespan == "months") {
    day(df[,dttm_col]) <- 1
  } else if (timespan == "years") {
    month(df[,dttm_col]) <- 1
  }


  # dttm_col <- enquo(dttm_col)
  # key_col <- enquo(key_col)
  # print(dttm_col)
  # print(key_col)

  # remove the duplicate rows now that the date-times are truncated

  df <- distinct(df, !!dttm_col, !!key_col, .keep_all = TRUE)

  return(df)
}

