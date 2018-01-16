get_fiscal_year <- function(date) {
  require(data.table)

  qrtr <- quarter(date)
  third_fourth_quarter <- (qrtr > 2)

  date <- year(date)
  date[third_fourth_quarter] <- date[third_fourth_quarter] + 1

  return(date)
}