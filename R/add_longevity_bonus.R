add_longevity_bonus <- function(df) {
  require(lubridate)
  require(dplyr)

  # Add a longevity raise amount column depending on the years of service
  longevity_date <- df$`Longevity Date`

  # calculate the years of service from the 1st. of the month,
  # that's when the longevity increase goes into effect in payroll
  day(longevity_date) <- 01

  # get an interval class object between the two measurement dates,
  # this interval is converted into the years unit by the duration (dyears)
  # function, i.e. what is the duration of the interval in years?
  yos_interval <- interval(longevity_date,
                           df$date)

  years_of_service <- yos_interval / dyears(x = 1)

  # use the whole integer for the lookup value in the longevity multiplier
  # percent table
  years_of_service <- floor(years_of_service)
  df$`years_of_service` <- years_of_service

  # if the bonus lookup doesn't exist give up and sent the data back as is

  #longevity_mult_lu <- readRDS("./longevity_mult_lu.RDS")
  # longevity rates df stored internally to package. see above RDS.
  df <- left_join(x = df,
                  y = longevity_rates,
                  by = c("years_of_service" = "YearsOfService"))
  df$PercentToBase <- df$PercentToBase + 1

  # only classifieds get the bonus so default in 100% for rest of rows
  if (!"EMRJobType" %in% names(df)) {
    df <- add_emr_job_type(df)
  }
  df[which(!df$EMRJobType == "Classified"), "PercentToBase"] <- 1

  # Now add the meat, potates and gravy
  df$BaseAndLongHourly <- df$PercentToBase * df$`Hourly Rate`
  df$BaseAndLongAssgn <- df$PercentToBase * df$`Assgn Salary`
  df$BaseAndLongAnnual <- df$PercentToBase * df$`Annual Salary`

  return(df)
}