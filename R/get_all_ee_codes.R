get_all_ee_codes <- function(df) {
  # filter the dataframe to only include the most recent
  # all ee data. Select the code values
  require(dplyr)

  df_new <- filter(df, date == max(df$date)) %>%
    select(job_key,
           `JCAT Code`,
           `JCAT Desc.`,
           `CUPA Code`,
           `CUPA Desc.`,
           `FED SOC Code`,
           `FED SOC Code Desc.`)

  return(df_new)
}