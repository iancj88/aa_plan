supplement_all_ee <- function(df) {
  df <- add_emr_job_type(df) %>%
    add_emr_orgs() %>%
    add_longevity_bonus()

  df$FullName <- paste0(df$`Last Name`,
                        ", ",
                        df$`First Name`)

  df$job_key <- paste0(df$GID,
                       df$`Position Number`,
                       df$Suffix)
  df$job_date_key <- paste0(df$GID,
                            df$`Position Number`,
                            df$Suffix,
                            df$date)

  df$FY <- get_fiscal_year(date = df$date)

  return(df)
}