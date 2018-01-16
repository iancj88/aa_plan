add_emr_orgs <- function(all_ee) {
  # Function to add a column of data to a dataframe corresponding to
  #   the EMR Organization of the data
  require(dplyr)

  emr_xwalk <-
  all_ee <- left_join(all_ee,
                      emr_xwalk,
                      by = c("Budget Org." = "Dept Number"))
  return(all_ee)
}