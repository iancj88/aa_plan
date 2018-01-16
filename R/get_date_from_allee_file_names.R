get_date_from_allee_file_names <- function(file_list) {
  date_from_file_name <- gsub(" All Employees.txt", "", file_list) %>%
    as.POSIXct(format = "%Y%m%d")
  return(date_from_file_name)
}
