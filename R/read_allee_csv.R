read_allee_csv <- function(path, name) {
  require(data.table)
  # compute the date of the file to determine the column types
  # contained in it. This date will be placed into it's own column
  # after it is read into a data.table
  date_from_file_name <- get_date_from_allee_file_names(name)

  col_fread_types <- all_ee_col_types(date = date_from_file_name)

  dt <- fread(path,
              header = TRUE,
              sep = ";",
              colClasses = col_fread_types,
              skip = 12)

  # dt <- data.table(dt)
  df <- setDF(dt)
  # place the date into it's own column
  df$date <- date_from_file_name

  return(df)
}