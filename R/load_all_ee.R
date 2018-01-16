load_all_ee <- function(filepath,
                        most_recent_only = TRUE,
                        opt_start_date,
                        opt_end_date) {
  require(stringr)
  require(dplyr)
  require(data.table)

  # check that the input filepath leads to a valid directory
  stopifnot(dir.exists(filepath))

  # check that the directory is not empty
  stopifnot(length(list.files(path = filepath)) > 0)

  # check that the directory contains a .txt file for parsing
  file_names_only <- list.files(filepath, full.names = FALSE)
  stopifnot(sum(grepl(".txt", file_names_only)) > 0)

  # get the full paths to the files in the directory
  file_names_paths <- list.files(filepath, full.names = TRUE)

  # only keep those files that are .txt
  file_names_paths <- file_names_paths[grepl(".txt",
                                             file_names_only)]
  file_names_only <- file_names_only[grepl(".txt",
                                           file_names_only)]
  # if only the most recent all ee file is to be loaded (for time
  #   considerations), select the final final file in the list
  if (most_recent_only == TRUE) {
    last_file_indx <- length(file_names_only)
    file_names_paths <- file_names_paths[last_file_indx]
    file_names_only <- file_names_only[last_file_indx]
  } else if ((most_recent_only == FALSE) &
                (!missing(opt_start_date)) &
                (!missing(opt_end_date))) {
    file_dates <- get_date_from_allee_file_names(file_names_only)
    file_names_paths <- file_names_paths[((file_dates >= opt_start_date) &
                                          (file_dates <= opt_end_date))]

    file_names_only <- file_names_only[((file_dates >= opt_start_date) &
                       (file_dates <= opt_end_date))]
  }

  # use data table fread (because it is fast) to load the csvs
  loaded_data <- mapply(read_allee_csv,
                        path = file_names_paths,
                        name = file_names_only,
                        SIMPLIFY = FALSE)

  # combine the loaded data and change to dataframe
  loaded_data <- bind_rows(loaded_data)

  # create a snapshot
  max_date <- max(loaded_data$date)
  rds_name <- paste0("X:/Employees/All EEs Reports/all_ee_compiled-",
                     max_date,
                     ".RDS")

  saveRDS(loaded_data, rds_name)

  # Format the date columns out of their screwy dd-MMM-yy format
  loaded_data <- format_allEE_dates(loaded_data) %>%
    fix_native_org_names() %>%
    supplement_all_ee()

  # if only a subset of the data is needed filter by the date
  # if ((most_recent_only == FALSE) &
  #     (!missing(opt_start_date)) &
  #     (!missing(opt_end_date))) {
  #   loaded_data <- filter(loaded_data,
  #                         date >= opt_start_date,
  #                         date <= opt_end_date)
  # }

  return(loaded_data)
}
