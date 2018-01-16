get_char_col_specs <- function(file_list, ...) {
  require(readr)
  col_length <- lapply(file_list, read_csv, n_max = 1, ...) %>%
    lapply(ncol)

  c_spec <- paste0(rep("c", col_length),
                   collapse = "")

  return(c_spec)
}