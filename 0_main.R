load_src_files <- function(folder_path) {
# Load up the functions stored in the ./R/ folder
  file.sources <- list.files(path = folder_path,
                             pattern = "\\.R$",
                             full.names = TRUE)
  require(magrittr)

  if (!length(file.sources)) {
    stop(simpleError(sprintf('No R Source files found')))
  }
  src <- invisible(lapply(file.sources, source))
  message(sprintf('%s files successfully sourced.', length(src)))
}

load_src_files(folder_path = "./R/")
csv_path <- "X:/Employees/All EEs Reports/csv_src/"
all_ee <- load_all_ee(csv_path) %>%
  supplement_all_ee()
rm(csv_path)

