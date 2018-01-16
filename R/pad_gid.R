PadGID <- function(gid_vec) {
  gid_length <- 9

  if (sum(nchar(gid_vec) > gid_length,  na.rm = TRUE) > 0) {
    return(gid_vec)
  }

  gid_numbers <- as.numeric(gid_vec)
  gid_vec <- sprintf("%09d", gid_numbers)
  return(gid_vec)
}