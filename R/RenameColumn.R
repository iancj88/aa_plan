#quickly rename a column based on it's current name rather than location
RenameColumn <- function(df, old_name, new_name) {
  colnames(df)[which(names(df) == old_name)] <- new_name
  return(df)
}