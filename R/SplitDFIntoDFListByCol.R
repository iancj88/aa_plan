#Create individual dataframes based on unique values of a single column in a dataframe
SplitDFIntoDFListByCol <- function(df, uniqueColName) {
  #get list of unique ids
  unique_names <- unique(df[[uniqueColName]])
  new_list <- lapply(unique_names, function(x, df) {df[df[[uniqueColName]] %in% c(x),]}, df)
  names(new_list) <- unique_names

  return(new_list)
}