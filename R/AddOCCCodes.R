AddOCCCodes <- function(all_ee, SOC_col_name, OCC_lu_df) {
  require(dplyr)

  # Build a SOC-OCC lookup table

  # get the unique federal soc values from the given all employees report
  # this dataframe will form the basis for the returned lookup table
  # an additional occ column will be added
  SOC_col_name <- quo(SOC_col_name)
  new_occ_lu <- distinct(all_ee, `FED SOC Code`) %>%
    RenameColumn(old_name = "FED SOC Code",
                 new_name = "soc_orig")


  # take out unneeded columns from occ table copied from
  # https://www.eeoc.gov/federal/directives/00-09opmcode.cfm
  OCC_lu_df <- select(OCC_lu_df,
                      `SOC CODE`,
                      `CENS CODE`)

  # the occ table uses 'X' and 'Y' as wildcards in their crosswalk
  # remove these characters to join the occs' of partial socs
  OCC_lu_df$`SOC CODE` <- gsub(pattern = "[X,Y]",
                               replacement = "",
                               OCC_lu_df$`SOC CODE`)

  # the initial join will pull in occ codes for those occs with a direct,
  # non-wildcard soc match
  new_occ_lu <- left_join(new_occ_lu,
                          OCC_lu_df,
                          by = c("soc_orig" = "SOC CODE"))

  # the new_occ_lu is reserved for socs with an occ match
  soc_na <- filter(new_occ_lu, is.na(new_occ_lu$`CENS CODE`))
  new_occ_lu <- filter(new_occ_lu, !is.na(new_occ_lu$`CENS CODE`))

  # the na socs might have an occ match using one of the wildcards
  # because the wildcards were dropped the socs in teh occ crosswalk,
  # shave off the last digit(s) of the soc to see if there is a match

  for (i in 6:3) {
    soc_na <- select(soc_na, -`CENS CODE`)

    # shave off a number of the last digits to check for wildcard matches
    soc_na$soc_temp <- substr(soc_na$soc_orig, 1, i)
    soc_na <- left_join(soc_na,
                        OCC_lu_df,
                        by = c("soc_temp" = "SOC CODE"))

    # if there's a match to the shortened soc, bind it to the final output df
    code_out_temp <- filter(soc_na, !is.na(soc_na$`CENS CODE`)) %>%
      select(-soc_temp)
    new_occ_lu <- bind_rows(new_occ_lu, code_out_temp)

    # consolidate the nonmatches to be passed to the beginning of the loop
    soc_na <- filter(soc_na, is.na(soc_na$`CENS CODE`))
  }


  new_occ_lu <- select(new_occ_lu, soc_orig, `CENS CODE`)#%>%
    #distinct(soc_orig, .keep_all = TRUE)

  return(new_occ_lu)
}

consolidate_complete <- function(code_df, na_df, complete_df) {

}

FillOCCLu <- function(OCC_lu_df) {
  require(readr)
  require(dplyr)
  occ_lu <- read_csv(file = "./occ_lu.csv")
  occ_with_socs <- filter(occ_lu, !grepl("[X,Y]$", occ_lu$`SOC CODE`))
  tst_str <- "00-0XXX"

  occ_no_socs <- filter(occ_lu, grepl("[X,Y]$", occ_lu$`SOC CODE`))
  str_replacement <- as.character(0:9)

  tstlapply <- mapply(gsub, pattern = "[X,Y]", replacement = str_replacement, x = tst)

}

Count_Chars <- function(list) {
  cnt <- sum(grepl("[X,Y]", unlist(strsplit(list, ""))))
  return(cnt)
}

Build_Number_String <- function(list_of_lengths) {
  numbers <- seq(0, (10 ^ list_of_lengths) - 5)
}
