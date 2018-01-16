fix_native_org_names <- function(df) {
  # overwrite various 'inconsistent' names for orgs
  isProvost <- (df$`Org. Heirarchy` == "Other Provost")
  df[isProvost,"Org. Heirarchy"] <- "Provost"

  isNursing <- (df$Department %in% c("College of Nursing Billings",
                                     "College of Nursing Great Falls",
                                     "College of Nursing Missoula"))
  df[isNursing, "Org. Heirarchy"] <- "Nursing"

  isExtended <- (df$Department %in% c("Extended University NTEN",
                                      "Extended University",
                                      "Extended University Director"))
  df[isExtended, "Org. Heirarchy"] <- "Extended University"

  isAg <- (df$Department == "AES EARC")
  df[isAg, "Org. Heirarchy"] <- "Agriculture"

  isPres <- (df$Department == "Museum of the Rockies")
  df[isPres, "Org. Heirarchy"] <- "President"

  isBusiness <- (df$Department == "TS College of Business")
  df[isBusiness, "Org. Heirarchy"] <- "College of Business"

  return(df)
}