all_ee_col_types <- function(date) {
  # Current column names in all ee as of 2017/12/27:

  # [1] "GID"                    "Last Name"
  # [3] "First Name"             "Home Street 1"
  # [5] "Home Street 2"          "Home Street 3"
  # [7] "City"                   "State"
  # [9] "Zip"                    "Campus"
  # [11] "Pict Code"              "Department"
  # [13] "Home Orgn Number"       "Budget Org."
  # [15] "Budget Org. Long Desc." "Org. Heirarchy"
  # [17] "Job Title"              "Status"
  # [19] "PEAEMPL ECLS"           "ECLS Description"
  # [21] "MUS"                    "Position Number"
  # [23] "Suffix"                 "Position Title"
  # [25] "FTE"                    "Job Type"
  # [27] "Pays"                   "Current Hire Date"
  # [29] "Campus Orig. Hire"      "Longevity Date"
  # [31] "Annual Lv Accrual"      "Anniversary Date"
  # [33] "Last Work Date"         "Job Begin Date"
  # [35] "Employee Group"         "Hourly Rate"
  # [37] "Annual Salary"          "Assgn Salary"
  # [39] "Retirement"             "Union"
  # [41] "Union Deduction"        "BCAT"
  # [43] "Leave Category"         "Sex"
  # [45] "Race 1"                 "Birth Date"
  # [47] "SOC Code"               "SOC Description"
  # [49] "Email"                  "Phone"
  # [51] "Index"                  "Fund"
  # [53] "Orgn"                   "Account"
  # [55] "Program"                "Percent"
  # [57] "date"                   "CUPA Code"
  # [59] "CUPA Desc."             "FED SOC Code"
  # [61] "FED SOC Code Desc."     "MUS SOC Code"
  # [63] "MUS SOC Code Desc."     "JCAT Code"
  # [65] "JCAT Desc."

  # column names in all ee as prior to 2017/12/15:

  # [1] "GID"                    "Last Name"
  # [3] "First Name"             "Home Street 1"
  # [5] "Home Street 2"          "Home Street 3"
  # [7] "City"                   "State"
  # [9] "Zip"                    "Campus"
  # [11] "Pict Code"              "Department"
  # [13] "Home Orgn Number"       "Budget Org."
  # [15] "Budget Org. Long Desc." "Org. Heirarchy"
  # [17] "Job Title"              "Status"
  # [19] "PEAEMPL ECLS"           "ECLS Description"
  # [21] "MUS"                    "Position Number"
  # [23] "Suffix"                 "Position Title"
  # [25] "FTE"                    "Job Type"
  # [27] "Pays"                   "Current Hire Date"
  # [29] "Campus Orig. Hire"      "Longevity Date"
  # [31] "Annual Lv Accrual"      "Anniversary Date"
  # [33] "Last Work Date"         "Job Begin Date"
  # [35] "Employee Group"         "Hourly Rate"
  # [37] "Annual Salary"          "Assgn Salary"
  # [39] "Retirement"             "Union"
  # [41] "Union Deduction"        "BCAT"
  # [43] "Leave Category"         "Sex"
  # [45] "Race 1"                 "Birth Date"
  # [47] "SOC Code"               "SOC Description"
  # [49] "Email"                  "Phone"
  # [51] "Index"                  "Fund"
  # [53] "Orgn"                   "Account"
  # [55] "Program"                "Percent"
  # [57] "date"

  all_ee_v2_date <- as.POSIXct("2017/12/15")
  if (date < all_ee_v2_date) { # This is the older version
    col_types <- list(character = c("GID",
                                    "Last Name",
                                    "First Name",
                                    "Home Street 1",
                                    "Home Street 2",
                                    "Home Street 3",
                                    "City",
                                    "State",
                                    "Zip",
                                    "Campus",
                                    "Pict Code",
                                    "Department",
                                    "Home Orgn Number",
                                    "Budget Org.",
                                    "Budget Org. Long Desc.",
                                    "Org. Heirarchy",
                                    "Job Title",
                                    "Status",
                                    "PEAEMPL ECLS",
                                    "ECLS Description",
                                    "MUS",
                                    "Position Number",
                                    "Suffix",
                                    "Position Title",
                                    "Job Type",
                                    "Current Hire Date",
                                    "Campus Orig. Hire",
                                    "Longevity Date",
                                    "Annual Lv Accrual",
                                    "Anniversary Date",
                                    "Last Work Date",
                                    "Job Begin Date",
                                    "Employee Group",
                                    "Retirement",
                                    "Union",
                                    "Union Deduction",
                                    "BCAT",
                                    "Leave Category",
                                    "Sex",
                                    "Race 1",
                                    "Birth Date",
                                    "SOC Code",
                                    "SOC Description",
                                    "Email",
                                    "Phone",
                                    "Index",
                                    "Fund",
                                    "Orgn",
                                    "Account",
                                    "Program"),
                      numeric = c("FTE",
                                  "Pays",
                                  "Hourly Rate",
                                  "Annual Salary",
                                  "Assgn Salary",
                                  "Percent"))
  } else {
    # This is the newer all ee version with removed SOC Code,
    # SOC Description columns and added CUPA, JCAT, SOC FED, and SOC
    # MUS columns specified
    col_types <- list(character = c("GID",
                                    "Last Name",
                                    "First Name",
                                    "Home Street 1",
                                    "Home Street 2",
                                    "Home Street 3",
                                    "City",
                                    "State",
                                    "Zip",
                                    "Campus",
                                    "Pict Code",
                                    "Department",
                                    "Home Orgn Number",
                                    "Budget Org.",
                                    "Budget Org. Long Desc.",
                                    "Org. Heirarchy",
                                    "Job Title",
                                    "Status",
                                    "PEAEMPL ECLS",
                                    "ECLS Description",
                                    "MUS",
                                    "Position Number",
                                    "Suffix",
                                    "Position Title",
                                    "Job Type",
                                    "Current Hire Date",
                                    "Campus Orig. Hire",
                                    "Longevity Date",
                                    "Annual Lv Accrual",
                                    "Anniversary Date",
                                    "Last Work Date",
                                    "Job Begin Date",
                                    "Employee Group",
                                    "Retirement",
                                    "Union",
                                    "Union Deduction",
                                    "BCAT",
                                    "Leave Category",
                                    "Sex",
                                    "Race 1",
                                    "Birth Date",
                                    "Email",
                                    "Phone",
                                    "Index",
                                    "Fund",
                                    "Orgn",
                                    "Account",
                                    "Program",
                                    "CUPA Code",
                                    "CUPA Desc.",
                                    "FED SOC Code",
                                    "FED SOC Code Desc.",
                                    "MUS SOC Code",
                                    "MUS SOC Code Desc.",
                                    "JCAT Code",
                                    "JCAT Desc."),
                      numeric = c("FTE",
                                  "Pays",
                                  "Hourly Rate",
                                  "Annual Salary",
                                  "Assgn Salary",
                                  "Percent"))
  }

  return(col_types)
}