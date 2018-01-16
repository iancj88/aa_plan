# FTE change
require(dplyr)
require(stringr)
# How many jobs changed FTE in calendar year 2016?

# Check if the all_ee dataset exists, if not stop

stopifnot(exists("all_ee") &&
          is.data.frame(get("all_ee")))

# first check for any job key that has a different fte

fte_dataset <- select(all_ee,
                      job_key,
                      job_date_key,
                      FTE,
                      date)

fte_dataset$job_fte_key <- paste0(fte_dataset$job_key, fte_dataset$FTE)
fte_key <- select(fte_dataset, job_key, job_fte_key) %>%
  distinct() %>%
  group_by(job_key) %>%
  summarize(n = n()) %>%
  filter(n > 1)

fte_flux_jobs <- filter(all_ee, job_key %in% fte_key$job_key) %>%
  select(job_key, Department, `Job Title`, FTE, date)

job_cnt_summary <-  %>%
  distinct(job) %>%
  group_by(job) %>%
  summarize(job_cnt = n())

filter(all_ee, str_detect(all_ee$`Position Title`, "Cook III"))
