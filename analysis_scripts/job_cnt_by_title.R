# Analyze job title total positions

require(lubridate)
require(tidyr)

# remove the students and grads. THis would also be the place to filter down to
# specific departments etc.

job_totals <- filter(all_ee,!EMRJobType %in% c("Student", "Grad. Asst."))

#use month not days in dates
job_totals$month_year <- job_totals$date
day(job_totals$month_year) <- 01

job_totals$job_month_key <- paste0(job_totals$job_key,
                                   job_totals$month_year)
job_totals <- distinct(job_totals,
                       job_month_key,
                       .keep_all = TRUE)

job_totals <- group_by(job_totals, month_year, `Position Title`) %>%
  summarize(job_cnt = n(), fte_sum = sum(FTE))

culinary_regex <- "Cook|Culinary|Baker|Custodian|Cashier"

culinary_jobs <- str_detect(job_totals$`Position Title`,
                           culinary_regex)
job_totals_cul <- job_totals[culinary_jobs,]

# make the data wide

job_totals2 <- spread(job_totals,
                       key = "month_year",
                       value = "job_cnt")
head

job_totals_cul_wide <- spread(job_totals_cul,
                              month_year,
                              job_cnt,
                              -(month_year:fte_sum))

job_totals_bkr <- job_totals_cul[str_detect(job_totals_cul$`Position Title`,
                                             "Baker"),]

job_totals_cul <- group_by(job_totals_cul, `Position Title`)
tst_plt <- ggplot(job_anlyst, aes(x = month_year,
                                      y = job_cnt,
                                      col = `Position Title`)) +
  geom_line()
tst_plt

job_anlyst <- job_totals[str_detect(job_totals$`Position Title`,
                                    "Data/Research Analyst"),]

job_totals_one <- group_by(job_totals, `Position Title`) %>%
  summarize(mn = mean(job_cnt)) %>%
  filter(mn == 1)
