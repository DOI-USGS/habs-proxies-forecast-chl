library(targets)

issue_dates <- as.character(seq(from = as.Date('2022-08-28'),
                                to = as.Date('2022-12-06'),
                                by = 'day'))

for(date in issue_dates) {
  #set environment variable
  Sys.setenv(ISSUE_TIME = date)
  message("Starting ", date)
  tar_make() 
  message("Done with ", date)
}
