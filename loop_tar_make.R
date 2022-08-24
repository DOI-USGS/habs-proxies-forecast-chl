library(targets)

issue_dates <- as.character(seq(from = as.Date('2022-06-30'),
                                to = as.Date('2022-08-20'),
                                by = 'day'))

for(date in issue_dates) {
  #set environment variable
  Sys.setenv(ISSUE_TIME = date)
  message("Starting ", date)
  tar_make() 
  message("Done with ", date)
}
