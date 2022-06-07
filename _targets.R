# main target script for calling all subsequent targets
library(targets)
library(tarchetypes)
library(tidyverse)

options(tidyverse.quiet = TRUE,
        clustermq.scheduler = "multicore")

source("0_config.R")
source("1_data.R") 
source("2_model.R") 
source("3_forecast.R")

# complete list of targets
c(p0_targets_list, 
  p1_targets_list,
  p2_targets_list,
  p3_targets_list) 

# Instructions for environment variables when running in github actions 
# 1) Run install.packages("renv") in R.
# 2) After the installation of renv finishes, run renv::activate() and renv::restore()
# 3) Modify forecast_model.R to make your forecast model . Many of the components you need to generate the
     # forecast, including downloading NOAA weather forecasts, downloading target data, generating forecast
     # files, generating metadata, validating files, and submitting forecasts. Avoid running the 
     # neon4cast::submit() function at the end of forecast_model.R until you are ready 
     # to submit a forecast to the Challenge. It is important that you do NOT change 
     # the name of the file. GitHub Actions (below) is looking for this file name. 
     # Be sure to change your team_name and team_list
# 4) Run renv::snapshot() to update the renv.lock file with any new packages 
     # that you have added. renv is a package that helps manage R packages. 
     # It ensures that GitHub actions runs the exact versions of packages that 
     # you desire. https://rstudio.github.io/renv/index.html
# 5) Commit and push the changes to renv.lock to Github.

