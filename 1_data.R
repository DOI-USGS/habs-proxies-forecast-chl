# sourcing functions
# source("1_data/src/")

# packages needed for these targets
tar_option_set(packages = c(
  "aws.s3", 
  "neon4cast",
  "tsibble")
)

# target list
p1_targets_list = list(
  
  # all targets (i.e. observations) for the forecast sties 
  tar_target(
    p1_aquatic_targets,
    aquatic <- read_csv("https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz") %>% 
      dplyr::filter(siteID %in% p0_forecast_site_ids) %>% 
      as_tsibble(index = time, key = siteID),
    cue = tar_cue(mode = "always")
  ),
  
  tar_target(
    p1_historic_drivers_noaa_gefs,
    purrr::
    for(i in 1:length(sites)){
      neon4cast::get_stacked_noaa_s3(".", site = sites[i], averaged = FALSE)
    }
  )
  
  # # install.packages("remotes")
  # remotes::install_github("cboettig/aws.s3") 
  # remotes::install_github("eco4cast/neon4cast")
  # 
  # library(neon4cast) 
  # library(aws.s3) 
  # 
  # # also need to manually rename .aws/ credential and config files 
  # Sys.setenv("AWS_S3_ENDPOINT"="ecoforecast.org")
  # 
  # neon4cast::get_stacked_noaa_s3(".",site = "POSE", averaged = FALSE, s3_region="data")
  
  
  
  
)