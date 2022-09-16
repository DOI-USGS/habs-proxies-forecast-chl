# sourcing functions
source("1_data/src/summarize_drivers.R")
source("1_data/src/nwm_utils.R")

# packages needed for these targets
tar_option_set(packages = c(
  "aws.s3", 
  "lubridate",
  "neon4cast",
  "tsibble")
)

# target list
p1_targets_list = list(
  
  tar_target(
    # issue date of the forecast; setting to system time 
    p1_forecast_issue_date, 
    # as.Date("2022-09-15"),
    Sys.Date(), # met forecasts aren't available until the next day 
    # as.Date(Sys.getenv("ISSUE_TIME")), # for testing the model with loop_tar_make.R 
    cue = tar_cue(mode = "always")
  ),
  
  # all targets (i.e. observations) for the forecast sties 
  tar_target(
    p1_aquatic_targets,
    aquatic <- read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") %>% 
      dplyr::filter(site_id %in% p0_forecast_site_ids) %>%  
      distinct() %>% 
      as_tsibble(index = datetime, key = c(site_id, variable)),
    cue = tar_cue(mode = "always")
  ),
  
  tar_target(
    # drivers for training models 
    p1_historic_drivers_noaa_gefs_rds,
    # # install.packages("remotes")
    # remotes::install_github("cboettig/aws.s3") 
    # remotes::install_github("eco4cast/neon4cast")
    # remotes::install_github("eco4cast/EFIstandards")
    # 
    # library(neon4cast) 
    # library(aws.s3) 
    # 
    # # also need to manually rename .aws/ credential and config files 
    # Sys.setenv("AWS_S3_ENDPOINT"="ecoforecast.org")
    # 
    # neon4cast::get_stacked_noaa_s3(".",site = "POSE", averaged = FALSE, s3_region="data")
    # neon4cast::noaa_stage1()
    {
      p1_forecast_issue_date
      Sys.setenv("AWS_DEFAULT_REGION" = "data",
                 "AWS_S3_ENDPOINT" = "ecoforecast.org")
      
      out_file = "1_data/out/historic_gefs.rds"
      
      # connect to db 
      historic_gefs <- neon4cast::noaa_stage3()
      
      # filter to sites we want and then pull down to local tibble 
      historic_gefs %>% 
        dplyr::filter(site_id %in% p0_forecast_site_ids) %>% 
        dplyr::collect() %>% 
        saveRDS(file = out_file) 
       
      return(out_file) 
    },
    cue = tar_cue("always")
  ),

  
  tar_target(
    # get forecasted drivers for today's forecast issue date 
    p1_forecasted_drivers_rds,
    {
      Sys.setenv("AWS_DEFAULT_REGION" = "data",
                 "AWS_S3_ENDPOINT" = "ecoforecast.org")

      out_file = "1_data/out/forecasted_gefs.rds"
       
      # connect to db 
      forecasted_gefs <- neon4cast::noaa_stage2()
      date_to_download <- p1_forecast_issue_date - 1
      # filter to sites we want and then pull down to local tibble 
      forecasted_gefs %>% 
        dplyr::filter(site_id %in% p0_forecast_site_ids,
                      start_date == as.character(date_to_download)) %>% 
        dplyr::collect() %>% 
        saveRDS(file = out_file) 
      
      return(out_file) 
    },
    cue = tar_cue(mode = "always")
  ), 
  
  tar_target(
    p1_met_drivers,
    # all the met drivers pulled from GEFS; to be used in summarize function below
    c("air_temperature",
      "air_pressure",
      "relative_humidity",
      "surface_downwelling_longwave_flux_in_air",
      "surface_downwelling_shortwave_flux_in_air",
      "precipitation_flux",
      "eastward_wind", 
      "northward_wind")
  ),
  
  # current drivers are in 1 hour intervals and we want to summarize to daily means 
  tar_target(
    p1_historic_gefs_daily_rds,
    summarize_drivers(in_file = p1_historic_drivers_noaa_gefs_rds,
                      vars = p1_met_drivers, 
                      group_by_ens = TRUE, 
                      out_file = "1_data/out/historic_gefs_daily.rds"),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    p1_forecasted_gefs_daily_rds,
    summarize_drivers(in_file = p1_forecasted_drivers_rds,
                      vars = p1_met_drivers, 
                      group_by_ens = TRUE, 
                      out_file = "1_data/out/forecasted_gefs_daily.rds"),
    cue = tar_cue(mode = "always")
  )
  
)