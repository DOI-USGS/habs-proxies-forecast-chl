# sourcing functions
source("1_data/src/summarize_drivers.R")

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
    Sys.Date() 
  ),
  
  # all targets (i.e. observations) for the forecast sties 
  tar_target(
    p1_aquatic_targets,
    aquatic <- read_csv("https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz") %>% 
      dplyr::filter(siteID %in% p0_forecast_site_ids) %>% 
      as_tsibble(index = time, key = siteID),
    cue = tar_cue(mode = "always")
  ),
  
  tar_target(
    # drivers for training models 
    p1_historic_drivers_noaa_gefs_nc,
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
    {
      Sys.setenv("AWS_DEFAULT_REGION" = "data",
                 "AWS_S3_ENDPOINT" = "ecoforecast.org")
      
      neon4cast::get_stacked_noaa_s3("1_data/in", 
                                     site = p0_forecast_site_ids, 
                                     averaged = FALSE,
                                     s3_region="data")
    },
    # 8 variables (excluding dimension variables):
    # float air_temperature[time,latitude,longitude]   
    # units: K
    # _FillValue: NaN
    # float air_pressure[time,latitude,longitude]   
    # units: Pa
    # _FillValue: NaN
    # float relative_humidity[time,latitude,longitude]   
    # units: 1
    # _FillValue: NaN
    # float surface_downwelling_longwave_flux_in_air[time,latitude,longitude]   
    # units: Wm-2
    # _FillValue: NaN
    # float surface_downwelling_shortwave_flux_in_air[time,latitude,longitude]   
    # units: Wm-2
    # _FillValue: NaN
    # float precipitation_flux[time,latitude,longitude]   
    # units: kgm-2s-1
    # _FillValue: NaN
    # float specific_humidity[time,latitude,longitude]   
    # units: 1
    # _FillValue: NaN
    # float wind_speed[time,latitude,longitude]   
    # units: ms-1
    # _FillValue: NaN
    # 3 dimensions:
    # time  Size:14568 
    # units: hours since 2020-09-25 00:00
    # long_name: time
    # latitude  Size:1 
    # units: degree_north
    # long_name: latitude
    # longitude  Size:1 
    # units: degree_east
    # long_name: longitude
    pattern = map(p0_forecast_site_ids)
  ),
  
  tar_target(
    # stack together historic drivers 
    p1_historic_drivers_noaa_gefs_rds,
    {
      out_file = "1_data/out/historic_gefs.rds"
      p1_historic_drivers_noaa_gefs_nc
      saveRDS(neon4cast::stack_noaa(dir = "1_data/in/drivers",
                                    model = "NOAAGEFS_1hr_stacked"), 
              file = out_file)
      return(out_file)
    }
  ),

  
  tar_target(
    # get forecasted drivers for today's forecast issue date 
    p1_forecasted_drivers_nc,
    {
      Sys.setenv("AWS_DEFAULT_REGION" = "data",
                 "AWS_S3_ENDPOINT" = "ecoforecast.org")
      
      neon4cast::get_noaa_forecast_s3(dir = "1_data/in", 
                                      model = "NOAAGEFS_1hr",
                                      site = p0_forecast_site_ids, 
                                      date = p1_forecast_issue_date, 
                                      cycle = "00")
    },
    pattern = map(p0_forecast_site_ids)
  ), 
  
  tar_target(
    # stack together forecasted drivers 
    p1_forecasted_drivers_rds,
    {
      out_file = "1_data/out/forecasted_gefs.rds"
      p1_forecasted_drivers_nc
      saveRDS(neon4cast::stack_noaa(dir = "1_data/in/drivers",
                                    model = "NOAAGEFS_1hr", 
                                    forecast_date = p1_forecast_issue_date),
              file = out_file)
      return(out_file) 
    }
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
      "specific_humidity",
      "wind_speed")
  ),
  
  # current drivers are in 1 hour intervals and we want to summarize to daily means 
  tar_target(
    p1_historic_gefs_daily_rds,
    summarize_drivers(in_file = p1_historic_drivers_noaa_gefs_rds,
                      vars = p1_met_drivers, 
                      group_by_ens = TRUE, 
                      out_file = "1_data/out/historic_gefs_daily.rds")
  ),
  tar_target(
    p1_forecasted_gefs_daily_rds,
    summarize_drivers(in_file = p1_forecasted_drivers_rds,
                      vars = p1_met_drivers, 
                      group_by_ens = TRUE, 
                      out_file = "1_data/out/forecasted_gefs_daily.rds")
  )
  
)