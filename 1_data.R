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
    # drivers for training models 
    p1_historic_drivers_noaa_gefs,
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
      Sys.setenv("AWS_S3_ENDPOINT"="ecoforecast.org")
      neon4cast::get_stacked_noaa_s3("1_data/in", 
                                     site = p0_forecast_site_ids, 
                                     averaged = TRUE,
                                     s3_region="data")
      # the neon4cast function just returns T/F but we want the file path. So 
      #  returning the file path below 
      return(sprintf("1_data/in/drivers/noaa/NOAAGEFS_1hr_stacked_average/%s/observed-met-noaa_%s.nc",
                     p0_forecast_site_ids, p0_forecast_site_ids)) 
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
  )
  

  
  
  
)