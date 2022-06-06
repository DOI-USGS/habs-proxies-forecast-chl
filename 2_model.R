# sourcing functions
source("2_model/src/chla_models.R")
# source("1_data/src/nc_utils.R")

# packages needed for these targets
tar_option_set(packages = c(
  "aws.s3", 
  "ncdf4", 
  "neon4cast",
  "tsibble")
)

# target list
p2_targets_list = list(
  
  tar_target(
    # model to train and use for forecast 
    p2_model_type,
    "linear"
  ),
  
  tar_target(
    # covariates of the model 
    p2_driver_vars,
    c("air_temperature",
      "surface_downwelling_shortwave_flux_in_air",
      "precipitation_flux")
  ),
  
  tar_target(
    # train models for each site 
    p2_train_model,
    train_model(driver_file = p1_historic_gefs_daily_rds,
                driver_vars = p2_driver_vars,
                target_file = p1_aquatic_targets, 
                target_vars = p0_target_var,
                site = p0_forecast_site_ids,
                out_file = sprintf("2_model/out/%s_%s.rds", 
                                   p0_forecast_site_ids, 
                                   p2_model_type)),
    pattern = map(p0_forecast_site_ids)
  )
  
  
)