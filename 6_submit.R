# sourcing functions

# packages needed for these targets
tar_option_set(packages = c(
  "tsibble")
)

# target list
p6_targets_list = list(
  
  tar_target(
    p6_submit,
    {
      Sys.setenv("AWS_DEFAULT_REGION" = "data",
                 "AWS_S3_ENDPOINT" = "ecoforecast.org")
      
      neon4cast::submit(forecast_file = forecast_file, 
                        metadata = p5_metadata_to_submit, 
                        ask = FALSE)
    }

  )
  
)