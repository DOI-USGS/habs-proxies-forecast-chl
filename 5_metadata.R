# sourcing functions

# packages needed for these targets
tar_option_set(packages = c(
  "EFIstandards",
  "neon4cast",
  "tsibble")
)

# target list
p5_targets_list = list(
  
  tar_target(
    p5_metadata_to_submit_xml,
    neon4cast::generate_metadata(forecast_file = p3_all_forecasts_csv,
                                 team_list = p0_team_list,
                                 model_metadata =  p5_model_metadata)
  ),
  
  tar_target(
    p5_model_metadata, 
    list(
      forecast = list(
        model_description = list(
          forecast_model_id =  "air2waterSat",  
          name = "Linear regression of air temperature and precipitation to chl-a", 
          type = "empirical",  
          repository = "https://github.com/USGS-R/habs-proxies-forecast-chl" 
        ),
        initial_conditions = list(
          status = "absent"
        ),
        drivers = list(
          status = "propagates",
          complexity = 3, 
          propagation = list( 
            type = "ensemble", 
            size = p3_n_en) 
        ),
        parameters = list(
          status = "absent"
        ),
        random_effects = list(
          status = "absent"
        ),
        process_error = list(
          status = "absent"
        ),
        obs_error = list(
          status = "absent"
        )
      )
    )
  )
  
)