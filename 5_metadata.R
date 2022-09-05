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
    p5_forecast_model_id,
    "rf_met_drivers"
  ),
  
  tar_target(
    p5_n_model_drivers,
    {
      model = read_rds(p2_train_model[1])
      return(nrow(model$importance))
    }
  ),
  
  tar_target(
    p5_n_model_parameters,
    {
      model = read_rds(p2_train_model)
      # find number of feature splits. this is analogous to number of parameters
      feature_splits = randomForest::treesize(model, terminal = F) - randomForest::treesize(model, terminal = T)
      return(floor(median(feature_splits)))
    },
    pattern = map(p2_train_model)
  ),
  
  tar_target(
    p5_metadata_to_submit_xml,
    neon4cast::generate_metadata(forecast_file = p3_all_forecasts_csv,
                                 team_list = p0_team_list,
                                 model_metadata =  p5_model_metadata,
                                 forecast_issue_time = p1_forecast_issue_date),
  ),
  
  tar_target(
    p5_model_metadata, 
    list(
      forecast = list(
        model_description = list(
          forecast_model_id = p5_forecast_model_id, # model identifier:
          name = "Random Forest regression of meteorological drivers to predict chl-a", #Name or short description of model
          type = "machine learning", #General type of model empirical, machine learning, process
          repository = "https://github.com/USGS-R/habs-proxies-forecast-chl" # put your GitHub Repository in here
        ),
        initial_conditions = list(
          status = "assimilates", #options: absent, present, data_driven, propagates, assimilates
          complexity = 1, #How many models states need initial conditions; delete if status = absent
          #Delete list below if status = absent, present, or data_driven
          propagation = list(
            type = "ensemble", #How does your model propogate initial conditions ('ensemble' is most common)
            size = 3100 #number of ensemble members
          ),
          #Delete list below UNLESS status = assimilates
          assimilation = list(
            type = "EnKF", #description of assimilation method
            reference = "Zwart et al. 2021 doi.org/10.31223/X55K7G", #reference for assimilation method
            complexity = 1 #number of states that are updated with assimilation
          )
        ),
        drivers = list(
          status = "propagates", #options: absent, present, data_driven, propagates, assimilates
          complexity = p5_n_model_drivers, #How many drivers are used? Delete if status = absent
          #Delete list below if status = absent, present, or data_driven
          propagation = list(
            type = "ensemble", #How does your model propogate driver (ensemble or MCMC is most common
            size = 31 #number of ensemble or MCMC members
          )
        ),
        parameters = list(
          status = "data_driven", #options: absent, present, data_driven, propagates, assimilates
          complexity = floor(mean(p5_n_model_parameters)) #How many parameters are included?; Delete if status = absent
        ),
        random_effects = list(
          status = "absent" #options: absent, present, data_driven, propagates, assimilates
        ),
        process_error = list(
          status = "assimilates", #options: absent, present, data_driven, propagates, assimilates
          complexity = 1, #Delete if status = absent
          #Delete the list below below blank if status = absent, present, or data_driven
          propagation = list(
            type = "ensemble", #How does your model propagate random effects uncertainty (ensemble or MCMC is most common)
            size = 3100 #How many ensemble or MCMC members
          ),
          #Delete the list below UNLESS status = assimilates
          assimilation = list(
            type = "EnKF", #Name of data assilimilation method
            reference = "Zwart et al. 2021 doi.org/10.31223/X55K7G", #Reference for data assimilation method
            complexity = 1, #Number of states assimilate
            covariance = TRUE, #TRUE OR FALSE
            localization = FALSE #TRUE OR FALSE
          )
        ),
        obs_error = list(
          status = "data_driven", #options: absent, present, data_driven, propagates, assimilates
          complexity = 1 #Delete if status = absent
        )
      )
    )
  )
)
