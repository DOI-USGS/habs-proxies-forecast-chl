# sourcing functions
source("3_forecast/src/forecast.R")
source("2_model/src/chla_models.R")

# packages needed for these targets
tar_option_set(packages = c(
  "tsibble")
)

# target list
p3_targets_list = list(
  
  tar_target(
    p3_chla_obs_cv,
    filter(p1_aquatic_targets, siteID == p0_forecast_site_ids,
           !is.infinite(chla), !is.infinite(chla_sd)) %>% 
      mutate(chla_cv = (chla_sd / chla)) %>%
      pull(chla_cv) %>% 
      median(., na.rm = T),
    pattern = map(p0_forecast_site_ids)
  ),
  
  tar_target(
    p3_start_forecast, 
    p1_forecast_issue_date - 40
  ),
  
  tar_target(
    p3_n_en, 
    31
  ),
  
  tar_target(
    # produce forecast for each site 
    p3_forecast_csv,
    forecast(trained_model = p2_train_model,
             n_en = p3_n_en,  
             start = p3_start_forecast,
             stop = p1_forecast_issue_date, 
             f_horizon = 35, # days  
             time_step = "days", 
             obs = p1_aquatic_targets,
             historic_driver_file = p1_historic_gefs_daily_rds, 
             forecasted_driver_file = p1_forecasted_gefs_daily_rds,
             driver_vars = p2_driver_vars, 
             n_states_est = 1, 
             n_params_est = 0,
             n_params_obs = 0, 
             obs_cv = p3_chla_obs_cv,
             init_cond_cv = p3_chla_obs_cv,
             site = p0_forecast_site_ids,
             out_file = sprintf("3_forecast/out/%s_%s_forecast.csv",
                                p0_forecast_site_ids, 
                                p1_forecast_issue_date)),
    pattern = map(p0_forecast_site_ids,
                  p2_train_model,
                  p3_chla_obs_cv)
  ), 
  
  tar_target(
    # combine forecasts from all sites 
    p3_all_forecasts_csv,
    {
      out_file <- paste0("3_forecast/out/aquatics", "-", 
                         p1_forecast_issue_date ,"-", 
                         p0_team_name, ".csv")
      all_forecasts <- read_csv(p3_forecast_csv) %>% bind_rows() 
      write_csv(all_forecasts, out_file) 
      # validate forecast 
      neon4cast::forecast_output_validator(out_file)
      return(out_file) 
    }
  )
  
  
)