# sourcing functions
source("4_visualize/src/visualize_forecast.R")

# packages needed for these targets
tar_option_set(packages = c(
  "ggplot2")
)

# target list
p4_targets_list = list(
  
  tar_target(
    p4_forecast_visual_png,
    visualize_forecast(forecast_file = p3_all_forecasts_csv,
                       obs = p1_aquatic_targets,
                       out_file = sprintf("4_visualize/out/%s_forecast.png", 
                                          p1_forecast_issue_date))
    
  )
  
)