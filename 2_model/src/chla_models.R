
#'
#' 
train_model <- function(driver_file,
                        driver_vars,
                        target_file, 
                        target_vars,
                        site, 
                        out_file){
   
  target <- filter(target_file, siteID == site) %>% 
    select(time, siteID, !!target_vars)
   
  drivers <- readRDS(driver_file) %>% 
    filter(siteID == site) %>% 
    select(time, all_of(driver_vars)) %>% 
    # taking mean of ensemble members 
    group_by(time) %>% 
    summarise(across(all_of(driver_vars), ~mean(.x, na.rm = T)), 
              .groups = "drop")
  
  all_data <- left_join(target, drivers, by = "time") %>% 
    filter_at(driver_vars, all_vars(!is.na(.))) %>% 
    mutate(chla_lagged_1 = dplyr::lag(chla, n = 1)) %>% 
    slice(2:n()) 
  
  model <- lm(chla ~ chla_lagged_1 + 
                air_temperature + 
                surface_downwelling_shortwave_flux_in_air + 
                precipitation_flux, 
              data = all_data) 
  summary(model)
  
  saveRDS(model, out_file) 
  return(out_file) 
}



predict_chla <- function(
    trained_model,
    chla_lagged_1,
    air_temp,
    swrad,
    precip)
{
  chla = trained_model$coefficients[1] + 
    trained_model$coefficients[2]*chla_lagged_1 + 
    trained_model$coefficients[3]*air_temp + 
    trained_model$coefficients[4]*swrad + 
    trained_model$coefficients[5]*precip
  
  return(list(chla = chla))
}
