
#'
#' 
train_model <- function(driver_file,
                        driver_vars,
                        target_file, 
                        target_vars,
                        site, 
                        out_file){
  
  target <- filter(target_file, 
                   site_id == site,
                   variable == target_vars) %>% 
    pivot_wider(names_from = variable, values_from = observed)
   
  drivers <- readRDS(driver_file) %>% 
    filter(site_id == site,
           variable %in% driver_vars) %>% 
    # taking mean of ensemble members 
    group_by(time, variable) %>% 
    summarise(predicted_mean = mean(predicted_mean, na.rm = T), 
              predicted_max = mean(predicted_max, na.rm = T), 
              predicted_min = mean(predicted_min, na.rm = T), 
              .groups = "drop") %>% 
    pivot_wider(names_from = variable, values_from = c(predicted_mean, predicted_max, predicted_min)) 
    # mutate(accumulated_precipitation_flux = zoo::rollsum(predicted_mean_precipitation_flux, k = 3, fill = NA, align = "right"))
  
  driver_vars <- lapply(c("predicted_mean", "predicted_max", "predicted_min"), 
                        function(x){paste(x, driver_vars, sep = "_")}) %>% unlist()
                   # "accumulated_precipitation_flux") 
   
  all_data <- left_join(target, drivers, by = "time") %>% 
    filter_at(driver_vars, all_vars(!is.na(.))) %>% 
    mutate(chla_lagged_1 = dplyr::lag(chla, n = 1)) %>% 
    slice(2:n())#  %>% 
    # rowwise() %>% 
    # mutate(cumulative_precip = )
  

  # model <- lm(chla ~ chla_lagged_1 + 
  #               air_temperature + 
  #               relative_humidity + 
  #               surface_downwelling_shortwave_flux_in_air + 
  #               precipitation_flux, 
  #             data = all_data) 
  # summary(model)
  model_data <- as_tibble(all_data) %>% 
    mutate(doy = lubridate::yday(time)) %>% 
    select(-c(time, site_id)) 
  
  model_data <- na.omit(model_data)
  
   
  tune = randomForest::tuneRF(x = select(model_data, -chla),
                              y = model_data$chla, 
                              stepFactor = 1.5,
                              improve = 0.01,
                              ntreeTry = 1000)
  
  model = randomForest::randomForest(chla ~ .,
                                     data = model_data,
                                     na.action = na.omit, 
                                     mtry = tune[which(min(tune[,2]) == tune[,2]),1], 
                                     importance = T, 
                                     ntree = 1000)
   
  # preds = predict(model, model_data, predict.all = T)
  # 
  # pred.rf.int2 <- sapply(1:length(preds$aggregate), function(i) {
  #   tmp <- preds$individual[i, ] + rnorm(1000, 0, sqrt(model$mse))
  #   quantile(tmp, c(0.025, 0.975), na.rm=T)
  # })
  
  saveRDS(model, out_file) 
  return(out_file) 
}



predict_chla <- function(
    trained_model,
    data,
    n_samples)
    # chla_lagged_1,
    # air_temp,
    # rh,
    # swrad,
    # precip)
{
  # chla = trained_model$coefficients[1] + 
  #   trained_model$coefficients[2]*chla_lagged_1 + 
  #   trained_model$coefficients[3]*air_temp + 
  #   trained_model$coefficients[4]*rh +
  #   trained_model$coefficients[5]*swrad + 
  #   trained_model$coefficients[6]*precip
   
  chla = predict(trained_model, data, predict.all = TRUE)
  
  # sampling trees 
  chla = rnorm(n_samples, 
               mean = mean(chla$individual, na.rm = T),
               sd = sd(chla$individual, na.rm = T)) 
  
  return(list(chla = chla))
}
