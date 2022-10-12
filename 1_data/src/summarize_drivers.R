
#' summarizes drivers to daily averages 
#' 
summarize_drivers <- function(
    in_file,
    vars, 
    group_by_ens, 
    out_file)
{
   
  data <- readRDS(in_file) 
  
  if(group_by_ens){
    summarized_data <- data %>% 
      # time offset is about 5 hours  
      mutate(datetime = datetime - (5 * 3600),
             datetime = as_date(datetime)) %>% 
      group_by(site_id, datetime, parameter, family, variable) %>% 
      summarise(predicted_mean = mean(prediction, na.rm = T), 
                predicted_max = max(prediction, na.rm = T), 
                predicted_min = min(prediction, na.rm = T), 
                .groups = "drop") 
  }else{
    summarized_data <- data %>% 
      # time offset is about 5 hours  
      mutate(datetime = datetime - (5 * 3600),
             datetime = as_date(datetime)) %>% 
      group_by(site_id, datetime, variable) %>% 
      summarise(predicted_mean = mean(prediction, na.rm = T), 
                predicted_max = max(prediction, na.rm = T), 
                predicted_min = min(prediction, na.rm = T), 
                .groups = "drop") 
  }
  
  saveRDS(summarized_data, out_file)
  return(out_file)
}