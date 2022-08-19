
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
      mutate(time = time - (5 * 3600),
             time = as_date(time)) %>% 
      group_by(site_id, time, ensemble, variable) %>% 
      summarise(predicted = mean(predicted, na.rm = T), 
                .groups = "drop") 
  }else{
    summarized_data <- data %>% 
      # time offset is about 5 hours  
      mutate(time = time - (5 * 3600),
             time = as_date(time)) %>% 
      group_by(site_id, time, variable) %>% 
      summarise(predicted = mean(predicted, na.rm = T), 
                .groups = "drop") 
  }
  
  saveRDS(summarized_data, out_file)
  return(out_file)
}