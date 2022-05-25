


train_model <- function(driver_file,
                        driver_vars,
                        target_file, 
                        target_vars,
                        site, 
                        out_file){
  browser() 
  target <- filter(target_file, siteID == site) %>% 
    select(time, siteID, !!target_vars)
  
  drivers <- purrr::map(driver_vars, nc_met_get, nc_file = driver_file) %>%
    purrr::reduce(left_join, by = c("time", "hour"))
  
  browser() 
  
  
}




