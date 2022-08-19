
#' 
#' author: Phil Savoy, Jake Zwart 
#' 
download_nwm <- function(sites,
                         start_date = NULL,
                         end_date = NULL,
                         out_file){
  map_df <- sites %>% 
    mutate(comid = COMID, 
           startDate = start_date,
           endDate = end_date,
           version = 2.1) %>% 
    select(comid, startDate, endDate, tz, version) 
  browser() 
  #Download data. For simplicity, download  and let readNWMdata handle
  #conversion to local time.
  nwm_data <- purrr::pmap(map_df, nwmTools::readNWMdata()) %>% unlist() 
  #   nwmTools::readNWMdata(
  #   comid = sites$COMID,
  #   startDate = start_date,
  #   endDate = end_date, 
  #   tz = sites$tz,
  #   version = 2.1
  # )   
  
  return(out_file)
}

