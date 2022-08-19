
#' Define a function to return the corresponding comid
#' author: Phil Savoy, Jake Zwart 
#' 
comid_from_point <- function(lat, lon, CRS = 4326) {
  point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = CRS)
  COMID <- nhdplusTools::discover_nhdplus_id(point)
  if(! length(COMID)) COMID = NA
  return(COMID)
}


get_tz <- function(lat, lon){
   
  #Import the tz_world dataset if it is not already loaded
  if(!exists("tz_world")){data("tz_world", package = "StreamLightUtils")}
  data <- tibble(lat = lat, lon = lon) 
  #Project (These sites all are in WGS84)
  sites_WGS84 <- sf::st_as_sf(
    data, 
    coords = c("lon", "lat"),
    crs = 4326
  )    
  
  #Return the timezone name
  tz_join <- sf::st_join(
    sites_WGS84, 
    tz_world,
    join = sf::st_nearest_feature
  )
  
  #Drop geometry
  drop <- sf::st_drop_geometry(tz_join)
  
  #Remove the factor
  tz_name <- as.character(drop$TZID)
  
  return(tz_name) 
}
