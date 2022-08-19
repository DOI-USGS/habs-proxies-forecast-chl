#================================================================
#Download and process national water model data for NEON forecasting challenge sites
#Created 6/7/2022
#================================================================
library("dplyr")

# #Requires the StreamLightUtils package (https://github.com/psavoy/StreamLightUtils)
# #Use the devtools packge to install StreamLightUtils
#   install.packages("devtools")
#   devtools::install_github("psavoy/StreamLightUtils")

#-------------------------------------------------
#Pull in NEON data and get site information for the three streams in the challenge
#-------------------------------------------------
  #Get NEON site information (this is the current link as of 6/7/2022 but may change...)
    neon_info <- readr::read_csv(
      "https://www.neonscience.org/sites/default/files/NEON_Field_Site_Metadata_20220412.csv"
    ) %>%
      as.data.frame()
  
  #Read in NEON data from the forecasting challenge
    neon_data <- readr::read_csv(
      "https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz"
    ) %>%
      as.data.frame()
    
  #Subset the neon site info for only streams and rivers  
    streams_rivers <- neon_info %>%
      filter(field_site_subtype %in% c("Wadeable Stream", "Non-wadeable River")) 

  #Find site ids for stream and river sites in the challenge
    challenge_ids <- intersect(
      unique(neon_data[, "siteID"]),
      streams_rivers[, "field_site_id"]
    )  
      
  #Get subset of site information for just the stream challenge sites
    challenge_sites <- neon_info %>%
      filter(field_site_id %in% challenge_ids)
    
#-------------------------------------------------
#Add reach comids
#-------------------------------------------------
  #Define a function to return the corresponding comid
    comid_from_point <- function(lat, lon, CRS) {
      point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = CRS)
      COMID <- nhdplusTools::discover_nhdplus_id(point)
      if(! length(COMID)) COMID = NA
      return(COMID)
    } #End comid_from_point function
    
  #Apply the funciton to get comids
    challenge_sites$comid <- mapply(
      comid_from_point,
      lat = challenge_sites[, "field_latitude"],
      lon = challenge_sites[, "field_longitude"],
      CRS = 4326
    )
    
#-------------------------------------------------   
#Get timezone information
#------------------------------------------------- 
  #Import the tz_world dataset if it is not already loaded
    if(!exists("tz_world")){data("tz_world", package = "StreamLightUtils")}
    
  #Project (These sites all are in WGS84)
    sites_WGS84 <- sf::st_as_sf(
      challenge_sites, 
      coords = c("field_longitude", "field_latitude"),
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
    drop$tz_name <- as.character(drop$TZID)

  #Get final columns
    timezones <- drop[, c("field_site_id", "tz_name")]    
    
  #Merge timezone information with the site info
    timezone_merged <- merge(
      challenge_sites,
      timezones,
      by = "field_site_id"
    )
    
#-------------------------------------------------   
#Download national water model data using the nwmTools package    
#-------------------------------------------------   
  #Define a simple wrapper to download national water model data for each comid
    nwm_downloader <- function(comid, site_info){
      #Subset site information
        info_sub <- site_info[site_info[, "comid"] == comid, ]
        
      #Download data. For simplicity, download all data and let readNWMdata handle
      #conversion to local time.
        nwm_data <- nwmTools::readNWMdata(
          comid = info_sub[, "comid"],
          tz = info_sub[, "tz_name"],
          version = 2.1
        )    
    
      return(nwm_data)
        
    } #end nwm_downloader function
  
  #Apply the function to download data
    nwm_downloaded <- lapply(
      timezone_merged[, "comid"],
      FUN = nwm_downloader,
      site_info = timezone_merged
    )  
   
    names(nwm_downloaded) <- timezone_merged[, "comid"]
    
#------------------------------------------------- 
#Calculate mean daily discharge   
#------------------------------------------------- 
  #Define a function to aggregate nwm discharge to daily timesteps
    format_nwm <- function(ts){
      #If the timeseries isn't null, aggregate to daily values
      if(!is.null(ts)){
        #Catch to remove negative values just in case (I think there are none)
          ts[ts[, "flow_cms_v2.1"] < 0 & !is.na(ts[, "flow_cms_v2.1"]), "flow_cms_v2.1"] <- NA        
        
        #Add date information and calculate daily values
          ts$date <- as.Date(
            stringr::str_sub(paste(ts[, "dateTime"]), 1, -10),
            format = "%Y-%m-%d"
          )

        #Calculate the daily mean value
          ts_mean <- aggregate(
            ts[, "flow_cms_v2.1"] ~ date, 
            data = ts, 
            FUN = mean, 
            na.rm = TRUE, 
            na.action = NULL
          )    
          
          names(ts_mean)[2] <- "nwm_discharge"
          
        #Get the final timeseries to return  
          final <- ts_mean %>%
            mutate(comid = unique(ts[, "comid"])) %>%
            select(comid, date, nwm_discharge)

      } else{
        final <- NULL
      } #end if else statement 
      
      return(final)
  
    } #End format_nwm function    
    
  #Apply the function to calculate daily discharge
    nwm_daily <- lapply(
      nwm_downloaded, 
      FUN = format_nwm
    )
    
  #Lazy way to assign the NEON site id to the timeseries in the list
    names(nwm_daily) <- sapply(
      names(nwm_daily),
      FUN = function(x){return(challenge_sites %>% filter(comid == x) %>% pull(field_site_id))}
    )
        

  