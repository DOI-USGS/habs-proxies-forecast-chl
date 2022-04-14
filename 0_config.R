# sourcing functions
# source("0_config/src/")


# target list
p0_targets_list = list(
  
  tar_target(
    p0_team_name, 
    "USGS_HABs_Proxies"
  ),
  
  tar_target(
    p0_team_list, 
    list(list(individualName = list(givenName = "Jacob", 
                                    surName = "Zwart"),
              organizationName = "U.S. Geological Survey",
              electronicMailAddress = "jzwart@usgs.gov"),
         list(individualName = list(givenName = "Philip", 
                                    surName = "Savoy"),
              organizationName = "U.S. Geological Survey",
              electronicMailAddress = "psavoy@usgs.gov"))
  ),
  
  # variables we want to forecast 
  tar_target(
    p0_target_var,
    "chla"
  ),
  
  # sites we're forecasting at 
  tar_target(
    p0_forecast_site_ids,
    c("COMO", "MCDI", "POSE")
  ),
  
  tar_target(
    p0_site_metadata,
    read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-aquatics/master/Aquatic_NEON_Field_Site_Metadata_20210928.csv") %>% 
      filter(field_site_id %in% p0_forecast_site_ids) 
  )
  
)