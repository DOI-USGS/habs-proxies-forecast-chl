# sourcing functions
# source("0_config/src/")

# see https://github.com/eco4cast/neon4cast-example for example forecast 
# and how to run in github actions 
 
# target list
p0_targets_list = list(
  
  tar_target(
    p0_team_name, 
    "USGSHABs1"
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
              electronicMailAddress = "psavoy@usgs.gov"),
         list(individualName = list(givenName = "Noah", 
                                    surName = "Schmadel"),
              organizationName = "U.S. Geological Survey",
              electronicMailAddress = "nschmadel@usgs.gov"),
         list(individualName = list(givenName = "Lisa", 
                                    surName = "Lucas"),
              organizationName = "U.S. Geological Survey",
              electronicMailAddress = "llucas@usgs.gov"), 
         list(individualName = list(givenName = "Judson", 
                                    surName = "Harvey"),
              organizationName = "U.S. Geological Survey",
              electronicMailAddress = "jwharvey@usgs.gov"),
         list(individualName = list(givenName = "Jennifer", 
                                    surName = "Murphy"),
              organizationName = "U.S. Geological Survey",
              electronicMailAddress = "jmurphy@usgs.gov"))
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