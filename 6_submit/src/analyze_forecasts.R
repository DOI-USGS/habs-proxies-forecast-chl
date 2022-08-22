
library(tidyverse)
library(targets)

forecast_dir <- "3_forecast/out" 

files <- list.files(forecast_dir)

files <- files[grepl("aquatics-", files) & grepl(".csv", files)]

obs <- read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") %>% 
  dplyr::filter(site_id %in% c("BLWA", "FLNT", "TOMB"), variable == "chla") %>% 
  pivot_wider(names_from = variable, values_from = observed) %>% 
  rename(obs_chla = chla)

out <- tibble() 
for(file in files){
  cur <- read_csv(file.path(forecast_dir, file)) %>% 
    group_by(time, site_id) %>% 
    summarise(mean = mean(chla, na.rm=T), .groups = "drop") %>% 
    mutate(issue_time = min(time),
           lead_time = time - issue_time) %>% 
    left_join(obs, by = c("time", "site_id")) %>% 
    group_by(site_id, lead_time) %>% 
    summarise(rmse = sqrt(mean((obs_chla - mean)^2, na.rm = T)),
              .groups = "drop") 
  
  out <- bind_rows(out, cur)
}

out2 <- out %>% 
  group_by(site_id, lead_time) %>% 
  summarise(rmse = mean(rmse, na.rm = T), .groups = "drop") 


ggplot(out2) + 
  geom_line(aes(x = as.numeric(lead_time), y = rmse)) + 
  geom_point(aes(x = as.numeric(lead_time), y = rmse)) + 
  facet_wrap(~site_id) + 
  theme_bw() + 
  ylab("RMSE chla (ug/L)") + xlab("Forecast Lead Time (days)")
