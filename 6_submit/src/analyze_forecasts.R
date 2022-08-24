
library(tidyverse)
library(targets)
library(scoringRules)

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
    filter(!is.na(chla)) %>% 
    group_by(time, site_id) %>% 
    left_join(obs, by = c("time", "site_id")) %>% 
    summarise(crps = ifelse(all((is.na(obs_chla) | is.na(chla))),
                            NA,
                            scoringRules::crps_sample(y = unique(obs_chla), dat = matrix(chla, nrow = 1))),
              mean = mean(chla, na.rm=T),
              obs_chla = mean(obs_chla, na.rm = T), .groups = "drop") %>% 
    mutate(issue_time = min(time),
           lead_time = time - issue_time) %>% 
    group_by(site_id, lead_time, issue_time) %>% 
    summarise(rmse = sqrt(mean((obs_chla - mean)^2, na.rm = T)),
              crps = crps, 
              mean = mean,
              obs_chla = obs_chla,
              .groups = "drop") 
  
  out <- bind_rows(out, cur)
}

out2 <- out %>% 
  group_by(site_id, lead_time) %>% 
  summarise(rmse = mean(rmse, na.rm = T),
            crps = mean(crps, na.rm = T), 
            .groups = "drop") 

time_series <- out %>% 
  mutate(time = issue_time + lead_time)

ggplot(out2) + 
  geom_line(aes(x = as.numeric(lead_time), y = crps)) + 
  geom_point(aes(x = as.numeric(lead_time), y = crps)) + 
  facet_wrap(~site_id) + 
  theme_bw() + 
  ylab("CRPS chla (ug/L)") + xlab("Forecast Lead Time (days)")


filter(time_series, lead_time == 34) %>% 
  ggplot() + 
  geom_line(aes(x = time, y = mean), size = 1.4) + 
  geom_point(aes(x = time, y = obs_chla), color = "red") + 
  facet_wrap(~site_id) + 
  theme_bw() + 
  ylab("chla (ug/L)") + xlab("Date") + 
  ggtitle("34 day lead time")


