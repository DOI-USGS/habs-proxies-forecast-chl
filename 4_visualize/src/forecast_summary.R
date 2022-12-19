

library(tidyverse)
library(targets)

# files <- list.files("3_forecast/out", 
#                     pattern = "^aquatics.*csv$",
#                     full.names = T)
# 
# forecasts <- purrr::map(files, read_csv) %>% bind_rows() 
# 
# summary <- forecasts %>% 
#   group_by(reference_datetime, datetime, site_id) %>% 
#   summarise(mean_pred = mean(prediction, na.rm = T), .groups = "drop")
# 
# saveRDS(object = summary, file = "4_visualize/out/forecast_summary.rds") 

summary <- readRDS("4_visualize/out/forecast_summary.rds") 

observations <- tar_read(p1_aquatic_targets) %>% 
  filter(variable == "chla")

rmse <- left_join(summary, observations, by = c("datetime", "site_id")) %>% 
  mutate(lead_time = datetime - reference_datetime) %>% 
  group_by(site_id, lead_time) %>% 
  summarise(rmse = sqrt(mean((mean_pred - observation)^2, na.rm = T)),
            rmse_scaled = rmse / mean(observation, na.rm = T),
            .groups = "drop")

rmse_plot <- ggplot(filter(rmse, lead_time >0)) + 
  geom_line(aes(x = as.numeric(lead_time), y = rmse),
            size = 2) + 
  facet_wrap(~site_id, scales = "free") + 
  theme_minimal() + 
  ylab(expression(RMSE~(mu*g/L))) + 
  xlab("Lead Time (days)") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        strip.text = element_text(size = 16))

ggsave(filename = "4_visualize/out/rmse_leadtime.png", plot = rmse_plot,
       width = 10, height = 6, units = "in")

# score_df <- neon4cast::combined_scores(theme = "aquatics")

# saveRDS(object = score_df, file = "4_visualize/out/all_scores.rds")
score_df <- readRDS("4_visualize/out/all_scores.rds")

summary_scores <- score_df %>% 
  filter(variable == "chla") %>% 
  mutate(lead_time = as.Date(datetime) - as.Date(reference_datetime)) %>% 
  group_by(lead_time, site_id, model_id) %>% 
  summarise(crps = mean(crps, na.rm = T),
            .groups = "drop") %>% 
  filter(site_id %in% c("BLWA", "FLNT", "TOMB"))

ggplot(filter(summary_scores, lead_time >0)) + 
  geom_line(aes(x = as.numeric(lead_time), y = crps,
                group = model_id, color = model_id),
            size = 2) + 
  facet_wrap(~site_id, scales = "free") + 
  theme_minimal() + 
  ylab(expression(CRPS~(mu*g/L))) + 
  xlab("Lead Time (days)") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18), 
        strip.text = element_text(size = 16)) + 
  ylim(c(0,10)) + xlim(c(0,30))

timeseries <- score_df %>% 
  filter(model_id == "USGSHABs1") %>% 
  mutate(datetime = as.Date(datetime),
         lead_time = datetime - as.Date(reference_datetime)) 

timeseries_plot <- 3
ggplot(filter(timeseries, lead_time == 1),
       aes(x = datetime, y = mean)) + 
  geom_ribbon(aes(ymin = quantile10, ymax = quantile90),
              alpha = .6, fill = 'grey60') + 
  geom_line(color = "white") + 
  geom_point(aes(x = datetime, y = observation)) + 
  facet_wrap(~site_id, scales = "free") + 
  theme_bw()+ 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
