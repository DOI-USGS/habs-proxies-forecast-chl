

visualize_forecast <- function(forecast_file, 
                               obs, 
                               out_file){
   
  obs <- filter(obs, variable == "chla") %>% 
    pivot_wider(names_from = variable, values_from = observed) %>% 
    rename(obs_chla = chla)
  
  forecasts <- read_csv(forecast_file) %>% 
    left_join(obs, by = c("time", "site_id"))
  
  plot <- ggplot(data = forecasts) + 
    geom_line(aes(x = time, y = chla, group = ensemble),
              size = 1) + 
    geom_point(aes(x = time, y = obs_chla), color = "red") + 
    facet_wrap(~site_id, scale = "free") + 
    theme_minimal() + 
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), 
          strip.text = element_text(size = 16)) + 
    ylab("Chl-a (ug / L)") + xlab("")
  
  ggsave(filename = out_file, plot = plot, 
         width = 10, height = 6)
  
} 