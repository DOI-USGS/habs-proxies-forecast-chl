

visualize_forecast <- function(forecast_file, 
                               out_file){
  
  forecasts <- read_csv(forecast_file) 
  
  plot <- ggplot(data = forecasts) + 
    geom_line(aes(x = time, y = chla, group = ensemble),
              size = 1) + 
    facet_wrap(~siteID, scale = "free") + 
    theme_minimal() + 
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16), 
          strip.text = element_text(size = 16)) + 
    ylab("Chl-a (ug / L)") + xlab("")
  
  ggsave(filename = out_file, plot = plot, 
         width = 10, height = 6)
  
} 