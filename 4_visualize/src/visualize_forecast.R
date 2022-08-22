

visualize_forecast <- function(forecast_file, 
                               obs, 
                               out_file){
   
  obs <- filter(obs, 
                variable == "chla") %>% 
    pivot_wider(names_from = variable, values_from = observed) %>% 
    rename(obs_chla = chla)
  
  forecasts <- read_csv(forecast_file) %>% 
    left_join(obs, by = c("time", "site_id"))
  
  obs <- filter(obs, time >= min(forecasts$time), 
                time <= max(forecasts$time))
  
  show_all_predicted = TRUE 
  
  # Breaks by 5, then associated with the data for dynamic coloring of text/ticks
  breaks_all <- seq(0,100, by = 5)
  breaks_draw <- breaks_all[breaks_all >= min(forecasts$chla, na.rm = T)]
  # copied from https://github.com/USGS-VIZLAB/forecast-drb/blob/bf40ad748814298878c76eaa793cd1e68e2f62a4/3_visualize/src/plot_interval.R
  browser() 
  # plot 1-day out predictions with mean prediction
  plot <- forecasts %>%
    ggplot(
      aes(
        x = time,
        y = chla,
        group = site_id
      )) +
    labs(x = " ",
         y = "Chl-a (ug / L)") +
    # panel for each site
    facet_grid(~ site_id, scales = "free") + #{
      # Turn off or on the full background of predicted values depending on argument
    #   if(show_all_predicted == TRUE){
    #     stat_gradientinterval(shape = NA,
    #                           aes(fill = ifelse(stat(y) > c_to_f(threshold), NA, "none")),
    #                           size = 1)  }
    #   else {
    #     stat_gradientinterval(shape = NA,
    #                           fill = "white",
    #                           size = 1) }
    # } +
    stat_interval(.width = seq(0.1, 0.9, by = 0.1), #set CI levels
                  size = 2) +
    # gradient color scale, using red for NA (over threshold)
    scico::scale_fill_scico_d(palette = "lapaz",
                              end = 0.7,
                              na.value = "orangered",
                              direction = -1) +
    scico::scale_color_scico_d(palette = "lapaz",
                               end = 0.5,
                               na.value = "orangered",
                               direction = -1)+
    # change alpha so that end of confidence interval shows and doesn't fade away
    scale_slab_alpha_continuous(
      range = c(0.5, 1) #default: 0,1
    )+
    # tile for mean prediction
    geom_tile(fill = 'white',
              stat = "summary",
              fun = "mean",
              height = 0.1)+
    # threshold line
    # { if (max_temp >= 71) {
    #   geom_hline(yintercept = c_to_f(threshold),
    #              linetype = "dotted",
    #              color = "orangered",
    #              size = .48,
    #              alpha = 0.8)
    # }}+
    theme(legend.position = "none",
          axis.text = element_text(angle = 0, hjust = 0.5),
          axis.text.x = element_text(size = 5),
          axis.ticks.x = element_line(size = 0.3),
          strip.background = element_rect(color = NA, fill = NA),
          # color for axis labels
          axis.text.y = element_text(size = 6,
                                     color = ifelse(breaks_draw == 75, "red", "black")),
          axis.ticks.y = element_line(color = ifelse(breaks_draw == 75, "red", "black"), size = 0.3),
          panel.background = element_rect(color="grey", fill = NA),
          axis.line = element_line(size = .5, color="gray"),
          strip.text = element_text(face = "bold"),
          # panel.grid left white marks over facet borders, removed with line below
          panel.grid = element_blank(),
          axis.title.y = element_text(size = 8),
          panel.spacing = unit(0,"lines"))+
    scale_x_date(breaks = scales::breaks_width("5 day"),
                 labels = scales::label_date_short()) + 
    geom_point(data = obs, aes(x = time, y = obs_chla), color = "red") 


  ggsave(filename = out_file, plot = plot, bg = "white", 
         width = 10, height = 6)
} 


