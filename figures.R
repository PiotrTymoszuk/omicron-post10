# This script stitches together the figures

# tools -----

  library(figur) ## available at https://github.com/PiotrTymoszuk/figur

# container list ----

  figs <- list()
  
# Figure 1: predictions for particular countries -----
  
  figs[paste0(names(plots$country_preds$cases), '_preds')] <- plots$country_preds %>% 
    transpose %>% 
    map(~.x[c('cases', 'hosp', 'icu', 'fatal')]) %>% 
    map(~map(.x, ~.x + theme(legend.position = 'none'))) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv', 
                   labels = LETTERS, 
                   label_size = 10) %>% 
          plot_grid(get_legend(plots$country_preds$cases[[1]] + 
                                 theme(legend.position = 'bottom')), 
                    nrow = 2, 
                    rel_heights = c(0.9, 0.1))) %>% 
    map2(., paste0(names(plots$country_preds$cases), '_preds'), 
         ~as_figure(.x, .y, w = 180, h = 180, unit = 'mm'))

# Figure 2: residuals for particular countries ----
  
  figs[paste0(names(plots$country_preds$cases), '_resids')] <- plots$country_resids %>% 
    transpose %>% 
    map(~.x[c('cases', 'hosp', 'icu', 'fatal')]) %>% 
    map(~map(.x, ~.x + theme(legend.position = 'none'))) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv', 
                   labels = LETTERS, 
                   label_size = 10) %>% 
          plot_grid(get_legend(plots$country_resids$cases[[1]] + 
                                 theme(legend.position = 'bottom')), 
                    nrow = 2, 
                    rel_heights = c(0.9, 0.1))) %>% 
    map2(., paste0(names(plots$country_preds$cases), '_resids'), 
         ~as_figure(.x, .y, w = 180, h = 180, unit = 'mm'))
  
# Figure 3 and 4: mean absolute and cumulative errors -----
  
  figs$mae <- plots$error_mae[c('cases', 'hosp', 'icu', 'fatal')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(get_legend(plots$error_mae[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'mae_summary', w = 180, h = 180)
  
  
  figs$cue <- plots$error_cue[c('cases', 'hosp', 'icu', 'fatal')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    plot_grid(get_legend(plots$error_mae[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'cue_summary', w = 180, h = 180)
  
# Figure 5: the most important factors for new cases and hospitalizations -----
  
  figs$importance <- plots$importance[c('cases', 'hosp')] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'importance', w = 180, h = 180)
  
# Saving the figures on the disc ----
  
  figs %>%
    walk(save_figure, 
         path = './figures', 
         format = 'png')
  
# END -----