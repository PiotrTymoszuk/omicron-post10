# This script generates specific plots with the summary modeling stats

# container list -----

  plots <- list()
  
# some plotting globals ----
  
  plots$outcome_labs <- c('cases' = 'New cases', 
                          'fatal' = 'Deaths', 
                          'icu' = 'ICU patients', 
                          'hosp' = 'Hospitalizations')
  
  plots$outcome_colors <- c('cases' = 'bisque2', 
                            'fatal' = 'lightpink2', 
                            'icu' = 'darkseagreen3', 
                            'hosp' = 'burlywood2')
  
  plots$outcome_ylabs <- c('cases' = 'new cases/million', 
                           'fatal' = 'deaths/million', 
                           'icu' = 'ICU patients/million', 
                           'hosp' = 'hospitalizations/million')
  
# Plots of cases and fatalities for single countries ------

  plots$country_preds <- list(outcome = mod$preds, 
                              outcome_title = plots$outcome_labs, 
                              outcome_color = plots$outcome_colors, 
                              outcome_y = plots$outcome_ylabs) %>% 
    pmap(function(outcome, 
                  outcome_title, 
                  outcome_color, 
                  outcome_y) list(data = outcome, 
                                  plot_title = outcome_title, 
                                  plot_subtitle = names(outcome), 
                                  fill_color = outcome_color, 
                                  y_lab = outcome_y) %>% 
           pmap(plot_predictions, 
                bar_alpha = 1))

# Plotting residuals for each country -----
  
  plots$country_resids <- list(outcome = mod$preds, 
                               outcome_title = plots$outcome_labs, 
                               outcome_y = plots$outcome_ylabs) %>% 
    pmap(function(outcome, 
                  outcome_title, 
                  outcome_y) list(data = outcome, 
                                  plot_title = outcome_title, 
                                  plot_subtitle = names(outcome), 
                                  y_lab = paste('Residuals:', outcome_y)) %>% 
           pmap(plot_resids, 
                bar_alpha = 1))
  
# Plotting the errors ------
  
  plots$error_mae <- list(data = mod$errors, 
                          plot_title = plots$outcome_labs, 
                          x_lab = paste('MAE,', plots$outcome_ylabs)) %>% 
    pmap(plot_errors, 
         err_var = 'mae')

  plots$error_cue <- list(data = mod$errors, 
                          plot_title = plots$outcome_labs, 
                          x_lab = paste('CumE,', plots$outcome_ylabs)) %>% 
    pmap(plot_errors, 
         err_var = 'cue')
  
# Plotting the importance -----

  plots$importance <- list(importance_list = mod$importance, 
                           plot_title = plots$outcome_labs , 
                           fill_color = plots$outcome_colors) %>% 
    pmap(plot_importance, 
         plot_subtitle = 'Top 5 most important vars')

# END ----