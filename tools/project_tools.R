# Functions for the blog post project

# tools ----

  require(plyr)
  require(tidyverse)
  require(rlang)
  require(forecast)
  require(ggrepel)
  require(cowplot)

  map <- purrr::map

# globals -----

  globals <- list()

  globals$common_text <- element_text(size = 8, face = 'plain', color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold', 
                                                                            color = 'black', 
                                                                            hjust = 0), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', color = 'gray80'), 
                                                  plot.margin = globals$common_margin, 
                                                  panel.grid.major = element_line(color = 'gray90'))
  
# functions ----

  add_lag <- function(vector, lag = 1) {
    
    ## adds a lag to a numeric vector
    
    stopifnot(is.numeric(vector))
    
    lags <- length(vector):(length(vector) - lag)
    
    c(rep(NA, lag + 1), vector[-lags])
    
  }
  
  add_lag_var <- function(data, src_variable, lags, complete = TRUE) {
    
    ## adds lag variables to the table
    ## can filter out incomplete scr_variable and the derived lag variables
    
    stopifnot(is.integer(lags))

    lag_names <- paste(src_variable, 'lag', lags, sep = '_')
    
    lag_tbl <- lags %>% 
      map(add_lag, 
              vector = data[[src_variable]]) %>% 
      set_names(lag_names)
    
    new_tbl <- cbind(data, lag_tbl) %>% 
      as_tibble
    
    if(complete) {
      
      for(i in lag_names) {
        
        new_tbl <- new_tbl %>% 
          filter(!is.na(.data[[i]]))
        
      }

    }
    
    new_tbl
    
  }
  
  fill_missing <- function(data, variable) {
    
    ## fills the missing values with the last non-missing value
    
    last_non <- data[[variable]][!is.na(data[[variable]])]
    
    last_non <- last_non[length(last_non)]
    
    data %>% 
      mutate(!!ensym(variable) := ifelse(is.na(.data[[variable]]), last_non, .data[[variable]]))
    
  }
  
  outer_rbind <- function(tbl1, tbl2) {
    
    ## binds two data frames by rows, missing variables are filled with NA
    
    ## missing variables
    
    miss1 <- names(tbl2)[!names(tbl2) %in% names(tbl1)]
    miss2 <- names(tbl1)[!names(tbl1) %in% names(tbl2)]
    
    ## filling the tables
    
    for(i in miss1){
      
      tbl1 <- tbl1 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    for(i in miss2){
      
      tbl2 <- tbl2 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    return(rbind(tbl1, tbl2))
    
  }
  
  train_list <- function(formula, data_list, ...) {
    
    ## trains a list of caret models
    
    caret_list <- data_list %>% 
      map(function(x) train(form = formula, 
                            data = x, ...))
    
    for(i in names(caret_list)) {
      
      caret_list[[i]]$formula <- as.formula(expr(!!formula))
      
    }
    
    caret_list

  }
  
  get_predictions <- function(caret_model, new_data = NULL) {
    
    ## calculates the model predictions and residuals
    
    predictions <- predict(caret_model, newdata = new_data)
    
    if(is.null(new_data)) {
      
      tibble(outcome = caret_model$trainingData$.outcome, 
             predicted_train = predictions, 
             predicted_cv = caret_model$pred %>% 
               arrange(rowIndex) %>% 
               .$pred) %>% 
        mutate(resid_train = predicted_train - outcome, 
               resid_cv = predicted_cv - outcome)
      
    } else {
      
      mod_response <- as.character(caret_model$formula)[2]
      
      tibble(day = new_data[['day']], 
             outcome = new_data[[mod_response]], 
             predicted_test = predictions) %>% 
        mutate(resid_test = predicted_test - outcome)
      
    }
    
  }
  
  get_errors <- function(prediction_table) {
    
    ## calculated mean absolute and cumulative error
    
    if('resid_train' %in% names(prediction_table)) {
      
      tibble(mae_train = mean(abs(prediction_table$resid_train), na.rm = TRUE), 
             mae_cv = mean(abs(prediction_table$resid_cv), na.rm = TRUE), 
             cue_train = sum(prediction_table$resid_train, na.rm = TRUE), 
             cue_cv = sum(prediction_table$resid_cv, na.rm = TRUE))
      
    } else {
      
      tibble(mae_test = mean(abs(prediction_table$resid_test), na.rm = TRUE), 
             cue_test = sum(prediction_table$resid_test, na.rm = TRUE))
      
    }

  }
  
  importance <- function(caret_model) {
    
    ## extracts RF variable importance measure obtained by noising
    
    caret_model$finalModel$importance %>% 
      as.data.frame %>% 
      rownames_to_column('variable') %>% 
      as_tibble
    
  }
  
  plot_predictions <- function(data, 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               plot_tag = NULL, 
                               y_lab = 'Outcome', 
                               fill_color = 'bisque2', 
                               bar_alpha = 0.75) {
    
    ## plots the predictions in the training, CV and test data sets
    ## the true outcome is presented as bars
    ## data: the common table with predictions for the training, CV and test data sets
    
    pred_tbl <- list(train = data[c('date', 'predicted_train')], 
                     cv = data[c('date', 'predicted_cv')],
                     test = data[c('date', 'predicted_test')]) %>% 
      map(set_names, 
          c('date', 'prediction')) %>% 
      map2_dfr(., names(.), ~mutate(.x, split = .y)) %>% 
      mutate(split = factor(split, c('train', 'cv', 'test')))
    
    ## plotting
    
    data %>% 
      ggplot(aes(x = date, 
                 y = outcome)) + 
      geom_bar(stat = 'identity', 
               fill = fill_color, 
               alpha = bar_alpha) + 
      geom_line(data = pred_tbl, 
                aes(color = split, 
                    y = prediction), 
                size = 0.75) + 
      scale_color_manual(values = c('train' = 'steelblue', 
                                    'cv' = 'gray60', 
                                    'test' = 'firebrick'), 
                         labels = c('train' = 'Training/Delta', 
                                    'cv' = 'CV/Delta', 
                                    'test' = 'Test/Omicron'), 
                         name = '') + 
      globals$common_theme + 
      labs(title = plot_title,
           subtitle = plot_subtitle,
           tag = plot_tag, 
           x = 'Date, 2021/22',
           y = y_lab)
    
  }
  
  plot_resids <- function(data, 
                          plot_title = NULL, 
                          plot_subtitle = NULL, 
                          plot_tag = NULL, 
                          y_lab = 'Residuals: Outcome', 
                          bar_alpha = 0.75) {
    
    ## plots the predictions in the training, CV and test data sets
    ## the true outcome is presented as bars
    ## data: the common table with predictions for the training, CV and test data sets
    
    resid_tbl <- list(train = data[c('date', 'resid_train')], 
                      test = data[c('date', 'resid_test')]) %>% 
      map(set_names, 
          c('date', 'resid')) %>% 
      map2_dfr(., names(.), ~mutate(.x, split = .y)) %>% 
      mutate(split = factor(split, c('train', 'test')))
    
    ## plotting
    
    resid_tbl %>% 
      ggplot(aes(x = date, 
                 y = resid, 
                 fill = split)) + 
      geom_bar(stat = 'identity', 
               alpha = bar_alpha) + 
      scale_fill_manual(values = c('train' = 'steelblue', 
                                   'test' = 'firebrick'), 
                        labels = c('train' = 'Training/Delta', 
                                   'test' = 'Test/Omicron'), 
                        name = '') + 
      globals$common_theme + 
      labs(title = plot_title,
           subtitle = plot_subtitle,
           tag = plot_tag, 
           x = 'Date, 2021/22',
           y = y_lab)
    
  }
  
  plot_errors <- function(data, 
                          err_var = 'mae', 
                          x_lab = 'MAE', 
                          plot_title = NULL, 
                          plot_subtitle = NULL, 
                          plot_tag = NULL) {
    
    err_tbl <- data %>% 
      gather(key = 'split', 
             value = 'error', 
             starts_with(err_var)) %>% 
      mutate(split = stri_replace(split, fixed = paste0(err_var, '_'), replacement = ''), 
             split = factor(split, c('train', 'cv', 'test')))
    
    err_tbl %>% 
      ggplot(aes(x = .data[['error']], 
                 y = location, 
                 fill = split)) + 
      geom_bar(stat = 'identity',
               color = 'black', 
               position = position_dodge(0.9)) + 
      scale_fill_manual(values = c('train' = 'steelblue', 
                                   'cv' = 'gray70', 
                                   'test' = 'firebrick'), 
                        labels = c('train' = 'Training/Delta', 
                                   'cv' = 'CV/Delta', 
                                   'test' = 'Test/Omicron'), 
                        name = '') + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab)
    
    
  }
  
  plot_importance <- function(importance_list, 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              plot_tag = NULL, 
                              n_top = 5, 
                              fill_color = 'bisque2') {
    
    plotting_tbl <- importance_list %>% 
      map(mutate, 
          importance = scale(IncNodePurity)[, 1]) %>% 
      map(top_n, 5, IncNodePurity) %>% 
      map2_dfr(., names(.), ~mutate(.x, location = .y)) %>% 
      mutate(variable = stri_replace(variable, fixed = '_per_', replacement = '/'), 
             variable = stri_replace_all(variable, fixed = '_', replacement = ' '), 
             variable = stri_replace(variable, fixed = 'lag ', replacement = 'L'), 
             variable = stri_replace(variable, fixed = 'icu', replacement = 'ICU')) %>% 
      arrange(location, importance) %>% 
      mutate(plot_order = 1:nrow(.))
    
    plotting_list <- plotting_tbl %>% 
      dlply(.(location)) %>% 
      map(function(country) country %>% 
            ggplot(aes(x = importance, 
                       y = reorder(variable, plot_order))) + 
            geom_bar(stat = 'identity', 
                     fill = fill_color, 
                     color = 'black') + 
            facet_grid(location ~ ., 
                       scales = 'free', 
                       space = 'free') + 
            globals$common_theme + 
            theme(axis.title = element_blank()))
    
    plotting_list[[1]] <- plotting_list[[1]] + 
      labs(title = plot_title, 
           subtitle = plot_subtitle)
    
    plotting_list[[length(plotting_list)]] <- plotting_list[[length(plotting_list)]] + 
      theme(axis.title.x = globals$common_text) + 
      labs(x = 'Normalized importance')
    
    plot_grid(plotlist = plotting_list, 
              ncol = 1, 
              align = 'hv')
    
  }
  
# END ----
