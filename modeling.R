# This script models daily incidence and mortality, hospitalizations and ICU patient counts data 
# based on the case, hospitalizations and ICU counts from previous days (infection effect, prolonged hospital stay), 
# daily test counts, vaccination and booster rate.
# The method of choice is Random Forest algorithm, which proved the most reliable in the previous blog post.
# The training data set includes the period from 2021-06-15 to 2021-12-20, dominated by the delta variant
# The test data set includes the period from 2021-12-21 till today

# tools ----

  library(doParallel)
  library(caret)

# container list ----

  mod <- list()
  
# modeling tables: the training and test sets ----
  
  mod$data <- cov_data$data %>% 
    dlply(.(location), function(x) dlply(x, .(split))) %>% 
    transpose %>% 
    map(~map(.x, as_tibble))
  
# train control object and modeling formula -----
  
  mod$control <- trainControl(method = 'cv', 
                              number = 10, 
                              seeds = c(map(1:10, function(x) rep(1234, 3)), 
                                        list(7)), 
                              returnResamp = 'final', 
                              savePredictions = 'final')

  mod$formula <- cov_data$responses %>% 
    map(~paste(.x, paste(cov_data$indep_vars , collapse = '+'), sep = '~')) %>% 
    map(as.formula)

# training the case, mortality, hospitalization and ICU models -----
  
  registerDoParallel(cores = 7)

  mod$train_models <- mod$formula %>% 
    map(~train_list(formula = .x, 
                    data_list = mod$data$train, 
                    method = 'rf', 
                    trControl = mod$control))

  stopImplicitCluster()
  
# getting the redistribution and CV residuals, mean absolute errors and cumulative errors -----
  
  mod$pred_train <- mod$train_models %>% 
    map(function(outcome) map(outcome, get_predictions) %>% 
          map2(., mod$data$train, ~mutate(.x, day = .y[['day']])))
  
  mod$error_train <- mod$pred_train %>% 
    map(function(outcome) map(outcome, get_errors) %>% 
          map2_dfr(., names(.), ~mutate(.x, location = .y)))

# getting the test set predictions and residuals, mean absolute errors and cumulative errors -----
  
  mod$pred_test <- mod$train_models %>% 
    map(function(outcome) list(caret_model = outcome, 
                               new_data = mod$data$test) %>% 
          pmap(get_predictions))

  mod$error_test <- mod$pred_test %>% 
    map(function(outcome) map(outcome, get_errors) %>% 
          map2_dfr(., names(.), ~mutate(.x, location = .y)))

# Common tables with the predictions, needed for the result visualization ----- 
  
  mod$preds <- map2(mod$pred_train, 
                    mod$pred_test, 
                    function(train_pred, test_pred) map2(train_pred, 
                                                         test_pred, 
                                                         outer_rbind))
  
  ## adding the exact date for plotting
  
  mod$preds <- mod$preds %>% 
    map(~map(.x, left_join, filter(cov_data$data[c('day', 'date')], !duplicated(day)), by = 'day'))
  
# Common tables with errors, needed for calculation of the outcome events saved -----

  mod$errors <- map2(mod$error_train, 
                     mod$error_test, 
                     left_join, by = 'location') %>% 
    map(mutate, 
        plot_label = paste0('Training:\nMAE = ', signif(mae_train, 2), 
                            '\nCumE = ', signif(cue_train, 2), 
                            '\n\nCV:\nMAE = ', signif(mae_cv, 2), 
                            '\n\nCumE = ', signif(cue_cv, 2), 
                            '\n\nTest:\nMAE = ', signif(mae_test, 2), 
                            '\nCumE = ', signif(cue_test, 2)))
  
# Importance measures ------
  
  mod$importance <- mod$train_models %>% 
    map(~map(.x, importance))
    
# END ----