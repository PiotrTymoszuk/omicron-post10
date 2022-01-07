# This script imports data on CoV cases, hospitalizations, admissions and vaccinations from owid

# tools -----

  library(plyr)
  library(tidyverse)
  library(stringi)

  source('./tools/project_tools.R')

# data container -----

  cov_data <- list()
  
# reading the raw OWID dataset ----
  
  ## unfortunately the faster and nicer read_csv crashes while reading the recent owid dataset
  
  cov_data$raw <- try(read.csv(file = 'https://covid.ourworldindata.org/data/owid-covid-data.csv'), silent = TRUE)
  
  if(any(class(cov_data$raw) == 'try-error')){
    
    warning('Data server access denied, retrieving the data locally', call. = FALSE)
    message('Data server access denied, retrieving the data locally')
    
    cov_data$raw <- read.csv(file = './data/owid-covid-data.csv')
    
  } 
  
# clearing ----
  
  ## selecting what I need for UK, Denemark, France and Italy, other interesting variables like booster vaccinations
  ## are unfortunately too messy!
  
  cov_data$data <- cov_data$raw %>% 
    as_tibble %>% 
    mutate(date = as.Date(date)) %>% 
    filter(location %in% c('Denmark', 'Italy', 'France', 'United Kingdom'), 
           date > as.Date('2021-06-14')) %>% 
    select(iso_code, 
           location, 
           date, 
           total_cases_per_million, 
           new_cases_per_million, 
           new_deaths_per_million, 
           icu_patients_per_million, 
           hosp_patients_per_million, 
           new_tests_per_thousand, 
           people_fully_vaccinated_per_hundred, 
           total_boosters_per_hundred, 
           stringency_index)

  ## new cases, icu and hospitalized patients are dependent on the cases registered on former days - up to 21 days
  ## adding such lag variables
  ## removing the incomplete observations per country
  
  cov_data$data <- cov_data$data %>% 
    group_by(location) %>% 
    add_lag_var(src_variable = 'new_cases_per_million', 
                lags = 1:21) %>% 
    add_lag_var(src_variable = 'hosp_patients_per_million', 
                lags = 1:21) %>% 
    add_lag_var(src_variable = 'icu_patients_per_million', 
                lags = 1:21) %>% 
    group_by(location) %>% 
    filter(!is.na(total_cases_per_million), 
           !is.na(new_cases_per_million), 
           !is.na(new_deaths_per_million), 
           !is.na(hosp_patients_per_million), 
           !is.na(icu_patients_per_million)) %>% 
    ungroup
  
  ## adding the year and day-of-the-year information
  ## coding for the day of the week and train/test splitting point
  
  cov_data$data <- cov_data$data %>% 
    mutate(day = date - as.Date('2021-06-15'), 
           day_week = factor(as.numeric(day) %% 7), 
           split = ifelse(date < as.Date('2021-12-20'), 'train', 'test'))
  
  ## filling in the missing booster data (set to 0) for the time period when no booster vax 
  ## was approved (let's say 2021-10-01)
  
  cov_data$data <- cov_data$data %>% 
    mutate(total_boosters_per_hundred = ifelse(date < as.Date('2021-10-01') & is.na(total_boosters_per_hundred), 
                                               0, total_boosters_per_hundred))
  
  ## filling in the missing information - in case of the new tests and vaccinations, 
  ## I assume that the last non-missing value may be filled in
  
  cov_data$data <- cov_data$data %>% 
    group_by(location) %>% 
    fill_missing(variable = 'new_tests_per_thousand') %>% 
    fill_missing(variable = 'people_fully_vaccinated_per_hundred') %>% 
    fill_missing(variable = 'total_boosters_per_hundred') %>% 
    fill_missing(variable = 'stringency_index') %>% 
    ungroup
  
# some globals ----
  
  cov_data$countries <- unique(cov_data$data$location)
  
  cov_data$responses <- c(cases = 'new_cases_per_million', 
                          fatal = 'new_deaths_per_million', 
                          icu = 'icu_patients_per_million', 
                          hosp = 'hosp_patients_per_million')
  
  cov_data$indep_vars <- c('day_week', 
                           'total_cases_per_million', 
                           'new_tests_per_thousand', 
                           'people_fully_vaccinated_per_hundred', 
                           'total_boosters_per_hundred', 
                           'stringency_index', 
                           paste('new_cases_per_million_lag', 1:14, sep = '_'), 
                           paste('icu_patients_per_million_lag', 1:14, sep = '_'), 
                           paste('hosp_patients_per_million_lag', 1:14, sep = '_'))
  
# END -----