

library(tidyverse)
library(patchwork)
library(lubridate)
library(tidygraph)

source('visualizations/app2/geom_horizon.R')


get_covid_data <- function() {
    
    results <-
      vroom::vroom('http://covidtracking.com/api/states/daily.csv') %>%
      mutate(date = lubridate::ymd(as.character(date))) %>% 
      mutate_if(is.numeric, function(x) replace_na(x, 0))
    
  
  return(results)
  
}

covid_data <-
  get_covid_data()

covid_data %>% 
  filter(totalTestResultsIncrease > 0) %>% 
  mutate(daily_positive_proportion = positiveIncrease / totalTestResultsIncrease) %>% 
  select(date, state, daily_positive_proportion) %>% 
  filter(state %in% c('NY', 'PA', 'NJ', 'MA', 'CT', 'RI', 'VT')) %>% 
  ggplot() +
  geom_horizon(aes(x = date, y = daily_positive_proportion), bandwidth = .1) +
  scale_fill_gradient(low = 'lightblue', high = 'blue') +
  facet_grid(state ~ .) +
  theme_minimal()


covid_data %>% 
  filter(totalTestResultsIncrease > 0) %>% 
  mutate(daily_positive_proportion = positiveIncrease / totalTestResultsIncrease) %>% 
  select(date, state, daily_positive_proportion) %>% 
  filter(
    state %in% c('NY', 'NJ', 'VT'),
    date > as.Date('2020-04-01'),
    daily_positive_proportion < 1
  ) %>% 
  pivot_wider(names_from = state, values_from = daily_positive_proportion) %>% 
  ggplot() +
  geom_point(aes(x = dplyr::lead(NY, 7), y = NJ)) +
  scale_x_log10() +
  scale_y_log10()


neighbors <- list(
  
  'AL' = c('FL', 'GA', 'MS', 'TN'),
  'AK' = c(),
  'AR' = c('LA', 'MS', 'MS', 'OK', 'TN', 'TX'),
  'CA' = c('AZ', 'NV', 'OR'),
  'CO' = c('AK', 'KS', 'NB', 'NM', 'OK', 'UT', 'WY'),
  'CT' = c('MA', 'NY', 'RI'),
  'DE' = c('MD', 'NJ', 'PA'),
  'FL' = c('AL', 'GA'),
  'GA' = c('AL', 'FL', 'NC', 'SC', 'TN'),
  'HI' = c(),
  'ID' = c('MT', 'NV', 'OR', 'UT', 'WA', 'WY'),
  'IL' = c('IN', 'IO', 'MI', 'MO', 'KY', 'WI'),
  'IN' = c('IL', 'KY', 'MI', 'OH'),
  'IO' = c('IL', 'MN', 'NB', 'SD', 'WI'),
  'KS' = c('CO', 'MO', 'NB', 'OK'),
  'KY' = c('IL', 'IN', 'MO', 'OH', 'TN', 'VA', 'WV'),
  'LA' = c('AK', 'MS', 'TX'),
  'ME' = c('NH'),
  'MD' = c('DE', 'PA', 'VA', 'WV'),
  'MA' = c('CT', 'NH', 'NY', 'RI', 'VT'),
  'MI' = c('IL', 'IN', 'OH', 'WI'),
  'MN' = c('IO', 'ND', 'SD', 'WI'),
  'MS' = c('AL', 'AR', 'LA', 'TN'),
  'MO' = c('AR', 'IL', 'IO', 'KS', 'KY', 'NB', 'OK', 'TN'),
  'MT' = c('ID', 'ND', 'SD', 'WY'),
  'NB' = c('CO', 'IO', 'KS', 'MO', 'SD', 'WY'),
  'NV' = c('AZ', 'CA', 'ID', 'OR', 'UT'),
  'NH' = c('ME', 'MA', 'VT'),
  'NJ' = c('DE', 'NY', 'PA'),
  'NM' = c('AZ', 'CO', 'OK', 'TX', 'UT'),
  'NY' = c('CT', 'MA', 'NJ', 'PA', 'VT'),
  'NC' = c('GA', 'SC', 'TN', 'VA'),
  'ND' = c('MN', 'MT', 'SD'),
  'OH' = c('IN', 'KY', 'MI', 'PA', 'WV'),
  'OK' = c('AR', 'CO', 'KS', 'MO', 'NM', 'TX'),
  'OR' = c('CA', 'ID', 'NV', 'WA'),
  'PA' = c('DE', 'MD', 'NJ', 'NY', 'OH', 'WV'),
  'RI' = c('CT', 'MA'),
  'SC' = c('GA', 'NC'),
  'SD' = c('IO', 'MN', 'MT', 'NB', 'ND', 'WY'),
  'TN' = c('AL', 'AR', 'GA', 'KY', 'MS', 'MO', 'NC', 'VA'),
  'TX' = c('AR', 'LA', 'NM', 'OK'),
  'UT' = c('AZ', 'CO', 'ID', 'NM', 'WY'),
  'VT' = c('MA', 'NH', 'NY'),
  'VA' = c('KY', 'MD', 'NC', 'TN', 'WV'),
  'WA' = c('ID', 'OR'),
  'WV' = c('KY', 'MD', 'OH', 'PA', 'VA'),
  'WI' = c('IL', 'IO', 'MI', 'MN'),
  'WY' = c('CO', 'ID', 'MT', 'NB', 'SD', 'UT')

)

neighbors_edgelist = NULL
for (from in names(neighbors)) {
  for (to in neighbors[[from]]) {
    
    neighbors_edgelist <-
      rbind(
        neighbors_edgelist,
        data.frame(from = from, to = to)
      )
  
  }
}

covid_time_series <-
  covid_data %>% 
  filter(totalTestResultsIncrease > 0) %>% 
  mutate(daily_positive_proportion = positiveIncrease / totalTestResultsIncrease) %>% 
  select(date, state, daily_positive_proportion)

df <-
  neighbors_edgelist %>% 
  inner_join(
    covid_time_series %>% 
      rename(
        from_state = state,
        from_daily_positive_proportion = daily_positive_proportion
      ),
    by = c('from' = 'from_state')
  ) %>% 
  inner_join(
    covid_time_series %>% 
      rename(
        to_state = state,
        to_daily_positive_proportion = daily_positive_proportion
      ),
    by = c('date', 'to' = 'to_state')
  ) %>% 
  pivot_wider(names_from = to, values_from = to_daily_positive_proportion, values_fn = list(to_daily_positive_proportion = unique)) %>%
  as_tibble()
