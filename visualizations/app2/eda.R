
library(tidyverse)
library(patchwork)

get_covid_data <- function() {
  
  results <-
    vroom::vroom('http://covidtracking.com/api/states/daily.csv') %>% 
    mutate(date = ymd(date)) %>% 
    replace(is.na(.), 0)
  
  return(results)
  
}

covid_data <-
  get_covid_data()

state_ <-
  'NY'

logscale <-
  FALSE

cumulative_chart <- function(covid_data, state_, variable, logscale) {
  
  variable <-
    enquo(variable)
  
  covid_data %>% 
    filter(state == state_) %>% 
    ggplot() +
    geom_line(aes(x = date, y = !!variable)) +
    { if (logscale) scale_y_log10() } +
    ggthemes::theme_hc()
  
}

positive <-
  covid_data %>% 
  cumulative_chart(state_, positive, logscale)

hospitalized <-
  covid_data %>% 
  cumulative_chart(state_, hospitalized, logscale)

death <-
  covid_data %>% 
  cumulative_chart(state_, death, logscale)

total <-
  covid_data %>% 
  cumulative_chart(state_, total, logscale)


total + positive + hospitalized + death 

daily_chart <- function(covid_data, state_, variable) {
  
  variable <-
    enquo(variable)
  
  covid_data %>% 
    filter(state == state_) %>% 
    ggplot() +
    geom_bar(aes(x = date, y = !!variable), stat = 'identity') +
    ggthemes::theme_hc()
  
}


positive_daily <-
  covid_data %>% 
  daily_chart(state_, positiveIncrease)
  

hospitalized_daily <-
  covid_data %>% 
  daily_chart(state_, hospitalizedIncrease)


death_daily <-
  covid_data %>% 
  daily_chart(state_, deathIncrease)


total_daily <-
  covid_data %>% 
  daily_chart(state_, totalTestResultsIncrease)


total_daily + positive_daily + hospitalized_daily + death_daily 


phase_plot <-
  covid_data %>% 
  filter(positive > 50, positiveIncrease > 0) %>% 
  ggplot() +
  geom_line(aes(x = positive, y = positiveIncrease, group = state, alpha = state == state_)) +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .1)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = 'none')

phase_plot


covid_data %>% 
  filter(state == 'NY') %>% 
  filter(positive > 50, positiveIncrease > 0) %>% 
  arrange(date) %>% 
  mutate(growth_factor = positiveIncrease / lag(positiveIncrease)) %>% 
  select(growth_factor)

