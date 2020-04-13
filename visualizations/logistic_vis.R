
library(tidyverse)
library(lubridate)

get_covid_data <- function() {
  
  results <-
    vroom::vroom('http://covidtracking.com/api/states/daily.csv') %>% 
    mutate(date = ymd(date)) %>% 
    replace(is.na(.), 0)
  
  totals <-
    results %>% 
    group_by(date) %>% 
    summarize_all(function(x) if( 'numeric' %in% class(x)) sum(x) else NA) %>% 
    ungroup() %>% 
    mutate(state = 'USA')
  
  return(rbind(results, totals))
  
}

covid_data <-
  get_covid_data()


covid_data %>% 
  filter(state == 'USA') %>%
  ggplot() +
  geom_point(aes(x = positive, y = positiveIncrease / positive)) +
  facet_wrap(~ state) +
  theme_minimal()


