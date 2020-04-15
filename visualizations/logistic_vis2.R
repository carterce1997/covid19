

library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(patchwork)

state_ <- 'NY'

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
  group_by(state) %>% 
  filter(
    positive > .1 * max(positive),
    any(max(positive) > 10000),
    positiveIncrease >= 0
  ) %>%
  mutate(
    rate = positiveIncrease / positive,
    positive = positive / max(positive)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = positive, y = positiveIncrease / positive)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  geom_hline(aes(yintercept = 0)) +
  # facet_wrap(~ state) +
  ggtitle('Growth') +
  theme_minimal()
