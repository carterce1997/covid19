
library(tidyverse)
library(hrbrthemes)
library(TTR)

get_covid_data <- function() {
  
  results <-
    vroom::vroom('http://covidtracking.com/api/states/daily.csv') %>%
    mutate(date = lubridate::ymd(as.character(date))) %>% 
    mutate_if(is.numeric, function(x) replace_na(x, 0))
  
  totals <-
    results %>%
    group_by(date) %>%
    summarize_all(function(x) if('numeric' %in% class(x)) sum(x) else NA) %>%
    ungroup() %>%
    mutate(state = 'USA')
  
  return(rbind(results, totals))
  
}

covid_data <-
  get_covid_data()

normalize <- function(x, ...) (x - min(x, ...)) / (max(x, ...) - min(x, ...))

covid_data %>% 
  select(date, state, positiveIncrease) %>% 
  filter(
    state != 'USA',
    positiveIncrease >= 0,
    !is.na(positiveIncrease)
  ) %>% 
  arrange(date) %>% 
  group_by(state) %>% 
  mutate(
    positiveIncrease = SMA(positiveIncrease, 3),
    latest_positiveIncrease = positiveIncrease[which.max(date)]
  ) %>% 
  ungroup() %>% 
  mutate(
    state = fct_rev(fct_reorder(state, latest_positiveIncrease))
  ) %>% 
  ggplot() +
  geom_horizon(aes(x = date, y = positiveIncrease), bandwidth = 1000) +
  facet_grid(state ~ .) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(panel.spacing.y=unit(-0.05, "lines")) +
  theme(strip.text.y = element_text(hjust=0, angle=360)) +
  theme(axis.text.y=element_blank()) +
  theme(aspect.ratio = 1/35) +
  theme(panel.grid = element_blank()) +
  theme(legend.position = 'none')


