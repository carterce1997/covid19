

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

df <-
  covid_data %>% 
  filter(state == 'NY') %>% 
  filter(positive > .15 * max(positive))

df %>% 
  ggplot(aes(x = positive, y = positiveIncrease / positive)) +
  geom_line() +
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, linetype = 'dotted', color = 'red') +
  geom_hline(aes(yintercept = 0)) +
  xlim(0, 1.5 * max(df$positive)) +
  ylim(0, max(df$positiveIncrease / df$positive)) +
  facet_wrap(~ state) +
  ggtitle('Growth') +
  theme_minimal()

