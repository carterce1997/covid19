

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
  filter(positiveIncrease > 0)

df %>% 
  ggplot(aes(x = positive, y = positiveIncrease, group = state)) +
  geom_line(alpha = 0.1) +
  geom_line(aes(x = positive, y = 0.25 * positive), data = df %>% filter(state == 'USA'), linetype = 'dotted', color = 'red') +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle('Growth') +
  theme_minimal()

