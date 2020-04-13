
library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)

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

m <-
  covid_data %>% 
  filter(state == state_) %>%
  group_by(state) %>% 
  filter(positive > .15 * max(positive)) %>% 
  ungroup() %>% 
  drop_na(positive, positiveIncrease) %>% 
  stan_glm(positiveIncrease / positive  ~ positive, data = ., chains = 1, iter = 10000)

current_cases <-
  covid_data %>% 
  filter(state == 'NY') %>% 
  pull(positive) %>% 
  max()

trace <-
  spread_draws(m, `(Intercept)`, positive) %>% 
  rename(intercept = `(Intercept)`) %>% 
  mutate(
    r = intercept,
    A = - intercept / positive,
    m = - 1 / r * log(current_cases / (A - current_cases))
  )


# growth

covid_data %>% 
  filter(state == state_) %>% 
  group_by(state) %>% 
  filter(positive > .15 * max(positive)) %>% 
  ggplot(aes(x = positive, y = positiveIncrease / positive)) +
  geom_line() +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~ state, scales = 'free') +
  theme_minimal() +
  theme(aspect.ratio = 1)


# carrying capacity

trace %>% 
  ggplot() +
  geom_histogram(aes(x = A), bins = 50) +
  theme_minimal()


# current percent runthrough

trace %>% 
  ggplot() +
  geom_histogram(aes(x = current_cases / A), bins = 50) +
  xlim(0, 1) +
  theme_minimal()


# days from today until 100p % runthrough

p <-
  .98

trace %>% 
  ggplot() +
  geom_histogram(aes(x = m + 1/r * log(p / (1 - p))), bins = 50) +
  theme_minimal()


