
library(rstan)
library(tidyverse)
library(tidybayes)
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

model <-
  stan_model('models/growth_factor_comparison/stan/hierarchical_normal_pooled_location_and_scale.stan')

covid_data <-
  get_covid_data()

gf_data <-
  covid_data %>% 
  filter(state != 'USA') %>% 
  arrange(date) %>% 
  group_by(state) %>% 
  filter(positiveIncrease > 0) %>% 
  mutate(
    growth_factor = log(positiveIncrease / lag(positiveIncrease))
  ) %>% 
  slice(max(n() - 14, 1):n()) %>% 
  ungroup() %>% 
  dplyr::select(state, growth_factor) %>% 
  drop_na(growth_factor) %>% 
  rename(y = growth_factor)

stan_data <-
  gf_data %>% 
  compose_data()

fit <- 
  sampling(model, data = stan_data, chains = 1, iter = 10000)

trace <-
  fit %>% 
  recover_types(gf_data) %>% 
  spread_draws(mu[state], sigma[state])

trace %>% 
  point_intervalh(mu) %>% 
  mutate(state = fct_reorder(state, mu)) %>% 
  ggplot(aes(x = mu, y = state)) +
  geom_pointintervalh() +
  theme_minimal()

trace %>% 
  point_intervalh(sigma) %>% 
  mutate(state = fct_reorder(state, sigma)) %>% 
  ggplot(aes(x = sigma, y = state)) +
  geom_pointintervalh() +
  theme_minimal()


trace %>% 
  group_by(state) %>% 
  summarize(
    mu = median(mu),
    sigma = median(sigma)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = mu, y = sigma, label = state)) +
  geom_point() +
  geom_text(hjust = -.2, vjust = -.2)




