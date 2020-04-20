
library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(patchwork)
library(rstan)

state_ <- 'NY'

model <-
  stan_model('models/growth_characteristic2/stan/gc.stan')

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
  filter(state == state_) %>% 
  group_by(state) %>% 
  filter(positive > .1 * max(positive)) %>% 
  ungroup() 

stan_df <-
  df %>% 
  mutate(
    y = positiveIncrease / positive,
    x = positive 
  ) %>% 
  select(x, y)

stan_data <-
  stan_df %>% 
  compose_data()

fit <-
  sampling(model, data = stan_data, chains = 1, iter = 2000)

trace <-
  spread_draws(fit, A, r, v, sigma)

pred <-
  modelr::data_grid(
    stan_df,
    x = modelr::seq_range(x, 30, expand = 1)
  ) %>% 
  filter(x > 0) %>% 
  merge(trace) %>% 
  mutate(
    ypred = rnorm(n(), r * (1 - (x / A) ^ v), sigma)
  )

pred_quantile <-
  pred %>% 
  group_by(x) %>% 
  summarize(
    low = quantile(ypred, .01),
    med = median(ypred),
    high = quantile(ypred, .99)
  ) %>% 
  ungroup()


ggplot() +
  geom_ribbon(aes(x = x, ymin = low, ymax = high), alpha = .2, data = pred_quantile) +
  geom_line(aes(x = x, y = med), data = pred_quantile) +
  geom_point(aes(x = x, y = y), data = stan_df) +
  geom_hline(yintercept = 0) +
  theme_minimal()



