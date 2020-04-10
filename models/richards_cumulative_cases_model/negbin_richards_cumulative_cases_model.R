

library(rstan)
library(tidyverse)
library(rvest)
library(lubridate)
library(tidybayes)
library(patchwork)

rstan_options(auto_write = TRUE)

region <- 'NY'

cumulative_model <-
  stan_model('models/richards_cumulative_cases_model/stan/richards_cumulative_model.stan')

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

ny_covid_data <-
  covid_data %>% 
  filter(state == region) %>% 
  arrange(date) %>% 
  mutate(DaysOut = as.numeric(difftime(date, Sys.Date(), units = 'days'))) %>% 
  filter(DaysOut >= -28)


stan_data <-
  list(
    n = nrow(ny_covid_data),
    t = ny_covid_data$DaysOut,
    cumu_y = ny_covid_data$positive
  )

fit <-
  sampling(cumulative_model, data = stan_data, chains = 1, iter = 10000)

trace <-
  spread_draws(fit, A, m, s, nu, phi) 

predictions <-
  merge(
    trace,
    data.frame(expand.grid(t = seq(-14, 14)))
  ) %>% 
  mutate(
    cumu_y_pred = rnbinom(n(), mu = A  / (1 + exp(-(t - m) / s)) ^ (1 / nu), size = phi)
  ) 

predictions_quantile <-
  predictions %>% 
  group_by(t) %>% 
  summarize(
    LowPred2 = max(quantile(cumu_y_pred, .01), 0),
    LowPred1 = max(quantile(cumu_y_pred, .1), 0),
    MedianPred = max(median(cumu_y_pred), 0),
    HighPred1 = max(quantile(cumu_y_pred, .9), 0),
    HighPred2 = max(quantile(cumu_y_pred, .99), 0)
  ) %>% 
  ungroup()

curves <-
  ggplot() +
  geom_ribbon(aes(x = t, ymin = LowPred2, ymax = HighPred2), color = 'gray', data = predictions_quantile, alpha = .25) +
  geom_ribbon(aes(x = t, ymin = LowPred1, ymax = HighPred1), color = 'gray', data = predictions_quantile, alpha = .25) +
  geom_line(aes(x = t, y = MedianPred), color = 'red', data = predictions_quantile) +
  geom_line(aes(x = DaysOut, y = positive), data = ny_covid_data, stat = 'identity', color = 'black', size = 1) +
  xlim(-21, 14) +
  ggtitle('Cumulative Cases Prediction')


median_point <-
  trace %>% 
  ggplot() +
  geom_histogram(aes(x = m + s * log(.5 ^ nu / (1 - .5 ^ nu))), bins = 100) +
  xlim(-21, 14)

curves / median_point
