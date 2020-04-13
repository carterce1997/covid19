

library(rstan)
library(tidyverse)
library(rvest)
library(lubridate)
library(tidybayes)
library(patchwork)

rstan_options(auto_write = TRUE)

region <- 'NY'

cumulative_model <-
  stan_model('models/logistic_cumulative_cases_model/stan/cumulative_model.stan')


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
  mutate(DaysOut = as.numeric(difftime(date, Sys.Date(), units = 'days'))) 

stan_data <-
  list(
    n = nrow(ny_covid_data),
    t = ny_covid_data$DaysOut,
    cumu_y = ny_covid_data$positive
  )

fit <-
  sampling(cumulative_model, data = stan_data, chains = 1, iter = 4000)


trace <-
  spread_draws(fit, A, m, s, phi) %>% 
  filter(A <= quantile(A, .9))


predictions <-
  merge(
    trace,
    data.frame(expand.grid(t = seq(-14, 14)))
  ) %>% 
  mutate(
    mu_pred = A  / (1 + exp(-(t - m) / s))
  )

curves <-
  ggplot() +
  geom_line(aes(x = t, y = mu_pred, group = .draw), data = predictions, alpha = .01) +
  geom_line(aes(x = DaysOut, y = positive), data = ny_covid_data, stat = 'identity', color = 'red', size = 1) +
  xlim(-14, 14) +
  ylim(0, max(trace$A)) +
  ggtitle('Cumulative Cases Prediction')

curves

