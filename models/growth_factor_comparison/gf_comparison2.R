
library(rstan)
library(tidybayes)

model <-
  stan_model('models/growth_factor_comparison/stan/hierarchical_normal_pooled_location_and_scale.stan')

covid_data <-
  get_covid_data()

gf_data <-
  covid_data %>% 
  arrange(date) %>% 
  group_by(state) %>% 
  filter(positiveIncrease > 0) %>% 
  mutate(
    log_growth_factor = log(positiveIncrease / lag(positiveIncrease))
  ) %>% 
  slice(max(n() - 7, 1):n()) %>% 
  ungroup() %>% 
  dplyr::select(state, log_growth_factor) %>% 
  drop_na(log_growth_factor)

stan_data <-
  gf_data %>% 
  rename(y = log_growth_factor) %>% 
  compose_data()

fit <- 
  sampling(model, data = stan_data, chains = 1)

trace <-
  gather_draws(fit, mu[state], sigma[state]) %>% 
  recover_types(gf_data)

trace




