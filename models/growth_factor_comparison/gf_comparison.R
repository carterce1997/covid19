
library(rstanarm)

covid_data <-
  get_covid_data()

gf_data <-
  covid_data %>% 
  arrange(date) %>% 
  group_by(state) %>% 
  filter(positiveIncrease > 0) %>% 
  mutate(
    log_growth_factor = positiveIncrease / lag(positiveIncrease)
  ) %>% 
  slice(max(n() - 5, 1):n()) %>% 
  ungroup() %>% 
  dplyr::select(state, log_growth_factor) %>% 
  drop_na(log_growth_factor)

m <-
  gf_data %>% 
  stan_glmer(log_growth_factor ~ 0 + (1 | state), data = ., chains = 1)

plot(m) +
  theme_minimal()




