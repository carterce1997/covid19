
library(rstan)
library(tidybayes)
library(rvest)
library(tidyverse)
library(lubridate)

phase_space_model <-
  stan_model('stan/phase_space_model.stan')

get_covid_data <- function() {
  
  content_url <- 
    'http://en.wikipedia.org/wiki/Template:2019-20_coronavirus_pandemic_data/United_States_medical_cases'
  
  content <-
    read_html(content_url)
  
  counts_raw <-
    content %>% 
    html_nodes('table') %>% 
    .[1] %>% 
    html_table(fill = TRUE, header = FALSE) %>% 
    as.data.frame()
  
  colnames(counts_raw) <- 
    counts_raw[2,]
  
  counts_raw <-
    counts_raw[-1:-2,-ncol(counts_raw):-(ncol(counts_raw) - 6)] 
  
  counts <-
    counts_raw %>% 
    filter(nchar(Date) <= 10) %>% 
    mutate(
      Date = mdy(paste(Date, ', 2020'))
    ) %>% 
    drop_na(Date) %>% 
    pivot_longer(AK:PR, names_to = 'Region', values_to = 'n') %>% 
    mutate(
      n = as.numeric(n),
      n = replace_na(n, 0)
    )
  
  return(counts)
  
}

covid_data <-
  get_covid_data()


covid_data %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(cumu_n = cumsum(n)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_path(aes(x = cumu_n, y = n, group = Region)) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Region) +
  theme_minimal()

stan_df <-
  covid_data %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(cumu_n = cumsum(n)) %>% 
  ungroup() %>% 
  filter(
    Region == 'NY',
    cumu_n >= 10
  ) %>% 
  select(
    y = n,
    cumu_y = cumu_n
  ) 

stan_data <-
  stan_df %>% 
  compose_data()


fit <-
  sampling(phase_space_model, data = stan_data, chains = 1)


trace <-
  spread_draws(fit, s, A)

pred <-
  trace %>% 
  sample_n(200) %>% 
  merge(data.frame(cumu_n = 10 ^ seq(1, 5, length.out = 100))) %>% 
  mutate(
    n_pred = cumu_n / s * (1 - cumu_n / A)
  )

ggplot() +
  geom_line(aes(x = cumu_n, y = n_pred, group = .draw), data = pred, alpha = .1) +
  geom_point(aes(x = cumu_y, y = y), data = stan_df) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()

