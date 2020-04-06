

library(rstan)
library(tidybayes)
library(rvest)
library(tidyverse)
library(modelr)
library(lubridate)
library(patchwork)

region <- 'VT'

model <-
  stan_model('models/richards_phase_space_model/stan/richardson_phase_space_model.stan')

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
  
  total_counts <-
    counts %>% 
    group_by(Date) %>% 
    summarize(n = sum(n)) %>% 
    ungroup() %>% 
    mutate(Region = 'USA')
  
  return(rbind(counts, total_counts))
  
}

# covid_data <-
#   get_covid_data()

stan_df <-
  covid_data %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(cumu_n = cumsum(n)) %>% 
  filter(!(Date == max(Date) & n == 0)) %>%
  ungroup() %>% 
  filter(
    Region == region,
    cumu_n >= 1e1
  ) %>% 
  select(
    y = n,
    cumu_y = cumu_n
  ) 

stan_data <-
  compose_data(stan_df)

map <-
  optimizing(model, data = stan_data)

fit <-
  sampling(model, data = stan_data, init = map$par, chains = 1, iter = 4000)

trace <-
  spread_draws(fit, a, A, inv_nu, sigma)

multiplier <-
  10

pred_grid <-
  stan_df %>% 
  data_grid(cumu_y = seq_range(c(cumu_y, multiplier * max(cumu_y)), n = 100)) %>% 
  filter(cumu_y >= min(stan_df$cumu_y))

pred <-
  trace %>% 
  merge(pred_grid) %>% 
  mutate(y_pred = rnorm(n(), a * cumu_y * (1 - (cumu_y / A) ^ inv_nu), sigma))

pred_quantile <-
  pred %>% 
  group_by(cumu_y) %>% 
  summarize(
    LowPred2 = max(quantile(y_pred, .01), 0),
    LowPred1 = max(quantile(y_pred, .1), 0),
    MedianPred = max(median(y_pred), 0),
    HighPred1 = max(quantile(y_pred, .9), 0),
    HighPred2 = max(quantile(y_pred, .99), 0)
  ) %>% 
  ungroup()

curves <-
  ggplot() +
  geom_ribbon(aes(x = cumu_y, ymin = LowPred2, ymax = HighPred2), color = 'gray', data = pred_quantile, alpha = .25) +
  geom_ribbon(aes(x = cumu_y, ymin = LowPred1, ymax = HighPred1), color = 'gray', data = pred_quantile, alpha = .25) +
  geom_line(aes(x = cumu_y, y = MedianPred), color = 'red', data = pred_quantile) +
  geom_point(aes(x = cumu_y, y = y), data = stan_df) +
  xlim(0, multiplier * max(stan_df$cumu_y)) +
  theme_minimal()

estimate <-
  pred %>% 
  ggplot() +
  geom_histogram(aes(x = A), bins = 100) +
  geom_vline(aes(xintercept = median(A)), color = 'red') +
  xlim(0, multiplier * max(stan_df$cumu_y)) +
  theme_minimal() 

curves / estimate
