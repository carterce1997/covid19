

library(rstan)
library(tidyverse)
library(rvest)
library(lubridate)
library(tidybayes)
library(patchwork)

rstan_options(auto_write = TRUE)

region <- 'NY'

cumulative_model <-
  stan_model('stan/richards_cumulative_model.stan')

get_covid_data <- function() {
  
  content_url <- 
    'https://en.wikipedia.org/wiki/Template:2019-20_coronavirus_pandemic_data/United_States_medical_cases'
  
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
    counts_raw[-1,-ncol(counts_raw):-(ncol(counts_raw) - 6)] 
  
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

ny_covid_data <-
  covid_data %>% 
  filter(
    Region == region,
    !(Date == max(Date) & n == 0)
  ) %>% 
  arrange(Date) %>% 
  mutate(
    DaysOut = as.numeric(difftime(Date, Sys.Date(), units = 'days')),
    cumu_n = cumsum(n)
  ) %>% 
  filter(
    DaysOut >= -14
  )


stan_data <-
  list(
    n = nrow(ny_covid_data),
    t = ny_covid_data$DaysOut,
    cumu_y = ny_covid_data$cumu_n
  )

fit <-
  sampling(cumulative_model, data = stan_data, chains = 1, iter = 4000)


trace <-
  spread_draws(fit, A, m, s, inv_nu, phi) 

predictions <-
  merge(
    trace,
    data.frame(expand.grid(t = seq(-14, 14)))
  ) %>% 
  mutate(
    cumu_y_pred = rnbinom(n(), mu = A  / (1 + exp(-(t - m) / s)) ^ inv_nu, size = phi)
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
  geom_line(aes(x = DaysOut, y = cumu_n), data = ny_covid_data, stat = 'identity', color = 'black', size = 1) +
  xlim(-21, 14) +
  ggtitle('Cumulative Cases Prediction')

curves

