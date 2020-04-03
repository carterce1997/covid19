

library(rstan)
library(tidyverse)
library(rvest)
library(lubridate)
library(tidybayes)
library(patchwork)

rstan_options(auto_write = TRUE)

region <- 'NY'

cumulative_model <-
  stan_model('stan/cumulative_model.stan')

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
  filter(Region == region) %>% 
  arrange(Date) %>% 
  mutate(
    DaysOut = as.numeric(difftime(Date, Sys.Date(), units = 'days')),
    cumu_n = cumsum(n)
  ) %>% 
  filter(DaysOut >= -14)


stan_data <-
  list(
    n = nrow(ny_covid_data),
    t = ny_covid_data$DaysOut,
    cumu_y = ny_covid_data$cumu_n
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
  geom_line(aes(x = DaysOut, y = cumu_n), data = ny_covid_data, stat = 'identity', color = 'red', size = 1) +
  xlim(-14, 14) +
  ylim(0, max(trace$A)) +
  ggtitle('Cumulative Cases Prediction')

p <- .5
inflection_estimate <-
  trace %>% 
  ggplot() +
  geom_histogram(aes(x = m + s * log(p / (1 - p))), bins = 100) +
  geom_vline(aes(xintercept = median(m + s * log(p / (1 - p))))) +
  xlim(-14, 14) +
  ggtitle('Inflection Point Estimate')

total_cases_estimate <-
  trace %>% 
  ggplot() +
  geom_histogram(aes(x = A), bins = 50) +
  geom_vline(aes(xintercept = median(A))) +
  xlim(0, max(trace$A)) +
  coord_flip() +
  ggtitle('Total Cases Estimate')

curves + 
  total_cases_estimate + 
  inflection_estimate + 
  plot_spacer() +
  plot_layout(ncol = 2, widths = c(3, 1, 3, 1))


