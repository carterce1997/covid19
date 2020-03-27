


library(rstan)
library(tidyverse)
library(rvest)
library(lubridate)
library(tidybayes)
library(patchwork)

rstan_options(auto_write = TRUE)

region <- 'NY'

hierarchical_cumulative_model <-
  stan_model('stan/hierarchical_cumulative_model.stan')

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

clean_covid_data <-
  covid_data %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(
    DaysOut = as.numeric(difftime(Date, Sys.Date(), units = 'days')),
    cumu_n = cumsum(n)
  ) %>% 
  ungroup() %>% 
  filter(
    DaysOut >= -14,
    !(Date == max(Date) & n == 0)
  )

stan_data <-
  clean_covid_data %>% 
  compose_data()

fit <-
  sampling(hierarchical_cumulative_model, data = stan_data, chains = 1, iter = 2000)

trace <-
  fit %>% 
  recover_types(clean_covid_data) %>% 
  spread_draws(A[Region], m[Region], s[Region], phi) 

inflection <-
  trace %>% 
  ungroup() %>% 
  mutate(Region = fct_reorder(Region, m)) %>% 
  ggplot(aes(x = m, y = Region)) +
  stat_pointintervalh() +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  theme(aspect.ratio = 2) +
  ggtitle('Estimated Days Until Inflection')

total_cases <-
  trace %>% 
  ungroup() %>% 
  mutate(Region = fct_reorder(Region, A)) %>% 
  ggplot(aes(x = A, y = Region)) +
  stat_pointintervalh() +
  scale_x_log10() +
  theme_minimal() +
  theme(aspect.ratio = 2) +
  ggtitle('Estimated Total Cases Over\nWhole Couse of Outbreak')

flatness <-
  trace %>% 
  ungroup() %>% 
  mutate(Region = fct_reorder(Region, s)) %>% 
  ggplot(aes(x = s, y = Region)) +
  stat_pointintervalh() +
  theme_minimal() +
  theme(aspect.ratio = 2) +
  ggtitle('Curve Flatness')

runthrough <-
  trace %>% 
  ungroup() %>% 
  mutate(PercentRunthrough = 1 / (1 + exp(m / s))) %>% 
  mutate(Region = fct_reorder(Region, -PercentRunthrough)) %>% 
  ggplot(aes(x = PercentRunthrough, y = Region)) +
  stat_pointintervalh() +
  xlim(0, 1) +
  theme_minimal() +
  theme(aspect.ratio = 2) +
  ggtitle('Percent Runthrough')

inflection + total_cases + flatness + plot_layout(nrow = 1)



