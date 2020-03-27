


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

clean_covid_data <-
  covid_data %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(
    DaysOut = as.numeric(difftime(Date, Sys.Date(), units = 'days')),
    cumu_n = cumsum(n)
  ) %>% 
  ungroup() %>% 
  filter(DaysOut >= -14)

stan_data <-
  clean_covid_data %>% 
  compose_data()

fit <-
  sampling(hierarchical_cumulative_model, data = stan_data, chains = 1, iter = 4000)
