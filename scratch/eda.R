
library(tidyverse)
library(tidybayes)
library(rstanarm)
library(lubridate)
library(rvest)
library(patchwork)

theme_set(theme_minimal())

region <- 
  'NY'

cumu_cases_lower_bound <-
  0

cumu_cases_log_scale <-
  FALSE

start_date <-
  as.Date('2020-03-16')

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

df <-
  covid_data %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(
    cumu_n = cumsum(n),
    log_growth_factor = log(n) - log(lag(n))
  ) %>% 
  ungroup() %>% 
  filter(is.finite(log_growth_factor))

m <-
  df %>% 
  filter(
    Region == region,
    cumu_n >= cumu_cases_lower_bound,
    Date >= start_date
  ) %>% 
  mutate(
    DateStamp = Date - Sys.Date()
  ) %>% 
  stan_glm(log_growth_factor ~ DateStamp, data = ., chains = 1, iter = 1e4)

trace <-
  m %>% 
  tidybayes::spread_draws(`(Intercept)`, DateStamp) %>% 
  filter(DateStamp <= 0) %>% # if there is a decreasing trend...
  mutate(DaysUntilInflection = - `(Intercept)` / DateStamp) %>% 
  filter(
    DaysUntilInflection <= quantile(DaysUntilInflection, .99),
    DaysUntilInflection >= quantile(DaysUntilInflection, .01)
  ) 

estimate_plot <-
  trace %>% 
  ggplot() +
  geom_histogram(aes(x = DaysUntilInflection), bins = 50) +
  geom_vline(aes(xintercept = median(DaysUntilInflection))) 


median_estimate_days <-
  trace %>% 
  pull(DaysUntilInflection) %>% 
  median() %>% 
  round()


gf_plot <-
  covid_data %>% 
  arrange(Date) %>% 
  filter(Region == region) %>% 
  mutate(
    cumu_n = cumsum(n),
    growth_factor = n / lag(n)
  ) %>% 
  filter(
    cumu_n >= cumu_cases_lower_bound,
    Date >= start_date
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = Date, y = growth_factor)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = Sys.Date() + median_estimate_days) + 
  xlim(start_date, max(Sys.Date(), Sys.Date() + median_estimate_days)) 

cumu_plot <-
  covid_data %>% 
  arrange(Date) %>% 
  filter(Region == region) %>% 
  mutate(
    cumu_n = cumsum(n)
  ) %>% 
  filter(
    cumu_n >= cumu_cases_lower_bound,
    Date >= start_date
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = Date, y = cumu_n)) +
  geom_line() +
  geom_vline(xintercept = Sys.Date() + median_estimate_days) +
  xlim(start_date, max(Sys.Date(), Sys.Date() + median_estimate_days)) +
  { if (cumu_cases_log_scale) scale_y_log10() }

( gf_plot / cumu_plot ) | estimate_plot

