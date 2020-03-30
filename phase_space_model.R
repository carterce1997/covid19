
library(rstanarm)
library(tidybayes)
library(rvest)
library(tidyverse)
library(modelr)
library(lubridate)
library(patchwork)

phase_space_model <-
  stan_model('stan/phase_space_model.stan')

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

covid_data <-
  get_covid_data()


covid_data %>% 
  arrange(Date) %>% 
  group_by(Region) %>% 
  mutate(cumu_n = cumsum(n)) %>% 
  filter(!(Date == max(Date) & n == 0)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_path(aes(x = cumu_n, y = n, group = Region)) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Region) +
  theme_minimal()

region = 'USA'

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

fit <-
  stan_glm(y ~ 0 + I(cumu_y) + I(cumu_y^2), data = stan_df, chains = 1)

trace <-
  spread_draws(fit, `I(cumu_y)`, `I(cumu_y^2)`) %>% 
  rename(a = `I(cumu_y)`, b = `I(cumu_y^2)`) %>% 
  mutate(
    A = -a / b,
    s = 1 / a
  ) 

pred_grid <-
  stan_df %>% 
  data_grid(cumu_y = seq_range(c(cumu_y, 3 * max(cumu_y)), n = 100)) %>% 
  filter(cumu_y >= min(stan_df$cumu_y))

pred <-
  trace %>% 
  sample_n(200) %>%
  merge(pred_grid) %>% 
  mutate(y_pred = a * cumu_y + b * cumu_y ^ 2) %>% 
  filter(y_pred >= 0)

curves <-
  ggplot() +
  geom_line(aes(x = cumu_y, y = y_pred, group = .draw), data = pred, alpha = .05) +
  geom_point(aes(x = cumu_y, y = y), data = stan_df) +
  xlim(0, 3 * max(stan_df$cumu_y)) +
  # scale_y_log10() +
  # scale_x_log10() +
  theme_minimal()

estimate <-
  ggplot() +
  geom_histogram(aes(x = A), data = pred, bins = 100) +
  xlim(0, 3 * max(stan_df$cumu_y)) +
  theme_minimal() 

curves / estimate
