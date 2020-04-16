
library(tidyverse)
library(lubridate)
library(rstanarm)
library(tidybayes)
library(patchwork)

get_covid_data <- function() {
  
  results <-
    vroom::vroom('http://covidtracking.com/api/states/daily.csv') %>% 
    mutate(date = ymd(date)) %>% 
    replace(is.na(.), 0)
  
  totals <-
    results %>% 
    group_by(date) %>% 
    summarize_all(function(x) if( 'numeric' %in% class(x)) sum(x) else NA) %>% 
    ungroup() %>% 
    mutate(state = 'USA')
  
  return(rbind(results, totals))
  
}

covid_data <-
  get_covid_data()

df <-
  covid_data %>% 
  filter(state != 'USA') %>% 
  group_by(state) %>% 
  filter(positive > .05 * max(positive)) %>% 
  ungroup() 

m <-
  df %>% 
  stan_glmer(positiveIncrease / positive ~ 0 + positive:state + (1 | state), data = ., chains = 1)

trace <-
  as.data.frame(m)

trace2 <-
  trace %>% 
  dplyr::select(-sigma, -`Sigma[state:(Intercept),(Intercept)]`) %>% 
  mutate(iter = 1:n()) %>% 
  pivot_longer(`positive:stateAK`:`b[(Intercept) state:WY]`) %>% 
  mutate(
    type = case_when(
      str_detect(name, 'positive:state') ~ 'slope',
      str_detect(name, 'b\\[\\(Intercept\\) state:') ~ 'intercept'
    ),
    state = case_when(
      type == 'slope' ~ str_sub(name, -2, -1),
      type == 'intercept' ~ str_sub(name, -3, -2)
    )
  ) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(
    names_from = type, 
    values_from = value
  )


carrying_capacity <-
  trace2 %>% 
  group_by(state) %>% 
  summarize(
    high = quantile(-intercept / slope, .95),
    med = median(-intercept / slope),
    low = quantile(-intercept / slope, .05)
  ) %>% 
  mutate(state = fct_reorder(state, med)) %>% 
  ggplot() +
  geom_pointintervalh(aes(x = med, xmin = low, xmax = high, y = state)) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle('Carrying Capacity Estimate')

growth_rate <-
  trace2 %>% 
  group_by(state) %>% 
  summarize(
    high = quantile(intercept, .99),
    med = median(intercept),
    low = quantile(intercept, .01)
  ) %>% 
  mutate(state = fct_reorder(state, med)) %>% 
  ggplot() +
  geom_pointintervalh(aes(x = med, xmin = low, xmax = high, y = state)) +
  theme_minimal() +
  ggtitle('Growth Rate Estimate')

growth_rate + carrying_capacity

trace2 %>% 
  group_by(state) %>% 
  summarize(
    rate = median(intercept),
    carrying_capacity = median(-intercept / slope)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = rate, y = carrying_capacity, label = state)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  # ggrepel::geom_text_repel(hjust = -.3, vjust = -.3) +
  scale_y_log10() +
  theme_minimal()

  
  
  