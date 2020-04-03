
ts_data <-
  vroom::vroom('https://coronadatascraper.com/timeseries-tidy.csv')


other_summaries <-
  ts_data %>% 
  filter(
    aggregate == 'country',
    level == 'country',
    type %in% c('cases', 'deaths', 'tested')
  ) %>% 
  group_by(country) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(
    deaths_per_million_capita = 1e6 * deaths / population,
    deaths_per_case = deaths / cases
  ) %>% 
  select(country, deaths_per_million_capita, deaths_per_case)

us_summaries <-
  ts_data %>% 
  filter(
    country == 'United States',
    type %in% c('cases', 'deaths', 'tested')
  ) %>% 
  filter(date == max(date)) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  summarize(
    country = unique(country),
    deaths_per_million_capita = 1e6 * sum(deaths, na.rm = TRUE) / sum(population, na.rm = TRUE),
    deaths_per_case = sum(deaths, na.rm = TRUE) / sum(cases, na.rm = TRUE)
  ) %>% 
  select(country, deaths_per_million_capita, deaths_per_case)

summaries <-
  rbind(other_summaries, us_summaries)

summaries %>% 
  mutate(country = fct_reorder(country, deaths_per_case)) %>% 
  ggplot() +
  geom_point(aes(x = deaths_per_case, y = country))

