
library(tidyverse)

theme_set(theme_minimal())

covid_data <-
  vroom::vroom('https://coronadatascraper.com/timeseries.csv')


korea <-
  covid_data %>% 
  filter(
    country == 'KOR',
    ty
  )
  

korea_summary <-
  korea %>% 
  group_by(date) %>%
  summarize_all(function(x) if ('numeric' %in% class(x)) sum(x, na.rm = TRUE) else NA) %>% 
  ungroup() 


korea_summary %>% 
  ggplot() +
  geom_line(aes(x = date, y = tested))
