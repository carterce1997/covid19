
library(tidyverse)
library(sf)
library(leaflet)

spatial <-
  vroom::vroom('https://coronadatascraper.com/timeseries-tidy.csv')

df <-
  spatial %>% 
  filter(
    country == 'USA',
    date == max(date),
    type == 'cases'
  ) %>% 
  drop_na(lat, long) %>% 
  st_as_sf(coords = c('long', 'lat'))


pal <-
  colorNumeric(c('blue', 'orange'), log1p(df$value))

df %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircles(color = ~pal(log1p(value)))
