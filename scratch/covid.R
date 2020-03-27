
library(tidyverse)
library(rvest)
library(lubridate)

content_url <- 
  'https://en.wikipedia.org/wiki/Template:2019%E2%80%9320_coronavirus_pandemic_data/United_States_medical_cases'


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
  mutate(
    Date = mdy(paste(Date, ', 2020'))
  ) %>% 
  drop_na(Date) %>% 
  pivot_longer(AK:PR, names_to = 'Region', values_to = 'n') %>% 
  mutate(
    n = as.numeric(n),
    n = replace_na(n, 0)
  )

counts %>% 
  arrange(Date) %>%
  group_by(Region) %>% 
  mutate(cumu_n = cumsum(n)) %>%
  ungroup() %>% 
  filter(Date >= Sys.Date() - 30) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = cumu_n, group = Region), stat = 'identity') +
  facet_wrap(~ Region, scales = 'free_y') +
  geom_vline(xintercept = Sys.Date(), linetype = 'dotted') +
  scale_y_log10() +
  ggthemes::theme_hc() +
  ggtitle('Total Cases')


counts %>% 
  group_by(Date) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(Date) %>%
  mutate(cumu_n = cumsum(n)) %>% 
  filter(Date >= Sys.Date() - 30) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = cumu_n), stat = 'identity') +
  geom_vline(xintercept = Sys.Date(), linetype = 'dotted') +
  scale_y_log10() +
  ggthemes::theme_hc() +
  ggtitle('Total Cases')


counts %>% 
  filter(Date >= Sys.Date() - 30) %>% 
  ggplot() +
  geom_bar(aes(x = Date, y = n), stat = 'identity', width = .75) +
  geom_vline(xintercept = Sys.Date(), linetype = 'dotted') +
  facet_wrap(~ Region, scales = 'free_y') +
  ggthemes::theme_hc() +
  ggtitle('Daily Cases')


counts %>% 
  group_by(Date) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  filter(Date >= Sys.Date() - 30) %>% 
  ggplot() +
  geom_bar(aes(x = Date, y = n), stat = 'identity') +
  geom_vline(xintercept = Sys.Date(), linetype = 'dotted') +
  ggthemes::theme_hc() +
  ggtitle('Daily Cases')


counts %>% 
  group_by(Region) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(Region = fct_reorder(Region, n, function(x) x)) %>% 
  ggplot() +
  geom_bar(aes(x = Region, y = n), stat = 'identity') +
  coord_flip() +
  theme_minimal()



