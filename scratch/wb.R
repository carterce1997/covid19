
input = list(state = 'VT', logscale = TRUE)

df <-
  covid_data %>% 
  filter(positiveIncrease > 0)

xrange <-
  df %>% 
  filter(state == input$state) %>% 
  pull(positive) %>% 
  range()

yrange <-
  df %>% 
  filter(state == input$state) %>% 
  pull(positiveIncrease) %>% 
  range()


ggplot() +
  geom_line(
    aes(x = positive, y = positiveIncrease, group = state, alpha = state == input$state),
    data = df
  ) +
  geom_point(
    aes(x = positive, y = positiveIncrease), 
    data = df %>% 
      filter(
        date == max(date)
      )
  ) +
  ggrepel::geom_text_repel(
    aes(x = positive, y = positiveIncrease, label = state, alpha = state == input$state), 
    data = df %>% 
      filter(
        date == max(date)
      ), 
    nudge_x = .4,
    nudge_y = -.4,
    size = 2.5
  ) +
  scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .1)) +
  { if (input$logscale) scale_x_log10() else scale_x_continuous(limits = xrange) } +
  { if (input$logscale) scale_y_log10() else scale_y_continuous(limits = yrange) } +
  theme_minimal() +
  theme(legend.position = 'none')


