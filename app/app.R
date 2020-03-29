
library(shiny)
library(tidyverse)
library(rvest)
library(lubridate)

get_covid_data <- function() {
  
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

ui <- fluidPage(
  titlePanel('COVID-19 Cases'),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('region', 'Region', choices = sort(unique(get_covid_data()$Region)), selected = 'NY'),
      checkboxInput('logscale', 'Log Scale', value = FALSE),
      dateRangeInput('daterange', 'Date Range', start = Sys.Date() - 14, end = Sys.Date()),
      h4('Context'),
      div(HTML('Data from <a href="https://en.wikipedia.org/wiki/Template:2019-20_coronavirus_pandemic_data/United_States_medical_cases" target="_blank">here</a>. Confirmed medical cases only. Data is refreshed every five minutes, though most states report numbers around 4pm EST.')),
      width = 3
    ),
    mainPanel(
      # tabsetPanel(
        # tabPanel(
          # 'Growth',
          fluidRow(
            column(
              6,
              plotOutput('cumulative_plot', height = '200px'),
              plotOutput('daily_plot', height = '200px'),
              plotOutput('gf_plot', height = '200px')
            ),
            column(
              6,
              plotOutput('phase_space_plot', height = '600px')
            )
          ),
        # ),
        # tabPanel(
        #   'Fastest Movers',
        #   plotOutput('daily_pareto', height = '300px'),
        #   plotOutput('cumulative_pareto', height = '300px')
        # )
      # ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  covid_data <- 
    reactive({
    
      invalidateLater(millis = 5 * 60 * 1000) # five minute refresh
      
      get_covid_data() %>% 
        group_by(Region) %>% 
        filter(!(Date == max(Date) & n == 0)) %>% 
        ungroup()
      
    })
  
  regions <- 
    reactive({
    
      sort(unique(covid_data()$Region))
    
    })
  

  
  output$daily_plot <- renderPlot({
    
    covid_data() %>% 
      filter(
          Date >= input$daterange[1] - 1,
          Date <= input$daterange[2] + 1,
          Region == input$region
      ) %>% 
      ggplot() +
      geom_bar(aes(x = Date, y = n), stat = 'identity', width = .75) +
      ggthemes::theme_hc() +
      ggtitle('Daily Cases') +
      xlab('Date') +
      ylab('Daily Cases')
      
  })
  
  output$cumulative_plot <- renderPlot({
    
    covid_data() %>% 
      filter(Region == input$region) %>% 
      arrange(Date) %>%
      mutate(cumu_n = cumsum(n)) %>%
      filter(
        Date >= input$daterange[1] - 1,
        Date <= input$daterange[2] + 1
      ) %>% 
      ggplot() +
      geom_line(aes(x = Date, y = cumu_n), stat = 'identity') +
      { if (input$logscale) scale_y_log10() } + 
      ggthemes::theme_hc() +
      ggtitle('Total Cases') +
      xlab('Date') +
      ylab('Cumulative Cases')
    
  })
  
  
  output$phase_space_plot <- renderPlot({
    
    covid_data() %>% 
      filter(Region == input$region) %>% 
      arrange(Date) %>%
      mutate(cumu_n = cumsum(n)) %>%
      ggplot() +
      geom_line(aes(x = cumu_n, y = n), stat = 'identity') +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal() +
      ggtitle('Total Cases') +
      xlab('Cumulative Cases') +
      ylab('Daily Cases')
    
  })
  
  output$gf_plot <- renderPlot({
    
    covid_data() %>% 
      filter(
        Date >= input$daterange[1] - 1,
        Date <= input$daterange[2] + 1,
        Region == input$region
      ) %>% 
      arrange(Date) %>% 
      mutate(
        GrowthFactor = n / lag(n)
      ) %>% 
      ggplot() +
      geom_line(aes(x = Date, y = GrowthFactor), stat = 'identity', width = .75) +
      ggthemes::theme_hc() +
      geom_hline(yintercept = 1, linetype = 'dashed') +
      ggtitle('Growth Factor') +
      xlab('Date') +
      ylab('Growth Factor')
    
  })
  
  # output$cumulative_pareto <- renderPlot({
  #   
  #   covid_data() %>% 
  #     group_by(Region) %>% 
  #     summarize(n = sum(n)) %>% 
  #     ungroup() %>% 
  #     arrange(desc(n)) %>% 
  #     slice(1:10) %>%
  #     mutate(Region = fct_reorder(Region, n, function(x) {x})) %>% 
  #     ggplot() +
  #     geom_bar(aes(x = Region, y = n, fill = Region == input$region), stat = 'identity') +
  #     coord_flip() +
  #     scale_fill_manual(values = c('TRUE' = 'blue', 'FALSE' = 'gray')) +
  #     theme_minimal() +
  #     theme(legend.position = 'none') +
  #     ggtitle('Total Cases')
  #   
  # })
  # 
  # output$daily_pareto <- renderPlot({
  #   
  #   covid_data() %>% 
  #     filter(Date == max(Date)) %>% 
  #     group_by(Region) %>% 
  #     summarize(n = sum(n)) %>% 
  #     ungroup() %>% 
  #     arrange(desc(n)) %>% 
  #     slice(1:10) %>% 
  #     mutate(Region = fct_reorder(Region, n, function(x) {x})) %>% 
  #     ggplot() +
  #     geom_bar(aes(x = Region, y = n, fill = Region == input$region), stat = 'identity') +
  #     coord_flip() +
  #     scale_fill_manual(values = c('TRUE' = 'blue', 'FALSE' = 'gray')) +
  #     theme_minimal() +
  #     theme(legend.position = 'none') +
  #     ggtitle('Daily Cases')
  #   
  # })
  
  
  
}

shinyApp(ui, server)