
library(shiny)
library(tidyverse)
library(rvest)
library(lubridate)

get_covid_cases <- function() {
  
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
  
  return(counts)
  
}

get_covid_deaths <- function() {
  
  content_url <- 
    'https://en.wikipedia.org/wiki/Template:2019%E2%80%9320_coronavirus_pandemic_data/United_States_medical_cases'
  
  content <-
    read_html(content_url)
  
  counts_raw <-
    content %>% 
    html_nodes('table') %>% 
    .[2] %>% 
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
    pivot_longer(AK:PA, names_to = 'Region', values_to = 'n') %>% 
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
      selectizeInput('region', 'Region', choices = sort(unique(get_covid_cases()$Region)), selected = 'NY'),
      checkboxInput('logscale', 'Log Scale', value = FALSE),
      dateRangeInput('daterange', 'Date Range', start = Sys.Date() - 14, end = Sys.Date()),
      h4('Context'),
      div(HTML('Data from <a href="https://en.wikipedia.org/wiki/Template:2019-20_coronavirus_pandemic_data/United_States_medical_cases" target="_blank">here</a>. Confirmed medical cases only. Data is refreshed every five minutes, though most states report numbers around 4pm EST.')),
      width = 2
    ),
    mainPanel(
      fluidRow(
        column(
          3,
          plotOutput('cumulative_cases_plot', height = '300px'),
          plotOutput('daily_cases_plot', height = '300px')
          # plotOutput('cases_gf_plot', height = '200px')
        ),
        column(
          3,
          plotOutput('cumulative_deaths_plot', height = '300px'),
          plotOutput('daily_deaths_plot', height = '300px')
          # plotOutput('deaths_gf_plot', height = '200px')
        ),
        column(
          6,
          plotOutput('phase_space_plot', height = '600px')
        )
      ),
      width = 10
    )
  )
)

server <- function(input, output, session) {
  
  covid_cases <- 
    reactive({
    
      invalidateLater(millis = 5 * 60 * 1000) # five minute refresh
      
      get_covid_cases() %>% 
        group_by(Region) %>% 
        filter(!(Date == max(Date) & n == 0)) %>% 
        ungroup()
      
    })
  
  covid_deaths <- 
    reactive({
      
      invalidateLater(millis = 5 * 60 * 1000) # five minute refresh
      
      get_covid_deaths() %>% 
        group_by(Region) %>% 
        filter(!(Date == max(Date) & n == 0)) %>% 
        ungroup()
      
    })
  
  regions <- 
    reactive({
    
      sort(unique(covid_cases()$Region))
    
    })
  

  
  output$daily_cases_plot <- renderPlot({
    
    daily_cases <-
      covid_cases() %>% 
      filter(
        Date >= input$daterange[1] - 1,
        Date <= input$daterange[2] + 1,
        Region == input$region
      ) 
    
    ggplot() +
      geom_bar(aes(x = Date, y = n), stat = 'identity', width = .75, data = daily_cases) +
      ggthemes::theme_hc() +
      ggtitle('Daily Cases') +
      xlab('Date') +
      ylab('Daily Cases')
      
  })
  
  output$daily_deaths_plot <- renderPlot({
    
    daily_deaths <-
      covid_deaths() %>% 
      filter(
        Date >= input$daterange[1] - 1,
        Date <= input$daterange[2] + 1,
        Region == input$region
      ) 
    
    ggplot() +
      geom_bar(aes(x = Date, y = n), stat = 'identity', width = .75, data = daily_deaths) +
      ggthemes::theme_hc() +
      ggtitle('Daily Deaths') +
      xlab('Date') +
      ylab('Daily Deaths')
    
  })
  
  output$cumulative_cases_plot <- renderPlot({
    
    covid_cases() %>% 
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
  
  output$cumulative_deaths_plot <- renderPlot({
    
    covid_deaths() %>% 
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
      ggtitle('Total Deaths') +
      xlab('Date') +
      ylab('Cumulative Deaths')
    
  })
  
  output$phase_space_plot <- renderPlot({
    
    cases <-
      covid_cases() %>% 
      filter(Region == input$region) %>% 
      arrange(Date) %>%
      mutate(
        cumu_n = cumsum(n),
        Type = 'Cases'
      )

    # deaths <-
    #   covid_deaths() %>% 
    #   filter(Region == input$region) %>% 
    #   arrange(Date) %>%
    #   mutate(
    #     cumu_n = cumsum(n), 
    #     Type = 'Deaths'
    #   )
    
    cases %>% 
    ggplot() +
      geom_line(aes(x = cumu_n, y = n), stat = 'identity') +
      scale_x_log10() +
      scale_y_log10() +
      # scale_color_manual(values = c('Deaths' = 'red', 'Cases' = 'black')) +
      theme_minimal() +
      ggtitle('Phase Space') +
      xlab('Cumulative Cases') +
      ylab('Daily Cases')
    
  })
  
  # output$cases_gf_plot <- renderPlot({
  #   
  #   covid_cases() %>% 
  #     filter(
  #       Date >= input$daterange[1] - 1,
  #       Date <= input$daterange[2] + 1,
  #       Region == input$region
  #     ) %>% 
  #     arrange(Date) %>% 
  #     mutate(
  #       GrowthFactor = n / lag(n)
  #     ) %>% 
  #     ggplot() +
  #     geom_line(aes(x = Date, y = GrowthFactor), stat = 'identity', width = .75) +
  #     ggthemes::theme_hc() +
  #     geom_hline(yintercept = 1, linetype = 'dashed') +
  #     ggtitle('Growth Factor') +
  #     xlab('Date') +
  #     ylab('Growth Factor')
  #   
  # })
  # 
  # output$deaths_gf_plot <- renderPlot({
  #   
  #   covid_deaths() %>% 
  #     filter(
  #       Date >= input$daterange[1] - 1,
  #       Date <= input$daterange[2] + 1,
  #       Region == input$region
  #     ) %>% 
  #     arrange(Date) %>% 
  #     mutate(
  #       GrowthFactor = n / lag(n)
  #     ) %>% 
  #     ggplot() +
  #     geom_line(aes(x = Date, y = GrowthFactor), stat = 'identity', width = .75) +
  #     ggthemes::theme_hc() +
  #     geom_hline(yintercept = 1, linetype = 'dashed') +
  #     ggtitle('Deaths Growth Factor') +
  #     xlab('Date') +
  #     ylab('Deaths Growth Factor')
  #   
  # })
  
  
}

shinyApp(ui, server)