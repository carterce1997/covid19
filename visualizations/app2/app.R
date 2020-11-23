
library(shiny)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(TTR)

source('geom_horizon.R')

get_covid_data <- function() {
  
  results <-
    vroom::vroom('http://covidtracking.com/api/states/daily.csv') %>%
    mutate(date = lubridate::ymd(as.character(date))) %>% 
    mutate_if(is.numeric, function(x) replace_na(x, 0))

  totals <-
    results %>%
    group_by(date) %>%
    summarize_all(function(x) if('numeric' %in% class(x)) sum(x) else NA) %>%
    ungroup() %>%
    mutate(state = 'USA')

  return(rbind(results, totals))
  
}

daily_chart <- function(covid_data, state_, variable) {
  
  variable <-
    enquo(variable)
  
  covid_data %>% 
    filter(
      state == state_,
      !!variable >= 0
    ) %>% 
    ggplot() +
    geom_line(aes(x = date, y = !!variable)) +
    theme_minimal()
  
}

horizon_chart <- function(.data, variable, order_by, normalized = FALSE, bands = 3, smoothwidth = 3) {
  
  variable <-
    enquo(variable)
  
  normalize <- function(x, ...) (x - min(x, ...)) / (max(x, ...) - min(x, ...))
  
  get_bandwidth <- function(x, bands, ...) (max(x, ...) - min(x, ...)) / bands
    
  plot_df <-
    .data %>% 
    filter(
      state != 'USA',
      !!variable >= 0,
      !is.na(!!variable)
    ) %>% 
    mutate(
      variable = !!variable,
      order_by_ = case_when(
        order_by == 'Cases' ~ positiveIncrease,
        order_by == 'Hospitalizations' ~ hospitalizedIncrease,
        order_by == 'Deaths' ~ deathIncrease
      )
    ) %>% 
    arrange(date) %>% 
    group_by(state) %>% 
    mutate(
      smoothed = SMA(variable, smoothwidth),
      smoothed_order_by_ = SMA(order_by_, smoothwidth),
      latest = smoothed_order_by_[which.max(date)]
    ) %>% 
    { if (normalized) mutate(., smoothed = normalize(smoothed, na.rm = TRUE)) else . } %>% 
    ungroup() %>% 
    mutate(
      state = fct_rev(fct_reorder(state, latest))
    ) 
  
  bandwidth <-
    get_bandwidth(plot_df$smoothed, bands, na.rm = TRUE)

  plot_df %>% 
    ggplot() +
    geom_horizon(aes(x = date, y = smoothed), bandwidth = bandwidth) +
    facet_grid(state ~ .) +
    scale_fill_gradient(low = 'lightblue', high = 'blue') +
    theme_minimal() +
    theme(
      panel.spacing.y = unit(-0.05, "lines"),
      strip.text.y = element_text(hjust=0, angle=360),
      axis.text.y = element_blank(),
      panel.grid = element_blank(), 
      axis.title = element_blank(),
      legend.position = 'none'
    )
  
}

plot_height <- '300px'

horizon_height <- '1200px'

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel('COVID-19 Dashboard'),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('state', 'State', choices = sort(unique(get_covid_data()$state)), selected = 'USA'),
      checkboxInput('logscale', 'Log Scale', value = TRUE),
      dateRangeInput('daterange', 'Date Range', start = as.Date('2020-02-01'), end = Sys.Date()),
      radioButtons('order_by', 'Order By', choices = c('Cases', 'Hospitalizations', 'Deaths')),
      sliderInput('horizon_bands', 'Number of Horizon Bands', min = 2, max = 20, value = 5),
      sliderInput('horizon_sma_period', 'Horizon SMA Period', min = 1, max = 7, value = 3),
      checkboxInput('horizon_normalize', 'Normalize Horizon Charts (Each State on a Different Scale)', value = FALSE),
      div(HTML('Data from <a target="_blank" href="https://covidtracking.com/">covidtracking.com</a>.')),
      hr(),
      div(HTML('If you would like to participate in this project, join the discussion on Slack <a target="_blank" href="https://join.slack.com/t/covid19datadi-nrv2825/shared_invite/zt-dajqaeac-nTNwKEtzkWUwqs_Y669csw">here</a>.')),
      hr(),
      div(HTML('Source code for the project can be found on Github <a target="_blank" href="https://github.com/carterce1997/covid19">here</a>.')),
      width = 2
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          'Overview',
          fluidRow(
            column(
              12,
              div(HTML('A horizon chart is a way of visualizing and comparing a large number of time series at the same time. They are a way of trading "shape" for "color"; each band represents a state (labeled on the right), the shape of each band gives a rough idea of the details of the trend, and the color of the band gives an indication of the magnitude. Darker bands mean more cases. To learn more about horizon charts, <a target="_blank" href="https://square.github.io/cubism/">click here</a>.'))
            )
          ),
          fluidRow(
            column(
              4,
              h3('Cases'),
              plotOutput('positive_daily_horizon', height = horizon_height)
            ),
            column(
              4,
              h3('Deaths'),
              plotOutput('deaths_daily_horizon', height = horizon_height)
            ),
            column(
              4,
              h3('Hospitalizations'),
              plotOutput('hospitalizations_daily_horizon', height = horizon_height)
            )
          )
        ),
        tabPanel(
          'Detail',
          fluidRow(
            column(
              4,
              h3('Cases'),
              div('Daily positive cases.'),
              # plotOutput('positive', height = plot_height),
              plotOutput('positive_daily', height = plot_height),
              h3('Tests'),
              div('Daily tests.'),
              # plotOutput('total', height = plot_height),
              plotOutput('total_daily', height = plot_height)
            ),
            column(
              4,
              h3('Hospitalizations'),
              div('Daily hospitalizations.'),
              # plotOutput('hospitalized', height = plot_height),
              plotOutput('hospitalized_daily', height = plot_height),
              h3('Deaths'),
              div('Daily deaths.'),
              # plotOutput('death', height = plot_height),
              plotOutput('death_daily', height = plot_height)
            ),
            column(
              4,
              h3('Percent of Tests that are Positive'),
              div('Number of tests divided by number of positive cases'),
              plotOutput('percent_positive', height = plot_height)
              
            )
          )
        )
      ),
      width = 10
    )
  )
)

server <- function(input, output, session) {
  
  covid_data <- reactive({
    
    invalidateLater(5 * 60 * 1000)
    
    get_covid_data() %>% 
      filter(date <= input$daterange[2], date >= input$daterange[1])
    
  })
  
  
  output$positive_daily_horizon <- renderPlot({
    
    covid_data() %>% 
      horizon_chart(positiveIncrease, order_by = input$order_by, normalized = input$horizon_normalize, bands = input$horizon_bands, smoothwidth = input$horizon_sma_period)
    
  })
  
  output$hospitalizations_daily_horizon <- renderPlot({
    
    covid_data() %>% 
      horizon_chart(hospitalizedIncrease, order_by = input$order_by, normalized = input$horizon_normalize, bands = input$horizon_bands, smoothwidth = input$horizon_sma_period)
    
  })
  
  output$deaths_daily_horizon <- renderPlot({
    
    covid_data() %>% 
      horizon_chart(deathIncrease,  order_by = input$order_by, normalized = input$horizon_normalize, bands = input$horizon_bands, smoothwidth = input$horizon_sma_period)
    
  })
  
  
  output$total <- renderPlot({
    
    covid_data() %>% 
      cumulative_chart(input$state, totalTestResults, input$logscale)
    
  })
  
  output$positive <- renderPlot({
    

    covid_data() %>% 
      cumulative_chart(input$state, positive, input$logscale)
    
  })  
  
  output$hospitalized <- renderPlot({
    
    covid_data() %>% 
      cumulative_chart(input$state, hospitalized, input$logscale)
    
  })  
  
  output$death <- renderPlot({
    
    covid_data() %>% 
      cumulative_chart(input$state, death, input$logscale)
    
  })  
  
  output$total_daily <- renderPlot({
    
    covid_data() %>% 
      daily_chart(input$state, totalTestResultsIncrease)
    
  })
  
  output$positive_daily <- renderPlot({
    
    covid_data() %>% 
      daily_chart(input$state, positiveIncrease)
    
  })  
  
  output$hospitalized_daily <- renderPlot({
    
    covid_data() %>% 
      daily_chart(input$state, hospitalizedIncrease)
    
  })  
  
  output$death_daily <- renderPlot({
    
    covid_data() %>% 
      daily_chart(input$state, deathIncrease)
    
  })  
  
  output$phase <- renderPlot({
    
    df <-
      covid_data() %>% 
      filter(
        state == input$state,
        positive > 10, 
        positiveIncrease > 0
      )
    
    ggplot() +
      geom_line(
        aes(x = positive, y = positiveIncrease),
        data = df
      ) +
      geom_point(
        aes(x = positive, y = positiveIncrease), 
        data = df %>% 
          filter(
            date == max(date)
          )
      ) +
      geom_line(aes(x = positive, y = .25 * positive), data = df, color = 'red', linetype = 'dashed') +
      ggrepel::geom_text_repel(
        aes(x = positive, y = positiveIncrease, label = state), 
        data = df %>% 
          filter(date == max(date)), 
        nudge_x = .4,
        nudge_y = -.4,
        size = 3
      ) +
      { if (input$logscale) scale_x_log10() } +
      { if (input$logscale) scale_y_log10() } +
      theme_minimal() +
      theme(legend.position = 'none')
    
  })
  
  output$growth_characteristic <- renderPlot({
    
    df <-
      covid_data() %>% 
      filter(state == input$state) %>% 
      filter(positive > .15 * max(positive))
    
    df %>% 
      ggplot(aes(x = positive, y = positiveIncrease / positive)) +
      geom_line() +
      stat_smooth(method = "lm", se = FALSE, fullrange = TRUE, linetype = 'dotted', color = 'red') +
      geom_hline(aes(yintercept = 0)) +
      xlim(0, 1.5 * max(df$positive)) +
      ylim(0, max(df$positiveIncrease / df$positive)) +
      facet_wrap(~ state) +
      ggtitle('Growth') +
      theme_minimal()
    
  })
  
  output$gf <- renderPlot({
    
    covid_data() %>% 
      filter(positiveIncrease > 0, state == input$state) %>% 
      arrange(date) %>% 
      mutate(growth_factor = positiveIncrease / lag(positiveIncrease)) %>% 
      ggplot() +
      geom_line(aes(x = date, y = growth_factor)) +
      geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
      scale_y_log10(expand = expand_scale(add = 1)) +
      theme_minimal()     
    
  })
  
  output$percent_positive <- renderPlot({
    
    covid_data() %>% 
      filter(
        state == input$state
      ) %>% 
      mutate(
        percent_positive =  positiveIncrease / totalTestResultsIncrease
      ) %>% 
      ggplot() +
      geom_line(aes(x = date, y = percent_positive)) +
      theme_minimal()
    
  })
  
  
}

shinyApp(ui, server)