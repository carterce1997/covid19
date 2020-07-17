
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
  
  # readRDS('covid_data.rds')
  
}

cumulative_chart <- function(covid_data, state_, variable, logscale) {
  
  variable <-
    enquo(variable)
  
  covid_data %>% 
    filter(state == state_) %>% 
    ggplot() +
    geom_line(aes(x = date, y = !!variable)) +
    { if (logscale) scale_y_log10() } +
    theme_minimal()
  
}

daily_chart <- function(covid_data, state_, variable) {
  
  variable <-
    enquo(variable)
  
  covid_data %>% 
    filter(state == state_) %>% 
    ggplot() +
    geom_bar(aes(x = date, y = !!variable), stat = 'identity') +
    theme_minimal()
  
}

plot_height <- '300px'

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel('COVID-19 Dashboard'),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('state', 'State', choices = sort(unique(get_covid_data()$state)), selected = 'USA'),
      checkboxInput('logscale', 'Log Scale', value = TRUE),
      dateRangeInput('daterange', 'Date Range', start = as.Date('2020-03-01'), end = Sys.Date()),
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
              6,
              h3('Cases'),
              plotOutput('positive_daily_horizon', height = '800px')
            ),
            column(
              6,
              h3('Deaths'),
              plotOutput('deaths_daily_horizon', height = '800px')
            )
          )
        ),
        tabPanel(
          'State Statistics',
          fluidRow(
            column(
              3,
              h3('Cases'),
              div('Cumulative and daily positive cases.'),
              plotOutput('positive', height = plot_height),
              plotOutput('positive_daily', height = plot_height)
            ),
            column(
              3,
              h3('Tests'),
              div('Cumulative and daily tests.'),
              plotOutput('total', height = plot_height),
              plotOutput('total_daily', height = plot_height)
            ),
            column(
              3,
              h3('Hospitalizations'),
              div('Cumulative and daily hospitalizations.'),
              plotOutput('hospitalized', height = plot_height),
              plotOutput('hospitalized_daily', height = plot_height)
            ),
            column(
              3,
              h3('Deaths'),
              div('Cumulative and daily deaths.'),
              plotOutput('death', height = plot_height),
              plotOutput('death_daily', height = plot_height)
            )
          )
        ),
        tabPanel(
          'Growth Analysis',
          fluidRow(
            column(
              4,
              h3('New Cases vs Cumulative Cases'),
              div('If a state is exhibiting exponential growth, its curve will be parallel to the red line.'),
              plotOutput('phase', height = plot_height),
              h3('Normalized Growth vs Total'),
              div('If a state is exhibuting logistic growth, its curve will be a decreasing linear trend, and the carrying capacity of the state will be the value where the red line touches the x axis.'),
              plotOutput('growth_characteristic', height = plot_height)
            ),
            column(
              4,
              h3('Ratio of New Cases to Previous New Cases'),
              div('A growth factor above 1 (the red line) indicates day-to-day growth in number of new cases, and a factor below 1 indicates day-to-day decrease in new cases.'),
              plotOutput('gf', height = plot_height),
              h3('Percent of Tests that are Positive'),
              div('Number of tests divided by number of positive cases'),
              plotOutput('percent_positive', height = plot_height)
            ),
            column(
              4,
              h3('Growth Factor Overview'),
              div('A seven-day geometric average of growth factors across states.'),
              plotOutput('gf_overview', height = '600px')
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
    
    normalize <- function(x, ...) (x - min(x, ...)) / (max(x, ...) - min(x, ...))
    
    covid_data() %>% 
      select(date, state, positiveIncrease) %>% 
      filter(
        state != 'USA',
        positiveIncrease >= 0,
        !is.na(positiveIncrease)
      ) %>% 
      arrange(date) %>% 
      group_by(state) %>% 
      mutate(
        positiveIncrease = SMA(positiveIncrease, 3),
        latest_positiveIncrease = positiveIncrease[which.max(date)]
      ) %>% 
      ungroup() %>% 
      mutate(
        state = fct_rev(fct_reorder(state, latest_positiveIncrease))
      ) %>% 
      ggplot() +
      geom_horizon(aes(x = date, y = positiveIncrease), bandwidth = 1000) +
      facet_grid(state ~ .) +
      scale_fill_viridis_c() +
      theme_minimal() +
      theme(panel.spacing.y=unit(-0.05, "lines")) +
      theme(strip.text.y = element_text(hjust=0, angle=360)) +
      theme(axis.text.y=element_blank()) +
      theme(aspect.ratio = 1/35) +
      theme(panel.grid = element_blank()) +
      theme(legend.position = 'none')
    
  })
  
  output$deaths_daily_horizon <- renderPlot({
    
    normalize <- function(x, ...) (x - min(x, ...)) / (max(x, ...) - min(x, ...))
    
    covid_data() %>% 
      select(date, state, deathIncrease) %>% 
      filter(
        state != 'USA',
        deathIncrease >= 0,
        !is.na(deathIncrease)
      ) %>% 
      arrange(date) %>% 
      group_by(state) %>% 
      mutate(
        deathIncrease = SMA(deathIncrease, 3),
        latest_deathIncrease = deathIncrease[which.max(date)]
      ) %>% 
      ungroup() %>% 
      mutate(
        state = fct_rev(fct_reorder(state, latest_deathIncrease))
      ) %>% 
      ggplot() +
      geom_horizon(aes(x = date, y = deathIncrease), bandwidth = 50) +
      facet_grid(state ~ .) +
      scale_fill_viridis_c() +
      theme_minimal() +
      theme(panel.spacing.y=unit(-0.05, "lines")) +
      theme(strip.text.y = element_text(hjust=0, angle=360)) +
      theme(axis.text.y=element_blank()) +
      theme(aspect.ratio = 1/35) +
      theme(panel.grid = element_blank()) +
      theme(legend.position = 'none')
    
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
      geom_bar(aes(x = date, y = percent_positive), stat = 'identity') +
      theme_minimal()
    
  })
  
  output$gf_overview <- renderPlot({
    
    covid_data() %>% 
      arrange(date) %>% 
      group_by(state) %>% 
      filter(positiveIncrease > 0) %>% 
      mutate(growth_factor = positiveIncrease / lag(positiveIncrease)) %>% 
      slice(max(n() - 7, 1):n()) %>% 
      summarize(seven_day_growth_factor = exp(mean(log(growth_factor)))) %>% 
      ungroup() %>% 
      mutate(state = fct_reorder(state, seven_day_growth_factor)) %>% 
      ggplot(aes(x = seven_day_growth_factor, xend = 1, y = state, yend = state, alpha = state == input$state)) +
      geom_point() +
      geom_segment() +
      scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .2)) +
      geom_vline(xintercept = 1, linetype = 'dashed', color = 'red') + 
      theme_minimal() +
      theme(legend.position = 'none')
    
    
  })
  
  
}

shinyApp(ui, server)