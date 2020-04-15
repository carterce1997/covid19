
library(shiny)
library(tidyverse)
library(arm)
library(lubridate)
library(tidybayes)
library(modelr)

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

phase_model_chart <- function(covid_data, state_) {
  
  stan_df <-
    covid_data %>% 
    filter(state == state_)
  
  fit <-
    lm(positiveIncrease ~ 0 + I(positive) + I(positive^2), data = stan_df)
  
  samples <-
    sim(fit, n.sims = 1e4)
  
  trace <-
    samples %>% 
    coef() %>% 
    as.data.frame()
  
  trace$sigma <-
    samples@sigma
  
  
  trace <-
    trace %>% 
    rename(a = `I(positive)`, b = `I(positive^2)`) %>% 
    mutate(
      A = -a / b,
      s = 1 / a
    ) 
  
  pred_grid <-
    stan_df %>% 
    data_grid(positive = seq_range(c(positive, 4 * max(positive)), n = 100)) %>% 
    filter(positive >= min(stan_df$positive))
  
  pred <-
    trace %>% 
    merge(pred_grid) %>% 
    mutate(y_pred = rnorm(n(), a * positive + b * positive ^ 2, sigma))
  
  pred_quantile <-
    pred %>% 
    group_by(positive) %>% 
    summarize(
      LowPred2 = max(quantile(y_pred, .01), 0),
      LowPred1 = max(quantile(y_pred, .1), 0),
      MedianPred = max(median(y_pred), 0),
      HighPred1 = max(quantile(y_pred, .9), 0),
      HighPred2 = max(quantile(y_pred, .99), 0)
    ) 
  
  ggplot() +
    geom_ribbon(aes(x = positive, ymin = LowPred2, ymax = HighPred2), color = 'gray', data = pred_quantile, alpha = .25) +
    geom_ribbon(aes(x = positive, ymin = LowPred1, ymax = HighPred1), color = 'gray', data = pred_quantile, alpha = .25) +
    geom_line(aes(x = positive, y = MedianPred), color = 'red', data = pred_quantile) +
    geom_point(aes(x = positive, y = positiveIncrease), data = stan_df) +
    xlim(0, 4 * max(stan_df$positive)) +
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
          'State Statistics',
          fluidRow(
            column(
              3,
              h3('Tests'),
              plotOutput('total', height = plot_height),
              plotOutput('total_daily', height = plot_height)
            ),
            column(
              3,
              h3('Cases'),
              plotOutput('positive', height = plot_height),
              plotOutput('positive_daily', height = plot_height)
              # plotOutput('phase', height = plot_height)
            ),
            column(
              3,
              h3('Hospitalizations'),
              plotOutput('hospitalized', height = plot_height),
              plotOutput('hospitalized_daily', height = plot_height)
            ),
            column(
              3,
              h3('Deaths'),
              plotOutput('death', height = plot_height),
              plotOutput('death_daily', height = plot_height)
            )
          )
        ),
        tabPanel(
          'Growth Analysis',
          fluidRow(
            # column(
            #   3,
            #   h3('Current Cases Pending Outcome'),
            #   plotOutput('current', height = plot_height),
            #   plotOutput('current_daily', height = plot_height)
            # ),
            column(
              4,
              h3('New Cases vs Cumulative Cases'),
              plotOutput('phase', height = plot_height),
              h3('Normalized Growth vs Total'),
              plotOutput('growth_characteristic', height = plot_height)
            ),
            column(
              4,
              h3('Ratio of New Cases to Previous New Cases'),
              plotOutput('gf', height = plot_height)
            ),
            column(
              4,
              h3('Growth Factor Overview'),
              plotOutput('gf_overview', height = '600px')
            )
            # column(
            #   4,
            #   h3('New Cases vs Cumulative Cases Model Estimate'),
            #   plotOutput('phase_model', height = plot_height)
            # )
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
      filter(positive > 50, positiveIncrease > 0)
    
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
    
    df %>% 
      ggplot() +
      geom_line(aes(x = positive, y = positiveIncrease, group = state, alpha = state == input$state)) +
      scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .1)) +
      { if (input$logscale) scale_x_log10() else scale_x_continuous(limits = xrange) } +
      { if (input$logscale) scale_y_log10() else scale_y_continuous(limits = yrange) } +
      # scale_x_log10() +
      # scale_y_log10() +
      theme_minimal() +
      theme(legend.position = 'none')
    
  })
  
  output$growth_characteristic <- renderPlot({
    
    covid_data() %>% 
      filter(state == input$state) %>% 
      group_by(state) %>% 
      filter(positive > .1 * max(positive)) %>% 
      ungroup() %>% 
      ggplot(aes(x = positive, y = positiveIncrease / positive)) +
      geom_line() +
      geom_hline(aes(yintercept = 0)) +
      facet_wrap(~ state, scales = 'free') +
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
  
  # output$phase_model <- renderPlot({
  #   
  #   covid_data() %>% 
  #     phase_model_chart(input$state)
  #   
  # })
  
  # output$current <- renderPlot({
  #   
  #   covid_data() %>% 
  #     arrange(date) %>% 
  #     group_by(state) %>% 
  #     mutate(
  #       current_cases = positive - death - recovered,
  #       current_cases_increase = c(0, diff(current_cases))
  #     ) %>% 
  #     ungroup() %>% 
  #     cumulative_chart(input$state, current_cases, logscale = input$logscale)
  #   
  # })
  # 
  # output$current_daily <- renderPlot({
  #   
  #   covid_data() %>% 
  #     arrange(date) %>% 
  #     group_by(state) %>% 
  #     mutate(
  #       current_cases = positive - death - recovered,
  #       current_cases_increase = c(0, diff(current_cases))
  #     ) %>% 
  #     ungroup() %>% 
  #     daily_chart(input$state, current_cases_increase)
  #   
  # })
  
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