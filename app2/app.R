
library(shiny)
library(tidyverse)
# library(brms)
# library(rstanarm)
library(arm)
library(lubridate)
library(tidybayes)
library(modelr)

get_covid_data <- function() {
  
  results <-
    vroom::vroom('https://covidtracking.com/api/states/daily.csv') %>% 
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
  titlePanel('COVID-19 Dashboard'),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('state', 'State', choices = sort(unique(get_covid_data()$state)), selected = 'USA'),
      checkboxInput('logscale', 'Log Scale', value = FALSE),
      dateRangeInput('daterange', 'Date Range', start = Sys.Date() - 30, end = Sys.Date()),
      div(HTML('Data from <a>covidtracking.com</a>. Modeling assumes logistic growth.')),
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
            column(
              6,
              h3('New Cases vs Cumulative Cases'),
              plotOutput('phase', height = '600px')
            ),
            column(
              6,
              h3('Ratio of New Cases to Previous New Cases'),
              plotOutput('gf', height = '280px'),
              h3('New Cases vs Cumulative Cases Model Estimate'),
              plotOutput('phase_model', height = '280px')
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
    
    covid_data() %>% 
      filter(positive > 50, positiveIncrease > 0) %>% 
      ggplot() +
      geom_line(aes(x = positive, y = positiveIncrease, group = state, alpha = state == input$state)) +
      scale_alpha_manual(values = c('TRUE' = 1, 'FALSE' = .1)) +
      scale_x_log10() +
      scale_y_log10() +
      theme_minimal() +
      theme(legend.position = 'none')
    
  })
  
  output$gf <- renderPlot({
    
    covid_data() %>% 
      filter(positive > 50, positiveIncrease > 0, state == input$state) %>% 
      arrange(date) %>% 
      mutate(growth_factor = positiveIncrease / lag(positiveIncrease)) %>% 
      ggplot() +
      geom_line(aes(x = date, y = growth_factor)) +
      geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
      scale_y_continuous(expand = expand_scale(add = 1)) +
      theme_minimal()     
    
  })
  
  output$phase_model <- renderPlot({
    
    covid_data() %>% 
      phase_model_chart(input$state)
    
  })
  
  
}

shinyApp(ui, server)