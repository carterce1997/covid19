
library(shiny)
library(tidyverse)

get_covid_data <- function() {
  
  results <-
    vroom::vroom('https://covidtracking.com/api/states/daily.csv') %>% 
    mutate(date = ymd(date)) %>% 
    replace(is.na(.), 0)
  
  return(results)
  
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
  titlePanel('COVID-19 Dashboard'),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('state', 'State', choices = sort(unique(get_covid_data()$state)), selected = 'NY'),
      checkboxInput('logscale', 'Log Scale', value = FALSE),
      dateRangeInput('daterange', 'Date Range', start = Sys.Date() - 14, end = Sys.Date()),
      width = 2
    ),
    mainPanel(
      fluidRow(
        column(
          3,
          h2('Tests'),
          plotOutput('total', height = plot_height),
          plotOutput('total_daily', height = plot_height)
        ),
        column(
          3,
          h2('Cases'),
          plotOutput('positive', height = plot_height),
          plotOutput('positive_daily', height = plot_height)
        ),
        # column(
        #   2,
        #   plotOutput('hospitalized', height = plot_height),
        #   plotOutput('hospitalized_daily', height = plot_height)
        # ),
        column(
          3,
          h2('Deaths'),
          plotOutput('death', height = plot_height),
          plotOutput('death_daily', height = plot_height)
        ),
        column(
          3,
          h2('Phase Plot'),
          plotOutput('phase', height = '600px')
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
      cumulative_chart(input$state, total, input$logscale)
    
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
  
  
}

shinyApp(ui, server)