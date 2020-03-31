
library(shiny)
library(tidyverse)

get_covid_data <- function() {
  
  results <-
    vroom::vroom('https://covidtracking.com/api/states.csv') 
  
}

cumulative_chart <- function(covid_data, state_, variable, logscale) {
  
  variable <-
    enquo(variable)
  
  covid_data %>% 
    filter(state == state_) %>% 
    ggplot() +
    geom_line(aes(x = date, y = !!variable)) +
    { if (logscale) scale_y_log10() } +
    ggthemes::theme_hc()
  
}

daily_chart <- function(covid_data, state_, variable) {
  
  variable <-
    enquo(variable)
  
  covid_data %>% 
    filter(state == state_) %>% 
    ggplot() +
    geom_bar(aes(x = date, y = !!variable), stat = 'identity') +
    ggthemes::theme_hc()
  
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput('state', 'State', choices = sort(unique(get_covid_data()$state)), selected = 'USA'),
      checkboxInput('logscale', 'Log Scale', value = FALSE)
    ),
    mainPanel(
      fluidRow(
        column(
          3,
          plotOutput('total'),
          plotOutput('positive'),
          plotOutput('hospitalized'),
          plotOutput('death')
        ),
        column(
          3,
          plotOutput('total_daily'),
          plotOutput('positive_daily'),
          plotOutput('hospitalized_daily'),
          plotOutput('death_daily')
        ),
        column(
          6,
          plotOutput('phase')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  covid_data <- reactive({
    
    invalidateLater(5 * 60 * 1000)
    
    get_covid_data()
    
  })
  
  total <- plotOutput({
    
    covid_data() %>% 
      cumulative_chart(input$state, total, input$logscale)
    
  })
  
  positive <- plotOutput({
    
    covid_data() %>% 
      cumulative_chart(input$state, positive, input$logscale)
    
  })  
  
  hospitalized <- plotOutput({
    
    covid_data() %>% 
      cumulative_chart(input$state, hospitalized, input$logscale)
    
  })  
  
  death <- plotOutput({
    
    covid_data() %>% 
      cumulative_chart(input$state, death, input$logscale)
    
  })  
  
  total_daily <- plotOutput({
    
    covid_data() %>% 
      daily_chart(input$state, totalTestResultsIncrease)
    
  })
  
  positive_daily <- plotOutput({
    
    covid_data() %>% 
      daily_chart(input$state, positiveIncrease)
    
  })  
  
  hospitalized_daily <- plotOutput({
    
    covid_data() %>% 
      daily_chart(input$state, hospitalizedIncrease)
    
  })  
  
  death_daily <- plotOutput({
    
    covid_data() %>% 
      daily_chart(input$state, deathIncrease)
    
  })  
  
  phase <- plotOutput({
    
  })
  
  
}

shinyApp(ui, server)