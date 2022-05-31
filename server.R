library(lubridate)
library(quantmod)
library(tidyverse)
library(highcharter)
library(tidyquant)
library(timetk)
library(shinyWidgets)
library(shiny)
library(fredr)
library(here)
fredr_set_key('dad071a33ef9414bb0a759835d9ec507')


shinyServer(function(input, output) {
  
  
  ## First Tab: S&P - Valuation
  
  output$distPlot_sp_val <- renderPlot({
    
    if (input$metric_SP_valuation == "SP500"){metricName <- "S&P500 - Valuation"}
    
    timeSeries <- fredr(input$metric_SP_valuation, 
                        observation_start = input$start_sp_val,
                        observation_end = input$end_sp_val) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
  })
  
  
  ## Second Tab: S&P - Growth
  
  output$distPlot_SP_growth <- renderPlot({
    
    metricName <- ifelse(input$metric_SP_growth == "A", "Sales PS ", 
                         ifelse(input$metric_SP_growth == "B", "Sales Growth",
                                ifelse(input$metric_SP_growth == "C", "EPS- NTM",
                                       ifelse(input$metric_SP_growth == "D", "EPS",NULL))))
    
    timeSeries <- fredr(input$metric_SP_growth, 
                        observation_start = input$start_sp_growth,
                        observation_end = input$end_sp_growth) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
  })
  

 
  ## Third Tab: Rates
  
  output$distPlot_rates <- renderPlot({
    
    
    
    metricName <- ifelse(input$metric_rates == "DTB3", "3mo Tbill", 
                         ifelse(input$metric_rates == 'T10Y2Y', "2-10 Spread",
                                ifelse(input$metric_rates == 'MORTGAGE30US', "30Y Mortgage",
                                       ifelse(input$metric_rates == 'DGS10', "10Yr Yield", NULL))))
    
    
    timeSeries <- fredr(input$metric_rates, 
                        observation_start = input$start_rates,
                        observation_end = input$end_rates) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
  })
  
  ## Fourth Tab: Inflation
  
  output$distPlot_inflation <- renderPlot({
    
    
    
    metricName <- ifelse(input$metric_inflation == 'DFII10', "10Y TIPS", 
                         ifelse(input$metric_inflation == 'T10YIE', "10Y Breakeven Inflation",
                                ifelse(input$metric_inflation == 'T5YIE', "5Y Breakeven Inflation",
                                       ifelse(input$metric_inflation == 'T5YIFR', "5,5 Forward Inflation",
                                              ifelse(input$metric_inflation  == "CPIAUCSL", "CPI",
                                                     ifelse(input$metric_inflation == "PPIACO",  "PPI",NULL))))))
    
    
    timeSeries <- fredr(input$metric_inflation, 
                        observation_start = input$start_inflation,
                        observation_end = input$end_inflation) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
  })
  
})
