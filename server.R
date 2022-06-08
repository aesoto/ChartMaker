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
  
  timeSeries_sp_val <- reactive({
    
    if (input$metric_SP_valuation == "SP500"){metricName <- "S&P500 - Valuation"}
    
    timeSeries <- fredr(input$metric_SP_valuation, 
                        observation_start = input$start_sp_val,
                        observation_end = input$end_sp_val) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    
  })
  
  output$distPlot_sp_val <- renderPlot({
    
    if (input$metric_SP_valuation == "SP500"){metricName <- "S&P500 - Valuation"}
    
    timeSeries <- timeSeries_sp_val()
    
    plot <- function(){
    
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
      ### Text
      
      max <- round(max(timeSeries$Close),1)
      min <- round(min(timeSeries$Close),1)
      mean <- round(mean(timeSeries$Close),1)
      
      
      max <- paste("Maximum:", max)  
      min <- paste("Minimum:", min)
      mean <- paste("Average:", mean)
      #######################################################
      
      timeSeries <- as.data.frame(timeSeries)
      timeSeries$date <- as_date(row.names(timeSeries))
      
      max_date <- max(timeSeries$date)
      min_date <- min(timeSeries$date)
      
      ### Difference selected period
      
      one <- subset(timeSeries, timeSeries$date == max_date | timeSeries$date == min_date)
      
      period <- max_date - min_date
      period
      
      change <- (one$Close[2] - one$Close[1]) / one$Close[1]
      change <- round(change*100,2)
      
      change <- paste("Selected period change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      change_1year <-  paste("Change 1 year:", round(change_1year*100,2), "%")
      
      text <- paste(max, min, mean, change, change_1year)
      
      
      mtext(text, side=1, outer=FALSE, cex = 0.75)
    
    }
    
    plot()
    
    
  })
  
  
  
  ## Second Tab: S&P - Growth
  
  timeSeries_sp_growth <- reactive({
    
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
    
    
    
  })
  
  
  output$distPlot_SP_growth <- renderPlot({
    
    timeSeries <- timeSeries_sp_growth()
    
    metricName <- ifelse(input$metric_SP_growth == "A", "Sales PS ", 
                         ifelse(input$metric_SP_growth == "B", "Sales Growth",
                                ifelse(input$metric_SP_growth == "C", "EPS- NTM",
                                       ifelse(input$metric_SP_growth == "D", "EPS",NULL))))
    plot <- function(){
    
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
   
    
    max <- round(max(timeSeries$Close),1)
    min <- round(min(timeSeries$Close),1)
    mean <- round(mean(timeSeries$Close),1)
    
    
    max <- paste("Maximum:", max)  
    min <- paste("Minimum:", min)
    mean <- paste("Average:", mean)
    #######################################################
    
    timeSeries <- as.data.frame(timeSeries)
    timeSeries$date <- as_date(row.names(timeSeries))
    
    max_date <- max(timeSeries$date)
    min_date <- min(timeSeries$date)
    
    ### Difference selected period
    
    one <- subset(timeSeries, timeSeries$date == max_date | timeSeries$date == min_date)
    
    period <- max_date - min_date
    period
    
    change <- (one$Close[2] - one$Close[1]) / one$Close[1]
    change <- round(change*100,2)
    
    change <- paste("Selected period change:", change, "%")
    
    ############################################################################
    ## one year
    
    max_date <- max(timeSeries$date)
    one_year <- max_date -365
    
    one_year <- subset(timeSeries, timeSeries$date >=  one_year)
    
    one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
    
    change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
    change_1year*100
    
    change_1year <-  paste("Change 1 year:", round(change_1year*100,2), "%")
    
    text <- paste(max, min, mean, change, change_1year)
    
    mtext(text, side=1, outer=FALSE, cex = 0.75)
    
    }
    
    plot()
    
  })
  
  
  
  
  

 
  ## Third Tab: Rates
  
  timeSeries_rates <- reactive({
    
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
    
    
  })
  
  
  
  
  output$distPlot_rates <- renderPlot({
    
    
    
    metricName <- ifelse(input$metric_rates == "DTB3", "3mo Tbill", 
                         ifelse(input$metric_rates == 'T10Y2Y', "2-10 Spread",
                                ifelse(input$metric_rates == 'MORTGAGE30US', "30Y Mortgage",
                                       ifelse(input$metric_rates == 'DGS10', "10Yr Yield", NULL))))
    
    
    timeSeries <- timeSeries_rates()
    
    plot <- function(){
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
  
    timeSeries <- timeSeries_rates()
    
    max <- round(max(timeSeries$Close),1)
    min <- round(min(timeSeries$Close),1)
    mean <- round(mean(timeSeries$Close),1)
    
    
    max <- paste("Maximum:", max)  
    min <- paste("Minimum:", min)
    mean <- paste("Average:", mean)
    #######################################################
    
    timeSeries <- as.data.frame(timeSeries)
    timeSeries$date <- as_date(row.names(timeSeries))
    
    max_date <- max(timeSeries$date)
    min_date <- min(timeSeries$date)
    
    ### Difference selected period
    
    one <- subset(timeSeries, timeSeries$date == max_date | timeSeries$date == min_date)
    
    period <- max_date - min_date
    period
    
    change <- (one$Close[2] - one$Close[1]) / one$Close[1]
    change <- round(change*100,2)
    
    change <- paste("Selected period change:", change, "%")
    
    ############################################################################
    ## one year
    
    max_date <- max(timeSeries$date)
    one_year <- max_date -365
    
    one_year <- subset(timeSeries, timeSeries$date >=  one_year)
    
    one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
    
    change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
    change_1year*100
    
    change_1year <-  paste("Change 1 year:", round(change_1year*100,2), "%")
    
    text <- paste(max, min, mean, change, change_1year)
    mtext(text, side=1, outer=FALSE, cex = 0.75)
    
  }
  
  plot()
    
  })
  
  
  
  
  
  ## Fourth Tab: Inflation
  
  
  timeSeries_inflation <- reactive({
    
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
    
    
  })
  
  
  
  
  output$distPlot_inflation <- renderPlot({
    
    
    
    metricName <- ifelse(input$metric_inflation == 'DFII10', "10Y TIPS", 
                         ifelse(input$metric_inflation == 'T10YIE', "10Y Breakeven Inflation",
                                ifelse(input$metric_inflation == 'T5YIE', "5Y Breakeven Inflation",
                                       ifelse(input$metric_inflation == 'T5YIFR', "5,5 Forward Inflation",
                                              ifelse(input$metric_inflation  == "CPIAUCSL", "CPI",
                                                     ifelse(input$metric_inflation == "PPIACO",  "PPI",NULL))))))
    
    
    timeSeries <- timeSeries_inflation()
   
    
    plot <- function(){
     
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
    max <- round(max(timeSeries$Close),1)
    min <- round(min(timeSeries$Close),1)
    mean <- round(mean(timeSeries$Close),1)
    
    
    max <- paste("Maximum:", max)  
    min <- paste("Minimum:", min)
    mean <- paste("Average:", mean)
    #######################################################
    
    timeSeries <- as.data.frame(timeSeries)
    timeSeries$date <- as_date(row.names(timeSeries))
    
    max_date <- max(timeSeries$date)
    min_date <- min(timeSeries$date)
    
    ### Difference selected period
    
    one <- subset(timeSeries, timeSeries$date == max_date | timeSeries$date == min_date)
    
    period <- max_date - min_date
    period
    
    change <- (one$Close[2] - one$Close[1]) / one$Close[1]
    change <- round(change*100,2)
    
    change <- paste("Selected period change:", change, "%")
    
    ############################################################################
    ## one year
    
    max_date <- max(timeSeries$date)
    one_year <- max_date -365
    
    one_year <- subset(timeSeries, timeSeries$date >=  one_year)
    
    one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
    
    change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
    change_1year*100
    
    change_1year <-  paste("Change 1 year:", round(change_1year*100,2), "%")
    
    text <- paste(max, min, mean, change, change_1year)
    
    mtext(text, side=1, outer=FALSE, cex = 0.75)
    
    }
    
    plot()
  })
  
  
  
  
  
  
  ## Fifth Tab: Currency
  
  
  timeSeries_currency <- reactive({
    
    metricName <- ifelse(input$metric_currency == 'DTWEXAFEGS', "USD Index-Adv", 
                         ifelse(input$metric_currency == 'DTWEXBGS', "USD Index-Broad",NULL))
    
    
    timeSeries <- fredr(input$metric_currency, 
                        observation_start = input$start_currency,
                        observation_end = input$end_currency) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    
  })
  
  
  
  
  output$distPlot_currency <- renderPlot({
    
    
    
    metricName <- ifelse(input$metric_currency == 'DTWEXAFEGS', "USD Index-Adv", 
                         ifelse(input$metric_currency == 'DTWEXBGS', "USD Index-Broad",NULL))
    
    
    timeSeries <-timeSeries_currency()
 
    plot <- function(){
       
    chartSeries(timeSeries,
                name = metricName,
                type="line",
                theme=chartTheme('white'))
    
    
    max <- round(max(timeSeries$Close),1)
    min <- round(min(timeSeries$Close),1)
    mean <- round(mean(timeSeries$Close),1)
    
    
    max <- paste("Maximum:", max)  
    min <- paste("Minimum:", min)
    mean <- paste("Average:", mean)
    #######################################################
    
    timeSeries <- as.data.frame(timeSeries)
    timeSeries$date <- as_date(row.names(timeSeries))
    
    max_date <- max(timeSeries$date)
    min_date <- min(timeSeries$date)
    
    ### Difference selected period
    
    one <- subset(timeSeries, timeSeries$date == max_date | timeSeries$date == min_date)
    
    period <- max_date - min_date
    period
    
    change <- (one$Close[2] - one$Close[1]) / one$Close[1]
    change <- round(change*100,2)
    
    change <- paste("Selected period change:", change, "%")
    
    ############################################################################
    ## one year
    
    max_date <- max(timeSeries$date)
    one_year <- max_date -365
    
    one_year <- subset(timeSeries, timeSeries$date >=  one_year)
    
    one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
    
    change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
    change_1year*100
    
    change_1year <-  paste("Change 1 year:", round(change_1year*100,2), "%")
    
    text <- paste(max, min, mean, change, change_1year)
    
    mtext(text, side=1, outer=FALSE, cex = 0.75)
    }
    
    plot()
  })
 
  
})
