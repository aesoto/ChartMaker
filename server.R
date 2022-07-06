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
library(httr)
library(jsonlite)
library(xts)
source('FSquery.R')
fredr_set_key('dad071a33ef9414bb0a759835d9ec507')
 

shinyServer(function(input, output) {
  
  
  ## First Tab: S&P - Valuation
  
  timeSeries_sp_val <- reactive({
    
    days <- input$end_sp_val - input$start_sp_val
    
    metricName2 <- ifelse(input$metric_SP_valuation == "S&P500", "SP500",
                          ifelse(input$metric_SP_valuation == "PE - LTM", paste("FMA_PE(LTMA,-", days,"D,0,D)", sep=""),
                                 ifelse(input$metric_SP_valuation == "PE - NTM", paste("FG_PE_NTM(-",days,"D,0,D,90,0)", sep=""),
                                        ifelse(input$metric_SP_valuation == "Eqty Prem", "Eqty Prem",NULL))))
    
    
    
    if(input$metric_SP_valuation == "S&P500"){
      #timeSeries <- fredr(metricName2) %>%
      #drop_na() %>%
      # select(1,3) %>%
      # rename(Close = 2)
      timeSeries <- fredr(metricName2, 
                          observation_start = input$start_sp_val,
                          observation_end = input$end_sp_val) %>%
        drop_na() %>%
        select(1,3) %>%
        rename(Close = 2)
    }
    
    if(input$metric_SP_valuation == "Eqty Prem"){
      
      timeSeries1 <-  FSquery("SP50", paste("FMA_PE(LTMA,-", days,"D,0,D)", sep="")) %>%
        drop_na() %>%
        select(1,2) %>%
        rename(Close = 2)
      
      timeSeries1 <- 1/as_tibble(timeSeries1)
      
      
      timeSeries1 <-  FSquery("SP50", paste("FMA_PE(LTMA,-", days,"D,0,D)", sep="")) %>%
        drop_na() %>%
        select(1,2) %>%
        rename(Close = 2)
      
      timeSeries1 <- 1/as_tibble(timeSeries1)
      
      
      timeSeries2 <- fredr('DFII10', 
                          observation_start = input$start_sp_val,
                          observation_end = input$end_sp_val) %>%
        drop_na() %>%
        select(1,3) %>%
        rename(Close = 2)
      
      timeSeries2 <- xts(timeSeries2[,2], order.by = timeSeries2$date)
      
      
      timeSeries <- timeSeries1/timeSeries2
      
      }
    
    if(input$metric_SP_valuation != c("S&P500","Eqty Prem")){
      timeSeries <-  FSquery("SP50", metricName2) %>%
        drop_na() %>%
        select(1,2) %>%
        rename(Close = 2)
      timeSeries <- as_tibble(timeSeries)
    }
    
    
    
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
  })
  
  output$distPlot_sp_val <- renderPlot({
    
    days <- input$end_sp_val - input$start_sp_val
    
    metricName <- ifelse(input$metric_SP_valuation == "S&P500", "S&P500 - Valuation",
                         ifelse(input$metric_SP_valuation == "PE - LTM", "PE - LTM",
                                ifelse(input$metric_SP_valuation == "PE - NTM", "PE - NTM",
                                       ifelse(input$metric_SP_valuation == "Eqty Prem", "Eqty Prem",NULL ))))
    #ifelse(input$metric_SP_valuation == "PE - NTM", paste("FG_PE_NTM(-", days,"D,0,D,90,0)", sep=""),NULL)))
    
    
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
      
      
      max <- paste("Max.:", max)  
      min <- paste("Min.:", min)
      mean <- paste("Avg.:", mean)
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
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      if(input$end_sp_val - input$start_sp_val >= 365){
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      }else{change_1year <-  paste("1Y Change:", "NA")}
      
      text <- paste(max, min, mean, change, change_1year, sep = "     ")
      
      
      mtext(text, side=1, outer=FALSE, cex = 0.75)
      
    }
    
    plot()
    
    
  })
  
  
  
  ## Second Tab: S&P - Growth
  
  timeSeries_sp_growth <- reactive({
    # FG_SALES_1YGR(-40D,0,0)
    
    days <- input$end_sp_growth-input$start_sp_growth
    
    metricName2 <- ifelse(input$metric_SP_growth == "EV/Sales-LTM", paste("FMA_EVAL_SALES(LTMA,0,-", days,"D,)", sep = ""),
                          ifelse(input$metric_SP_growth == "EV/Sales-NTM", paste("FMA_EVAL_SALES(NTMA,0,-", days, "D,D)", sep = ""),
                                 ifelse(input$metric_SP_growth == "EPS-LTM", paste("FMA_EPS(LTM,0,-", days, "D)", sep = ""),
                                        ifelse(input$metric_SP_growth == "EPS-NTM", paste("FMA_EPS(NTMA,0,-", days,"D,D)", sep= ""),NULL))))
    
    timeSeries <-  FSquery("SP50", metricName2) %>%
      drop_na() %>%
      select(1,2) %>%
      rename(Close = 2)
    
    timeSeries <- as_tibble(timeSeries)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    
    
  })
  
  
  output$distPlot_sp_growth <- renderPlot({
    
    
    days <- input$end_sp_growth - input$start_sp_growth
    
    metricName <- ifelse(input$metric_SP_growth == "EV/Sales-LTM", "EV/Sales - LTM",
                         ifelse(input$metric_SP_growth == "EV/Sales-NTM", "EV/Sales - NTM",
                                ifelse(input$metric_SP_growth == "EPS-LTM", "EPS - LTM",
                                       ifelse(input$metric_SP_growth == "EPS-NTM", "EPS - NTM"))))
    
    timeSeries <- timeSeries_sp_growth()
    
    plot <- function(){
      
      chartSeries(timeSeries,
                  name = metricName,
                  type="line",
                  theme=chartTheme('white'))
      
      
      
      
      max <- round(max(timeSeries$Close),1)
      min <- round(min(timeSeries$Close),1)
      mean <- round(mean(timeSeries$Close),1)
      
      
      max <- paste("Max.:", max)  
      min <- paste("Min.:", min)
      mean <- paste("Avg.:", mean)
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
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      if(input$end_sp_growth - input$start_sp_growth >= 365){
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      }else{change_1year <-  paste("1Y Change:", "NA")}
      
      text <- paste(max, min, mean, change, change_1year, sep = "     ")
      
      mtext(text, side=1, outer=FALSE, cex = 0.75)
      
    }
    
    plot()
    
  })
  
  
  
  
  
  
  
  ## Third Tab: Rates
  
  timeSeries_rates <- reactive({
    
    metricName2 <- ifelse(input$metric_rates == "3mo Tbill", "DTB3",
                          ifelse(input$metric_rates == "2-10 Spread", 'T10Y2Y',
                                 ifelse(input$metric_rates == "30Y Mortgage", 'MORTGAGE30US',
                                        ifelse(input$metric_rates == "10Yr Yield",'DGS10',
                                               ifelse(input$metric_rates == 'High Yield' ,"BAMLH0A0HYM2EY",NULL)))))
    
    
    timeSeries <- fredr(metricName2, 
                        observation_start = input$start_rates,
                        observation_end = input$end_rates) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    
  })
  
  
  
  
  output$distPlot_rates <- renderPlot({
    
    
    
    metricName <- ifelse(input$metric_rates == "3mo Tbill", "3mo Tbill", 
                         ifelse(input$metric_rates == "2-10 Spread", "2-10 Spread",
                                ifelse(input$metric_rates == "30Y Mortgage", "30Y Mortgage",
                                       ifelse(input$metric_rates == "10Yr Yield", "10Yr Yield", 
                                              ifelse(input$metric_rates == 'High Yield' ,'High Yield',NULL)))))
    
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
      
      
      max <- paste("Max.:", max)  
      min <- paste("Min.:", min)
      mean <- paste("Avg.:", mean)
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
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      if(input$end_rates - input$start_rates >= 365){
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      }else{change_1year <-  paste("1Y Change:", "NA")}
      
      text <- paste(max, min, mean, change, change_1year, sep = "     ")
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
                                                     ifelse(input$metric_inflation == "PPIACO",  "PPI",
                                                            ifelse(input$metric_inflation == "UMCSENT", "Consumer Sentiment",
                                                                   ifelse(input$metric_inflation == "MICH", "Consumer Expectations",NULL))))))))
    
    
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
                                                     ifelse(input$metric_inflation == "PPIACO",  "PPI",
                                                            ifelse(input$metric_inflation == "UMCSENT", "Consumer Sentiment",
                                                                   ifelse(input$metric_inflation == "MICH", "Consumer Expectations",NULL))))))))
    
    
    timeSeries <- timeSeries_inflation()
    
    
    plot <- function(){
      
      chartSeries(timeSeries,
                  name = metricName,
                  type="line",
                  theme=chartTheme('white'))
      
      
      max <- round(max(timeSeries$Close),1)
      min <- round(min(timeSeries$Close),1)
      mean <- round(mean(timeSeries$Close),1)
      
      
      max <- paste("Max.:", max)  
      min <- paste("Min.:", min)
      mean <- paste("Avg.:", mean)
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
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      if(input$end_inflation - input$start_inflation >= 365){
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      }else{change_1year <-  paste("1Y Change:", "NA")}
      
      text <- paste(max, min, mean, change, change_1year, sep = "     ")
      
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
      
      
      max <- paste("Max.:", max)  
      min <- paste("Min.:", min)
      mean <- paste("Avg.:", mean)
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
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      if(input$end_currency - input$start_currency >= 365){
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      }else{change_1year <-  paste("1Y Change:", "NA")}
      
      text <- paste(max, min, mean, change, change_1year, sep = "     ")
      
      mtext(text, side=1, outer=FALSE, cex = 0.75)
    }
    
    plot()
  })
  
  ## sixth Tab: Liquidity
  
  
  timeSeries_liquidity <- reactive({
    
    
    timeSeries <- fredr(input$metric_liquidity, 
                        observation_start = input$start_liquidity,
                        observation_end = input$end_liquidity) %>%
      drop_na() %>%
      select(1,3) %>%
      rename(Close = 2)
    
    timeSeries <- xts(timeSeries[,2], order.by = timeSeries$date)
    
    
  })
  
  
  
  
  output$distPlot_liquidity <- renderPlot({
    
    metricName <- ifelse(input$metric_liquidity == "EFFR", "FF Rates", 
                         ifelse(input$metric_liquidity == "DPCREDIT", "Discount Rate",
                                ifelse(input$metric_liquidity == "TERMCBPER24NS", "Personal Loans",
                                       ifelse(input$metric_liquidity == "DPRIME", "Prime Rate",NULL))))
    
    
    timeSeries <-timeSeries_liquidity()
    
    plot <- function(){
      
      chartSeries(timeSeries,
                  name = metricName,
                  type="line",
                  theme=chartTheme('white'))
      
      
      max <- round(max(timeSeries$Close),1)
      min <- round(min(timeSeries$Close),1)
      mean <- round(mean(timeSeries$Close),1)
      
      
      max <- paste("Max.:", max)  
      min <- paste("Min.:", min)
      mean <- paste("Avg.:", mean)
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
      
      change <- paste("Period Change:", change, "%")
      
      ############################################################################
      ## one year
      
      max_date <- max(timeSeries$date)
      one_year <- max_date -365
      
      one_year <- subset(timeSeries, timeSeries$date >=  one_year)
      
      one_year <- subset(one_year, one_year$date == min(one_year$date) | one_year$date == max(one_year$date))
      
      change_1year <- (one_year$Close[2] - one_year$Close[1])/one_year$Close[1]
      change_1year*100
      
      if(input$end_liquidity - input$start_liquidity >= 365){
        change_1year <-  paste("1Y Change:", round(change_1year*100,2), "%")
      }else{change_1year <-  paste("1Y Change:", "NA")}
      
      text <- paste(max, min, mean, change, change_1year, sep = "     ")
      
      mtext(text, side=1, outer=FALSE, cex = 0.75)
    }
    
    plot()
  })
  
})
