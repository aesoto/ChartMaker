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



shinyUI(fluidPage(
  
  titlePanel("Market Dashboard"),
  
  
  tabsetPanel(
    
    ## First Tab: S&P - Valuation
    
    
    tabPanel("S&P500 - Valuation",
             
             tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
             
             fluidRow(
               column(2,
                      awesomeRadio(
                        inputId = "metric_SP_valuation",
                        label = "Metric",
                        choices = c("S&P500" = 'S&P500' ,
                                    "PE - LTM" = "PE - LTM",
                                    "PE - NTM"= "PE - NTM"),
                        selected = "S&P500",
                        status = "warning"
                      ),
                      dateInput("start_sp_val",
                                "Start Date",
                                Sys.Date() - years(1),
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_sp_val",
                                "End Date",
                                Sys.Date(),
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_sp_val').print();")
               ),
               
               
               column(width = 10,
                      br(),
                      br(),
                      plotOutput('distPlot_sp_val')
               )
             )
    ),
    
    
    ## Second Tab: S&P - Growth
    
    
    tabPanel("S&P500 - Growth",
             
             tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
             
             fluidRow(
               column(2,
                      awesomeRadio(
                        inputId = "metric_SP_growth",
                        label = "Metric",
                        choices = c("EV/Sales-LTM" = "EV/Sales-LTM",
                                    "EV/Sales-NTM" = "EV/Sales-NTM",
                                    "EPS-LTM" = "EPS-LTM" ,
                                    "EPS-NMT" = "EPS-NMT"),
                        selected = "EV/Sales-LTM",
                        status = "warning"
                      ),
                      dateInput("start_sp_growth",
                                "Start Date",
                                Sys.Date() - years(1),
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_sp_growth",
                                "End Date",
                                Sys.Date(),
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_sp_growth').print();")
               ),
               column(10,
                      br(),
                      br(),
                      plotOutput('distPlot_sp_growth')
               )
             )
    ),
    
    
    
    ## Third Tab: Rates
    
    tabPanel("Rates",
             
             tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
             
             fluidRow(
               column(2,
                      awesomeRadio(
                        inputId = "metric_rates",
                        label = "Metric",
                        choices = c("3mo Tbill" = "3mo Tbill",
                                    "2-10 Spread" = "2-10 Spread",
                                    "10Yr Yield" =  "10Yr Yield",
                                    "30Y Mortgage" = "30Y Mortgage",
                                    "High Yield" = "High Yield"),
                        
                        selected = "3mo Tbill",
                        status = "warning"
                      ),
                      dateInput("start_rates",
                                "Start Date",
                                Sys.Date() - years(1),
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_rates",
                                "End Date",
                                Sys.Date(),
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_rates').print();")
               ),
               column(10,
                      br(),
                      br(),
                      plotOutput('distPlot_rates')
               )
             )
             
    ),
    
    ## Fourth Tab: Inflation
    
    tabPanel("Inflation",
             
             tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
             
             fluidRow(
               column(2,
                      awesomeRadio(
                        inputId = "metric_inflation",
                        label = "Metric",
                        choices = c( "5Y Breakeven Inf." ='T5YIE',
                                     "10Y Breakeven Inf." ='T10YIE',
                                     "10Y TIPS" = 'DFII10',
                                     "5,5 Forward Inf."= 'T5YIFR',
                                     "CPI" = "CPIAUCSL",
                                     "PPI" = "PPIACO",
                                     "Consumer Sent." = "UMCSENT",
                                     "Consumer Exp." = "MICH"),
                        selected = "DFII10",
                        status = "warning"
                      ),
                      dateInput("start_inflation",
                                "Start Date",
                                Sys.Date() - years(1),
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_inflation",
                                "End Date",
                                Sys.Date(),
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_inflation').print();")
               ),
               column(10,
                      br(),
                      br(),
                      plotOutput('distPlot_inflation')
               )
             )
    ),
    
    
    ## Fifth Tab: Currency
    
    tabPanel("Currency",
             
             tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
             
             fluidRow(
               column(2,
                      awesomeRadio(
                        inputId = "metric_currency",
                        label = "Metric",
                        choices = c( "USD Index-Adv" ='DTWEXAFEGS',
                                     "USD Index-Broad" ='DTWEXBGS'),
                        selected = "DTWEXAFEGS",
                        status = "warning"
                      ),
                      dateInput("start_currency",
                                "Start Date",
                                Sys.Date() - years(1),
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_currency",
                                "End Date",
                                Sys.Date(),
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_currency').print();")
               ),
               column(10,
                      br(),
                      br(),
                      plotOutput('distPlot_currency')
               )
             )
    ),
    
    # sixth tab:
    
    tabPanel("Liquidity",
             
             tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
             
             fluidRow(
               column(2,
                      awesomeRadio(
                        inputId = "metric_liquidity",
                        label = "Metric",
                        choices = c( "FF Rates" ="EFFR",
                                     "Discount Rate" = "DPCREDIT",
                                     "Personal Loans" = "TERMCBPER24NS",
                                     "Prime Rate" = "DPRIME"),
                        selected = "EFFR",
                        status = "warning"
                      ),
                      dateInput("start_liquidity",
                                "Start Date",
                                Sys.Date() - years(1),
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_liquidity",
                                "End Date",
                                Sys.Date(),
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_liquidity').print();")
               ),
               column(10,
                      br(),
                      br(),
                      plotOutput('distPlot_liquidity')
               )
             )
    )
    
    
  )
)
)
