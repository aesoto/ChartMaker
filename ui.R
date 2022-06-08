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
                        choices = c("S&P500" = 'SP500' ,
                                    "EV/Sales - LTM " ='.',
                                    "PE - LTM" ='.',
                                    "PE - NTM"= '.'),
                        selected = "SP500",
                        status = "warning"
                      ),
                      dateInput("start_sp_val",
                                "Start Date",
                                "2020-01-01",
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_sp_val",
                                "End Date",
                                "2022-04-18",
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_sp_val').print();")
               ),
               column(width = 8,
                      plotOutput('distPlot_sp_val'),
                      verbatimTextOutput("text_sp_val")
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
                        choices = c("Sales PS" = 'A' ,
                                    "Sales Growth" ='B',
                                    "EPS- NTM" ='C',
                                    "EPS"= 'D'),
                        selected = "A",
                        status = "warning"
                      ),
                      dateInput("start_sp_growth",
                                "Start Date",
                                "2020-01-01",
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_sp_growth",
                                "End Date",
                                "2022-04-18",
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_sp_growth').print();")
               ),
               column(10,
                      plotOutput('distPlot_sp_growth'),
                      verbatimTextOutput("text_sp_growth")
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
                        choices = c("3mo Tbill" = 'DTB3' ,
                                    "2-10 Spread" ='T10Y2Y',
                                    "10Yr Yield" =  'DGS10',
                                    "30Y Mortgage" ='MORTGAGE30US'),
                        
                        selected = "DTB3",
                        status = "warning"
                      ),
                      dateInput("start_rates",
                                "Start Date",
                                "2020-01-01",
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_rates",
                                "End Date",
                                "2022-04-18",
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_rates').print();")
               ),
               column(10,
                      plotOutput('distPlot_rates'),
                      verbatimTextOutput("text_rates")
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
                        choices = c( "5Y Breakeven Inflation" ='T5YIE',
                                     "10Y Breakeven Inflation" ='T10YIE',
                                     "10Y TIPS" = 'DFII10',
                                     "5,5 Forward Inflation"= 'T5YIFR',
                                     "CPI" = "CPIAUCSL",
                                     "PPI" = "PPIACO"),
                        selected = "DFII10",
                        status = "warning"
                      ),
                      dateInput("start_inflation",
                                "Start Date",
                                "2020-01-01",
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_inflation",
                                "End Date",
                                "2022-04-18",
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_inflation').print();")
               ),
               column(10,
                      plotOutput('distPlot_inflation'),
                      verbatimTextOutput("text_inflation")
             )
             )
    ),
    
    
    ## Fift Tab: Currency
    
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
                                "2020-01-01",
                                format = "yyyy-mm-dd"
                      ),
                      dateInput("end_currency",
                                "End Date",
                                "2022-04-18",
                                format = "yyyy-mm-dd"
                      ),
                      actionButton("go", "Print", onclick = "$('#distPlot_currency').print();")
               ),
               column(10,
                      plotOutput('distPlot_currency'),
                      verbatimTextOutput("text_currency")
             )
             )
    )
    
    
  )
)
)
