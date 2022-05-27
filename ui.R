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

titlePanel("Chart Maker"),


  tabsetPanel(
    
## First Tab
    
    tabPanel("Overview",
      
    tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
             
    fluidRow(
      column(2,
             awesomeRadio(
               inputId = "metric",
               label = "Metric",
               choices = c("10Yr Yield" = 'DGS10' ,
                           "EV/Sales" ='.',
                           "PE-LTM" ='.',
                           "PE-NTM"= '.'),
               selected = "DGS10",
               status = "warning"
             ),
             dateInput("start",
                       "Start Date",
                       "2020-01-01",
                       format = "yyyy-mm-dd"
             ),
             dateInput("end",
                       "End Date",
                       "2022-04-18",
                       format = "yyyy-mm-dd"
             ),
             actionButton("go", "Submit"),
             actionButton("go", "Print", onclick = "$('#distPlot').print();")
      ),
      column(10,
             plotOutput('distPlot')
      )
    )
),


#### Second Tab

tabPanel("Rates",
         
         tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
         
         fluidRow(
           column(2,
                  awesomeRadio(
                    inputId = "metric2",
                    label = "Metric",
                    choices = c("3mo Tbill" = 'DTB3' ,
                                "2-10 Spread" ='T10Y2Y',
                                "30Y Mortgage" ='MORTGAGE30US'),
                    
                    selected = "DTB3",
                    status = "warning"
                  ),
                  dateInput("start2",
                            "Start Date",
                            "2020-01-01",
                            format = "yyyy-mm-dd"
                  ),
                  dateInput("end2",
                            "End Date",
                            "2022-04-18",
                            format = "yyyy-mm-dd"
                  ),
                  actionButton("go", "Submit"),
                  actionButton("go", "Print", onclick = "$('#distPlot3').print();")
           ),
           column(10,
                  plotOutput('distPlot2'))
         )
         
         
),

## Third Tab

  tabPanel("Inflation",
         
         tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")),
         
         fluidRow(
           column(2,
                  awesomeRadio(
                    inputId = "metric3",
                    label = "Metric",
                    choices = c("10Y TIPS" = 'DFII10' ,
                                "10Y Breakeven Inflation" ='T10YIE',
                                "5Y Breakeven Inflation" ='T5YIE',
                                "5,5 Forward Inflation"= 'T5YIFR'),
                    selected = "DFII10",
                    status = "warning"
                  ),
                  dateInput("start3",
                            "Start Date",
                            "2020-01-01",
                            format = "yyyy-mm-dd"
                  ),
                  dateInput("end3",
                            "End Date",
                            "2022-04-18",
                            format = "yyyy-mm-dd"
                  ),
                  actionButton("go", "Submit"),
                  actionButton("go", "Print", onclick = "$('#distPlot2').print();")
           ),
           column(10,
                  plotOutput('distPlot3'))
          )
         )
       
         
)
)
)
