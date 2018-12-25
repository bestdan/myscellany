#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Performance Comparison"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput(inputId = "primary_ticker",
                 label = "Primary ticker",
                 value = "VASGX"),
       
       textInput(inputId = "secondary_ticker",
                 label = "Secondary ticker",
                 value = "VFINX"), 
       
       selectInput(inputId = "return_period", 
                   label = "Return period", 
                   choices = c("daily", "weekly", "monthly", "annually"), 
                   selected = "weekly", multiple = FALSE), 
       
       dateInput(inputId = "live_start_date", 
                 label = "Start date of live data", 
                 value = as.Date("2018-01-01"), 
                 weekstart = 1, 
                 max = Sys.Date() - 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Distribution",  
          plotOutput("distPlot")
        ), 
        tabPanel(title = "Array",
          plotOutput("arrayPlot")
        )
      )
    )
  )
))
