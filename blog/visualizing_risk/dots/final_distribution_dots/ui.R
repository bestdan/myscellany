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
  titlePanel("Stuff"),

  # Sidebar with a slider input for number of bins
  fluidPage(
    fluidRow(
      column(width = 6,
             sliderInput("riskLevel","Risk Level", min = 0, max = 100, value = 50),
             sliderInput("timeHorizon", label = "Time Horizon", min = 1, max = 50, value=10)),
      column(width = 6,
             numericInput("n", label = "Number of obs", min = 100, max = 10000, value = 300),
             selectInput("graphType", label="Graph Type", choices = c("Dotplot: transparent" = "dotplot_transparent", "Dotplot: stacked" = "dotplot_stacked"), selected="dotplot_stacked")
      )),
    fluidRow(plotOutput("distPlot", height = "800px")
    )
  )
)
)
