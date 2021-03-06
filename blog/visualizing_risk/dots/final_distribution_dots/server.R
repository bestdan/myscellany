#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(scales)
library(animation)
source("dot_functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    results <- cumulative_simulation(risk_level = input$riskLevel/100, n = input$n, t = input$timeHorizon)

    switch(input$graphType,
           "dotplot_transparent" = dotplot_transparent(results),
           "dotplot_stacked" = dotplot_stacked(results))

  })

})

