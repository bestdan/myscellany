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
source("dot_functions.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    results <- cumulative_simulation(risk_level = input$riskLevel/100, n = input$n, t = input$timeHorizon)

    results <- data.frame(x=rep(1, length(results)),
                          y=results)
    avg <- mean(results$y)
    results$dev <- results$y - avg
    results$xdev <- (abs(1/pmax(abs(results$dev),0.01))) ^ (1/3)
    results$x_adj <- (results$xdev  * ((rnorm(n = nrow(results), mean = 0, sd = 0.02))/10) + 1)

    limoffset <- 0.05
    ggplot(results, aes(x_adj, y)) +
      geom_point(shape = 20, size=4,  color = adjustcolor("dark green", 0.10)) +
      coord_cartesian(xlim=c(1-limoffset, 1+limoffset)) +
      scale_x_continuous(breaks = 1, label="") + xlab("") +
      scale_y_continuous(labels = percent) + ylab("Cumulative return") +
      geom_hline(yintercept=0, color='dark grey')


  })

})
