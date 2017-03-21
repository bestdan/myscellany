
library(ggplot2)
library(scales)
library(animation)
source("dot_functions.R")

results <- cumulative_simulation(risk_level = 50/100, n = 1000, t = 10)

# This builds a gif of dots filling in the gif over time.
incremental_nobs <- 20
ani.options("interval" = 0.2, autobrowse = FALSE)

buildPlot <- function(results){
    for(i in seq(incremental_nobs, length(results), incremental_nobs)){
      p <- stack_dotplot(results[1:i])
      print(p)
    }
  }

saveGIF(buildPlot(results))
