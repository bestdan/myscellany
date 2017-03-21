


results <- cumulative_simulation(risk_level = 50/100, n = 1000, t = 10)
p <- stack_dotplot(results[1:i])  

  ani.options("interval" = 0.3, autobrowse = FALSE)
  buildPlot <- function(results){
    for(i in seq(20, length(results), 100)){
      p <- stack_dotplot(results[1:i])  
      print(p)
    }
  }
  
  saveGIF(buildPlot(results))
