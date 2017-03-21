


results <- cumulative_simulation(risk_level = 50/100, n = 5000, t = 10)

  ani.options("interval" = 0.5, autobrowse = FALSE)
  buildPlot <- function(results){
    for(i in seq(20, length(results),length.out = 20)){
      p <- stack_dotplot(results[1:i])  
      print(p)
    }
  }
  
  saveGIF(buildPlot(results))
