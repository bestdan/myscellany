# A very simply way to get fixed-Sharpe portfolios stats based on risk level.
# Anchored to a 100% stock portfolio with 6% return and 17% vol.
portstats <- function(risk_level){
  er <- 6 * risk_level
  sd <- 17 * risk_level
  return(c(er = er, sd = sd))
}


# A workhorse function to produce the results.
simulate_returns <- function(er, sd, n, t){
  raw_returns <- matrix(rnorm(n = n*t, mean = er, sd = sd) / 100,
                        nrow=t)
}


cumulative_simulation <- function(risk_level, n=200, t=10){

  ps <- portstats(risk_level)

  raw_returns <- simulate_returns(ps['er'], ps['sd'], n, t)

  returns_cumul <- as.numeric(apply(raw_returns, 2, function(x) {cumprod(x+1)}) - 1)
  return(returns_cumul)
}
# results <- cumulative_simulation(0.5)


dotplot_transparent <- function(results){
  results <- data.frame(x=rep(1, length(results)),
                        y=results)
  avg <- mean(results$y)
  results$dev <- results$y - avg
  results$xdev <- (abs(1/pmax(abs(results$dev),0.01))) ^ (1/3)
  results$x_adj <- (results$xdev  * ((rnorm(n = nrow(results), mean = 0, sd = 0.02))/10) + 1)

  limoffset <- 0.05
  ggplot(results, aes(x_adj, y)) +
    geom_point(shape = 20, size=4,  color = adjustcolor("dark green", 0.10), position = position_dodge(width = 0.01)) +
    coord_cartesian(xlim=c(1-limoffset, 1+limoffset)) +
    scale_x_continuous(breaks = 1, label="") + xlab("") +
    scale_y_continuous(labels = percent) + ylab("Cumulative return") +
    geom_hline(yintercept=0, color='dark grey')
}


dotplot_stacked <- function(results){

  ylims <- range(pretty(quantile(results, probs=c(0.01, 0.99))))
  binwidth <- diff(ylims)/200

  ggplot(data.frame(results=results), aes(y=results, x=0)) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = binwidth,
                 fill = adjustcolor("dark green", alpha.f = 0.2), color = adjustcolor("dark green", alpha.f = 0.2)) +
    coord_cartesian( ylim=ylims) +
    scale_x_continuous(NULL, labels = NULL, breaks = NULL) +
    scale_y_continuous(labels = percent) +
    geom_vline(xintercept = 0, color= "dark grey") +
    geom_hline(yintercept = seq(-1, 10, 0.1), color = "light grey") + geom_hline(yintercept = 0, color = "dark grey") +
    ylab("Cumulative growth") +
    theme_light()


}
# results <- cumulative_simulation(0.5, n=1000)
# dotplot_stacked(results)
