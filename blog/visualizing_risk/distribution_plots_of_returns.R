
# A very simply way to get fixed-Sharpe portfolios stats based on risk level. 
# Anchored to a 100% stock portfolio with 6% return and 17% vol. 
portstats <- function(risk_level){
  er <- 6 * risk_level
  sd <- 17 * risk_level
  return(c(er = er, sd = sd))
}


# A workhorse function to produce the results. 
cumulative_results <- function(risk_level, ptiles = c(0.20, 0.5, 0.80) ){
  ps <- portstats(risk_level)
  raw_returns <- matrix(rnorm(n = 100000, mean = ps['er'], sd = ps['sd']) / 100, 
                        nrow=10)
  returns_cumul <- apply(raw_returns, 2, function(x) {cumprod(x+1)}) - 1
  
  returns_cumul_dens_by_year <- apply(returns_cumul, 1, density)
  
  cumul_densities <- lapply(returns_cumul_dens_by_year, function(z){
    data.frame(loc = z$x,
               dens =z$y)
  })
  
  cumul_quantiles <-  apply(returns_cumul, 1, function(x){
    quantile(x, probs = ptiles)
  }) 
  
  return(list(cumul_densities = cumul_densities, 
              cumul_quantiles = cumul_quantiles))
}
