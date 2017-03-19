

cumulative_results <- function(risk_level, ptiles = c(0.20, 0.5, 0.80) ){
  ps <- portstats_v2(risk_level)
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
