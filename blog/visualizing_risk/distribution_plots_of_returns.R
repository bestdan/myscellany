
# A very simply way to get fixed-Sharpe portfolios stats based on risk level.
# Anchored to a 100% stock portfolio with 6% return and 17% vol.
portstats <- function(risk_level){
  er <- 6 * risk_level
  vol <- 17 * risk_level
  return(c(er = er, vol = vol))
}


# A workhorse function to produce the results.
cumulative_results <- function(risk_level, ptiles = c(0.20, 0.5, 0.80) ){
  ps <- portstats(risk_level)
  raw_returns <- matrix(rnorm(n = 100000, mean = ps['er'], sd = ps['vol']) / 100,
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


outcomeDensity <- function(df, group_var){
  df %>% group_by(risk_level) %>% do(data.frame(loc = density(.$value)$x,
                                                dens = density(.$value)$y))
}


ggplot_vertical_dist <- function(df, group_var, addMedians = TRUE){

  tp <- ggplot(df, aes(y = dens, x = loc, group = group_var, height = as.numeric(as.factor(group_var)),
                       fill= adjustcolor("dark green", alpha.f = 0.2), color="dark green")) +
    geom_density_ridges(stat = "identity", inherit.aes = TRUE) + coord_flip() +
    #scale_y_continuous(breaks = pretty(pdat$loc), label=percent) +
    ylab('Expected Return') + xlab("Expected Risk") +
    geom_hline(yintercept = 0, color = "dark grey") +
    theme_minimal() + theme(axis.text.y = element_text(colour = "dark grey")) #+
    #coord_cartesian(ylim = c(-0.4, 0.4))
  tp
  # if(addMedians){
  #     temp_medians <-
  #     tp + geom_point(data = temp_medians, aes(x = group_var, y = value), size=2, color="red")
  #   } else {
  #     tp
  #   }

}


