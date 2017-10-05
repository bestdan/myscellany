
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, scales, ggridges)

# A very simply way to get fixed-Sharpe portfolios stats based on risk level.
# Anchored to a 100% stock portfolio with 6% return and 17% vol.
portstats <- function(risk_level, er = 6, vol=17){
  er <- er * risk_level
  vol <- vol * risk_level
  return(c(er = er, vol = vol))
}


# A workhorse function to produce the results.
cumulative_results <- function(risk_level, ptiles = c(0.20, 0.5, 0.80), years = 10, nobs = 100000){
  ps <- portstats(risk_level)
  # Generate a matrix of returns
  raw_returns <- matrix(rnorm(n = nobs, mean = ps['er'], sd = ps['vol']) / 100,
                        nrow=years)
  returns_cumul <- apply(raw_returns, 2, function(x) {cumprod(x+1)}) - 1
  
  # For each year, find the pdf of cumulative returns
  returns_cumul_dens_by_year <- apply(returns_cumul, 1, density)
  
  # Create a list of data.frames the PDFs for each year.
  cumul_densities <- lapply(returns_cumul_dens_by_year, function(z){
    data.frame(loc = z$x,
               dens =z$y)
  })
  
  # Find the focus percentils
  cumul_quantiles <-  apply(returns_cumul, 1, function(x){
    quantile(x, probs = ptiles)
  })
  
  return(list(cumul_densities = cumul_densities,
              cumul_quantiles = cumul_quantiles))
}
res_50 <- cumulative_results(0.5)
res_100 <- cumulative_results(1)
str(res)

# Use it to general a large amount of cumulative returns for a given risk profile, 
# which is later sub-setted to get just the number of desired years. 
# More work up front, less work later.

outcomeDensity <- function(df, group_var){
  df %>% group_by(risk_level) %>% do(data.frame(loc = density(.$value)$x,
                                                dens = density(.$value)$y))
}



ex_df <- cbind(res_50$cumul_densities[7])

ggplot_vertical_dist <- function(df, group_var, addMedians = TRUE){

  tp <-  ggplot(data = df, aes_string(y = group_var , 
                               x = "loc", 
                               height ="dens")) +
    geom_density_ridges(scale = 0.9, stat = "identity",
                        fill = adjustcolor("dark green", alpha.f = 0.2), 
                        color = "dark green") + 
    scale_x_continuous(label=percent) +
    coord_flip() +
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
#' @examples 
# ex_df <- data.frame(group = as.factor(rep(c("a","b","c"), each=100)), 
#                     dens = rep(c(0:50, 49:1),3),
#                     loc = rep(seq(1:100),3))
# 
# ggplot_vertical_dist(ex_df, "group")
# 
