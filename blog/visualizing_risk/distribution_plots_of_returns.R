
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, scales, ggridges, reshape2)

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
  names(returns_cumul_dens_by_year) <- paste0("year_",seq_len(length(returns_cumul_dens_by_year)))
  
  # Create a list of data.frames the PDFs for each year.
  cumul_densities <- lapply(seq_along(returns_cumul_dens_by_year), function(i){
    z <- returns_cumul_dens_by_year[[i]]
    data.frame(year = as.numeric(gsub("year_","",names(returns_cumul_dens_by_year)[[i]])), 
               risk = risk_level,
               loc = z$x,
               dens =z$y)
  })
  cumul_densities_df <-  do.call(rbind, cumul_densities)
  # Find the focus percentiles
  cumul_quantiles <-  apply(returns_cumul, 1, function(x){
    quantile(x, probs = ptiles)
  })
  cumul_quantiles <- data.frame(t(cumul_quantiles))
  cumul_quantiles$year = seq_len(nrow(cumul_quantiles))
  cumul_quantiles <- melt(cumul_quantiles, id="year", variable.name = "percentile")
  cumul_quantiles$percentile <- as.numeric(gsub("[^0-9\\]", "", cumul_quantiles$percentile))/100
  cumul_quantiles$risk = risk_level
  
  return(list(cumul_densities = cumul_densities_df,
              cumul_quantiles = cumul_quantiles))
}
# res_50 <- cumulative_results(0.5)
# str(res_50$cumul_densities)
# res_100 <- cumulative_results(1)
# res_100_50_df <- rbind(res_50$cumul_densities, res_100$cumul_densities)
# res_100_50_qt <- rbind(res_50$cumul_quantiles, res_100$cumul_quantiles)

# Use it to general a large amount of cumulative returns for a given risk profile, 
# which is later sub-setted to get just the number of desired years. 
# More work up front, less work later.

ggplot_vertical_dist <- function(df, group_var, ylims = NULL, quantile_data = NULL){
  #browser()
  df[,group_var] <- as.factor(df[,group_var])
  
  if(nlevels(df[,group_var]) < 2) warning("group_var only has 1 level.")
  
  tp <-  ggplot(data = df, aes_string(y = group_var, 
                                      x = "loc", 
                                      height ="dens")) 
  
  tp <- tp + geom_density_ridges(scale = 0.9, stat = "identity",
                        fill = adjustcolor("dark green", alpha.f = 0.2), 
                        color = "dark green") 
  
  tp <- tp + geom_vline(xintercept = 0, color = "dark grey") +
    theme_minimal() + theme(axis.text.y = element_text(colour = "dark grey")) 
  
  if(!is.null(ylims)){
    tp <- tp + coord_flip(xlim=ylims) 
  } else {
    tp <- tp + coord_flip() 
  }
  
  if(!is.null(quantile_data)){
    
    quantile_data[,group_var] <- as.factor(quantile_data[,group_var])
    quantile_data$dens <- NA
    tp <-  tp + geom_point(data = quantile_data, aes_string(x = "value", y = group_var), size=2, color="red")
  } 
  
  tp
}
#' @examples 
# ex_df <- data.frame(group = as.factor(rep(c("a","b","c"), each=100)), 
#                     dens = rep(c(0:50, 49:1),3),
#                     loc = rep(seq(1:100),3))
# 
# ggplot_vertical_dist(ex_df, "group")
# ggplot_vertical_dist(res_100_50_df %>% filter(risk == 0.50), group_var = "year")
# ggplot_vertical_dist(res_100_50_df %>% filter(year==1), group_var = "risk")
# tp <- ggplot_vertical_dist(res_100_50_df %>% filter(year==1), group_var = "risk", ylims = c(-1,2))
# ggplot_vertical_dist(res_100_50_df %>% filter(year==1), group_var = "risk", ylims = c(-1,2), 
#                      quantile_data =  res_100_50_qt %>% filter(year==1))
# 
# 
