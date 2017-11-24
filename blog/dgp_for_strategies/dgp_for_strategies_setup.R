rm(list=ls())
if(!require(pacman)) install.packages(pacman)
pacman::p_load("markovchain", ggplot2, scales, reshape2, dplyr, rowr, zoo)

# Define the data-generating process ----
years <- 10
nobs <- years * 252

sims <- 1000
sim_vector <- 1:sims          
set.seed(8675309) 

good_er <- 4
good_vol <- 12
bad_er <- 0
bad_vol <- 20

market_states <- c("good", "bad")
good_to_bad_transition_prob <- 0.15
bad_to_good_transition_prob <- 0.25

window <- 22
threshold_p <- 0.02 # 2% annual return




# Set up the underlying DGP returns
good_market_returns <- matrix(rnorm(nobs * sims, mean = good_er, sd=good_vol), ncol = sims, nrow=nobs) 
good_market_returns_daily <- (1 + good_market_returns/100) ^ (1/252)
bad_market_returns <- matrix(rnorm(nobs * sims, mean = bad_er, sd=bad_vol), ncol = sims, nrow=nobs)
bad_market_returns_daily <- (1 + bad_market_returns/100) ^ (1/252)

byRow <- TRUE
market_transition_matrix <- matrix(data = c((1-good_to_bad_transition_prob), good_to_bad_transition_prob, 
                                            bad_to_good_transition_prob, (1-bad_to_good_transition_prob)), 
                                            byrow = byRow, nrow = 2, dimnames = list(market_states, market_states))

mc_market <- new("markovchain", states = market_states, byrow = byRow, transitionMatrix = market_transition_matrix, name = "market")
mc_market_sim_states <- as.matrix(sapply(sim_vector, function(x) rmarkovchain(n = nobs , object = mc_market, t0 = "good")), 
                                  nrow=nobs,ncol=sims)


# Generate the Assumed World:  regime switching. 
mc_market_sim_returns <- matrix(NA, nrow = nrow(mc_market_sim_states), ncol = ncol(mc_market_sim_states))

good_index <- which(mc_market_sim_states == "good")
bad_index <- which(mc_market_sim_states == "bad")

mc_market_sim_returns[good_index] <- good_market_returns_daily[good_index]
mc_market_sim_returns[bad_index] <- bad_market_returns_daily[bad_index]
mc_market_sim_returns[1, ] <- 1

mc_market_sim_returns_decimal <- mc_market_sim_returns - 1

# Generate the Null World, just random, no regime switching. 
# Notes: using the exact same mean outcomes as the hypothesized world. 
# It's about timing, no average market returns.

empirical_mean <- mean(mc_market_sim_returns_decimal)
empirical_sd   <- sd(mc_market_sim_returns_decimal)
mc_market_sim_returns_null_decimal <- matrix(rnorm(n = sims * nobs, 
                                                   mean = empirical_mean*100, 
                                                   sd = empirical_sd*100), 
                                     nrow = nrow(mc_market_sim_states), 
                                     ncol = ncol(mc_market_sim_states)) / 100
mc_market_sim_returns_null_decimal[1, ] <- 1

mc_market_sim_returns_cumul <- apply(mc_market_sim_returns, 2, function(x) cumprod(x))
mc_market_sim_returns_null_cumul <- apply(mc_market_sim_returns_null_decimal, 2, function(x) cumprod(x+1))
mc_market_sim_returns_df_long <- melt(mc_market_sim_returns_cumul)
names(mc_market_sim_returns_df_long) <- c("day", "scenario","value")
mc_market_sim_returns_null_df_long <- melt(mc_market_sim_returns_null_cumul)
names(mc_market_sim_returns_null_df_long) <- c("day", "scenario","value")


# Run the strategy through the DGP, compare it to a benchmark
calcStrategySignalAndReturns <- function(return_matrix, annual_threshold, window){
  nobs = nrow(return_matrix)
  sims = ncol(return_matrix)
  daily_threshold <- (1 +annual_threshold) ^ (1/252) - 1
  signals <- matrix(NA, nrow = nobs, ncol = sims)
  return_matrix_zoo <- zoo(return_matrix, 
                           seq.Date(Sys.Date(), (Sys.Date()+nobs-1), by=1)) #dates don't really matter.
  
  # This is the strategy implementation, faster than writing an actual function. 
  # Create a rolling mean. 
  mean_returns <- rollmean(x= return_matrix_zoo, 
                           k = window,
                           align = "right", fill = NA)
  
  signals[which(mean_returns >= daily_threshold)] <- 1
  signals[which(mean_returns < daily_threshold)] <- 0
  signals[1:window,] <- 1 # During the burn-in period, buy.
  
  # Create an index of when the strategy would have bought.
  buy_index <- which(signals == 1)
  
  port_return_matrix <- matrix(0, ncol=ncol(return_matrix), nrow=nrow(return_matrix))
  port_return_matrix <- unname(as.matrix(port_return_matrix)) # remove zoo stuff
  
  port_return_matrix[buy_index] <- mc_market_sim_returns_decimal[buy_index]
  port_return_matrix_cumul <- apply(unname(port_return_matrix),       2, function(x) cumprod(x+1))
  
  port_return_matrix_cumul_long <- melt(port_return_matrix_cumul)
  names(port_return_matrix_cumul_long) <- c("day","scenario","value")
  
  return(list(signals = signals, 
              buy_index = buy_index, 
              port_return_matrix = port_return_matrix, 
              port_return_matrix_cumul = port_return_matrix_cumul, 
              port_return_matrix_cumul_long = port_return_matrix_cumul_long))
}

# Run the strategy through both the null and believed world view.
res_market_null_strategy_true <- calcStrategySignalAndReturns(mc_market_sim_returns_null_decimal, 
                                                              annual_threshold  = threshold_p, 
                                                              window = window)

res_market_true_strategy_true <- calcStrategySignalAndReturns(mc_market_sim_returns_decimal,
                                                              annual_threshold = threshold_p, 
                                                              window = window)


# Conversely, for the strategy = FALSE, we just create buy-and-hold metrics.
calcNullStrategyReturns <- function(return_matrix){
  
  port_return_matrix_cumul <- apply(unname(return_matrix),       2, function(x) cumprod(x+1))
  
  port_return_matrix_cumul_long <- melt(port_return_matrix_cumul)
  names(port_return_matrix_cumul_long) <- c("day","scenario","value")
  
  return(list(port_return_matrix = return_matrix, 
              port_return_matrix_cumul = port_return_matrix_cumul, 
              port_return_matrix_cumul_long = port_return_matrix_cumul_long))
}

res_market_true_strategy_null <- calcNullStrategyReturns(mc_market_sim_returns_decimal)
res_market_null_strategy_null <- calcNullStrategyReturns(mc_market_sim_returns_null_decimal)

res_market_true_strategy_null$port_return_matrix_cumul_long$strategy <- "passive"
res_market_null_strategy_null$port_return_matrix_cumul_long$strategy <- "passive"
res_market_true_strategy_true$port_return_matrix_cumul_long$strategy <- "active"
res_market_null_strategy_true$port_return_matrix_cumul_long$strategy <- "active"

res_market_true_strategy_null$port_return_matrix_cumul_long$world <- "regime"
res_market_null_strategy_null$port_return_matrix_cumul_long$world <- "null"
res_market_true_strategy_true$port_return_matrix_cumul_long$world <- "regime"
res_market_null_strategy_true$port_return_matrix_cumul_long$world <- "null"

cumul_values_all <- rbind(res_market_true_strategy_true$port_return_matrix_cumul_long, 
                          res_market_null_strategy_true$port_return_matrix_cumul_long,
                          res_market_true_strategy_null$port_return_matrix_cumul_long, 
                          res_market_null_strategy_null$port_return_matrix_cumul_long)


save.image("dgp_for_strategies.RData")

