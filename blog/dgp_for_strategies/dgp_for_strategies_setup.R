rm(list=ls())
if(!require(pacman)) install.packages(pacman)
<<<<<<< HEAD
pacman::p_load("markovchain", ggplot2, scales, reshape2, dplyr, rowr, zoo)
=======
pacman::p_load("markovchain", ggplot2, scales, reshape2, dplyr, rowr, zoo, here)
>>>>>>> d4b20eb2ec83f76c9c82434d367abc1efe76a03e

# Data generating processes
years <- 10
nobs <- years * 252

sims <- 1000
sim_vector <- 1:sims          
set.seed(8675309) 

good_er <- 4
good_vol <- 12
bad_er <- 0
bad_vol <- 20
good_to_bad_transition_prob <- 0.3
bad_to_good_transition_prob <- 0.3

market_states <- c("good", "bad")
good_to_bad_transition_prob <- 0.15
bad_to_good_transition_prob <- 0.25

window <- 22
<<<<<<< HEAD
threshold_p <- 0.02 # 2% annual return




# Set up the underlying DGP returns
good_market_returns <- matrix(rnorm(nobs * sims, mean = good_er, sd=good_vol), ncol = sims, nrow=nobs) 
good_market_returns_daily <- (1 + good_market_returns/100) ^ (1/252)
bad_market_returns <- matrix(rnorm(nobs * sims, mean = bad_er, sd=bad_vol), ncol = sims, nrow=nobs)
bad_market_returns_daily <- (1 + bad_market_returns/100) ^ (1/252)

=======
threshold_p <- 0.02 # This parameter controls when the alternatie stratey believes it's in a 'good' or 'bad' market.
set.seed(8675309)


## Alternative (Non-Null)  data-generating process ----

#### This is the setup of a regime-changing type process switching from good to bad etc.
good_market_returns <- matrix(rnorm(nobs * sims, mean = good_er, sd=good_vol), ncol = sims, nrow=nobs)
good_market_returns_daily <- ((1 + good_market_returns/100) ^ (1/252))-1
bad_market_returns <- matrix(rnorm(nobs * sims, mean = bad_er, sd=bad_vol), ncol = sims, nrow=nobs)
bad_market_returns_daily <- ((1 + bad_market_returns/100) ^ (1/252))-1
market_states <- c("good", "bad")
>>>>>>> d4b20eb2ec83f76c9c82434d367abc1efe76a03e
byRow <- TRUE
market_transition_matrix <- matrix(data = c((1-good_to_bad_transition_prob), good_to_bad_transition_prob,
                                            bad_to_good_transition_prob, (1-bad_to_good_transition_prob)),
                                            byrow = byRow, nrow = 2, dimnames = list(market_states, market_states))

mc_market <- new("markovchain", states = market_states, byrow = byRow, transitionMatrix = market_transition_matrix, name = "market")
<<<<<<< HEAD
mc_market_sim_states <- as.matrix(sapply(sim_vector, function(x) rmarkovchain(n = nobs , object = mc_market, t0 = "good")), 
                                  nrow=nobs,ncol=sims)


# Generate the Assumed World:  regime switching. 
mc_market_sim_returns <- matrix(NA, nrow = nrow(mc_market_sim_states), ncol = ncol(mc_market_sim_states))
=======


### Simulated market outcomes based on alternative DGP
sim_vector <- 1:sims
mc_market_sim_states <- as.matrix(sapply(sim_vector, function(x) rmarkovchain(n = nobs , object = mc_market, t0 = "good")),
                                  nrow=nobs,ncol=sims)

market_sim_returns_alternative <- matrix(NA, nrow = nrow(mc_market_sim_states), ncol = ncol(mc_market_sim_states))
>>>>>>> d4b20eb2ec83f76c9c82434d367abc1efe76a03e

good_index <- which(mc_market_sim_states == "good")
bad_index <- which(mc_market_sim_states == "bad")

<<<<<<< HEAD
mc_market_sim_returns[good_index] <- good_market_returns_daily[good_index]
mc_market_sim_returns[bad_index] <- bad_market_returns_daily[bad_index]
mc_market_sim_returns[1, ] <- 1
=======
market_sim_returns_alternative[good_index] <- good_market_returns_daily[good_index]
market_sim_returns_alternative[bad_index] <- bad_market_returns_daily[bad_index]
market_sim_returns_alternative[1, ] <- 0

>>>>>>> d4b20eb2ec83f76c9c82434d367abc1efe76a03e

## Null DGP Market Outcomes ------
## We want these to be similar in ER etc to the Alternative outcomes, so we'll just do a random
## normal sample from the empirical outcomes.
scalar_factor <- 100000 # Deailing with values <1
market_sim_returns_null <- matrix(rnorm(n = length(market_sim_returns_alternative),
                                        mean = mean(market_sim_returns_alternative * scalar_factor),
                                        sd = sd(market_sim_returns_alternative * scalar_factor)),
                                  nrow = nrow(market_sim_returns_alternative)) / scalar_factor

<<<<<<< HEAD
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

=======

##  Alternative Strategy ----
strategy_market_timing <- function(return_data, threshold, weighted = FALSE){
  if(weighted){
    weights <- 1/seq(1:length(return_data))
    weights <- weights/sum(weights)
    measure <- weighted.mean(return_data, weights)
  } else {
    measure <- mean(return_data)
  }
  signal <- ifelse(measure >= threshold, 1,0)
  return(signal)
}
threshold <- ((1 + threshold_p) ^ (1/252)) - 1
# strategy_market_timing(rep(1, 30))
# strategy_market_timing(rep(2, 30))
# strategy_market_timing(rep(2.1, 30))
# strategy_market_timing(seq(0, 3.2, length.out = 30))
# strategy_market_timing(mean(seq(3.2,0, length.out = 30)),  threshold =  2)
# strategy_market_timing(mc_market_sim_returns_decimal[22:44,1], threshold = threshold)
# mean(mc_market_sim_returns_decimal[22:44,2]) > threshold


# Process the results/returns of a strategy. Note it relies upon R's leaky encapsulation for window.
processStrategy <- function(return_matrix, strategy){
  if(strategy == "baseline"){
    strategy_sim_returns <- return_matrix
  } else {  # Assumes binary strategy input
    strategy_sim_returns <- matrix(0, ncol=ncol(return_matrix), nrow=nrow(return_matrix))

    signals <- matrix(NA, nrow = nrow(return_matrix), ncol = ncol(return_matrix))
    market_sim_returns_zoo <- zoo(return_matrix,
                                  seq.Date(Sys.Date(), (Sys.Date()+nobs-1), by=1))
    mean_returns <- rollmean(x= market_sim_returns_zoo,
                                  k = window,
                                  align = "right", fill = NA, by.column =TRUE)

    signals[which(mean_returns >= threshold)] <- 1
    signals[which(mean_returns < threshold)] <- 0
    signals[1:window,] <- 1  # Auto-invest in burn-in period.
    buy_index <- which(signals == 1)
    strategy_sim_returns[buy_index] <- return_matrix[buy_index]
  }

  strategy_cumul_values <- apply(unname(strategy_sim_returns),       2, function(x) cumprod(x+1))
  results <- melt(strategy_cumul_values)
  results$strategy <- strategy
  return(results)
}


processSimulations <- function(outcomes_null, outcomes_alternative){

  results_null_baseline         <- processStrategy(outcomes_null, "baseline")
  results_null_active           <- processStrategy(outcomes_null, "active")
  results_alternative_baseline  <- processStrategy(outcomes_alternative, "baseline")
  results_alternative_active    <- processStrategy(outcomes_alternative, "active")

  results_null_baseline$world <- "null"
  results_null_active$world <- "null"
  results_alternative_baseline$world <- "alternative"
  results_alternative_active$world <- "alternative"

  results <- rbind(results_null_baseline,
                   results_null_active,
                   results_alternative_baseline,
                   results_alternative_active)
  return(results)

}

cumul_values_all <- processSimulations(outcomes_null = market_sim_returns_null,
                                       outcomes_alternative = market_sim_returns_alternative )

names(cumul_values_all)[names(cumul_values_all) == "Var2"] <- "simulation_id"
names(cumul_values_all)[names(cumul_values_all) == "Var1"] <- "day"

# Saving down data ----
rm(bad_market_returns, bad_market_returns_daily, good_market_returns, good_market_returns_daily)
save.image(here("blog","dgp_for_strategies","data","dgp_for_strategies.RData"))  # 94MB of data.
>>>>>>> d4b20eb2ec83f76c9c82434d367abc1efe76a03e
