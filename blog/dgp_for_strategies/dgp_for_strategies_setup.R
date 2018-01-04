rm(list=ls())
if(!require(pacman)) install.packages(pacman)
pacman::p_load("markovchain", ggplot2, scales, reshape2, dplyr, rowr, zoo, here)

# Data generating processes
years <- 10
sims <- 1000
nobs <- years * 252
good_er <- 4
good_vol <- 12
bad_er <- 0
bad_vol <- 20
good_to_bad_transition_prob <- 0.3
bad_to_good_transition_prob <- 0.3

window <- 22
threshold_p <- 0.02 # This parameter controls when the alternatie stratey believes it's in a 'good' or 'bad' market.
set.seed(8675309)


## Alternative (Non-Null)  data-generating process ----

#### This is the setup of a regime-changing type process switching from good to bad etc.
good_market_returns <- matrix(rnorm(nobs * sims, mean = good_er, sd=good_vol), ncol = sims, nrow=nobs)
good_market_returns_daily <- ((1 + good_market_returns/100) ^ (1/252))-1
bad_market_returns <- matrix(rnorm(nobs * sims, mean = bad_er, sd=bad_vol), ncol = sims, nrow=nobs)
bad_market_returns_daily <- ((1 + bad_market_returns/100) ^ (1/252))-1
market_states <- c("good", "bad")
byRow <- TRUE
market_transition_matrix <- matrix(data = c((1-good_to_bad_transition_prob), good_to_bad_transition_prob,
                                            bad_to_good_transition_prob, (1-bad_to_good_transition_prob)),
                                            byrow = byRow, nrow = 2, dimnames = list(market_states, market_states))

mc_market <- new("markovchain", states = market_states, byrow = byRow, transitionMatrix = market_transition_matrix, name = "market")


### Simulated market outcomes based on alternative DGP
sim_vector <- 1:sims
mc_market_sim_states <- as.matrix(sapply(sim_vector, function(x) rmarkovchain(n = nobs , object = mc_market, t0 = "good")),
                                  nrow=nobs,ncol=sims)

market_sim_returns_alternative <- matrix(NA, nrow = nrow(mc_market_sim_states), ncol = ncol(mc_market_sim_states))

good_index <- which(mc_market_sim_states == "good")
bad_index <- which(mc_market_sim_states == "bad")

market_sim_returns_alternative[good_index] <- good_market_returns_daily[good_index]
market_sim_returns_alternative[bad_index] <- bad_market_returns_daily[bad_index]
market_sim_returns_alternative[1, ] <- 0


## Null DGP Market Outcomes ------
## We want these to be similar in ER etc to the Alternative outcomes, so we'll just do a random
## normal sample from the empirical outcomes.
market_sim_returns_null <- matrix(rnorm(n = length(market_sim_returns_alternative),
                                        mean = mean(market_sim_returns_alternative * 100),
                                        sd = sd(market_sim_returns_alternative*100)),
                                  nrow = nrow(market_sim_returns_alternative)) / 100


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
                                  align = "right", fill = NA)

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
