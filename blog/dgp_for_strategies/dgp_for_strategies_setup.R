rm(list=ls())
here("blog", "dgp_for_strategies")
if(!require(pacman)) install.packages(pacman)
pacman::p_load("markovchain", ggplot2, scales, reshape2, dplyr, rowr, zoo, here)

# Define the data-generating process ----
years <- 10
sims <- 1000
nobs <- years * 252
good_er <- 4
good_vol <- 12
bad_er <- 0
bad_vol <- 20

window <- 22
threshold_p <- 0.02 # 2% annual retur
set.seed(8675309)

good_market_returns <- matrix(rnorm(nobs * sims, mean = good_er, sd=good_vol), ncol = sims, nrow=nobs)
good_market_returns_daily <- (1 + good_market_returns/100) ^ (1/252)
bad_market_returns <- matrix(rnorm(nobs * sims, mean = bad_er, sd=bad_vol), ncol = sims, nrow=nobs)
bad_market_returns_daily <- (1 + bad_market_returns/100) ^ (1/252)

market_states <- c("good", "bad")
good_to_bad_transition_prob <- 0.3
bad_to_good_transition_prob <- 0.3

byRow <- TRUE
market_transition_matrix <- matrix(data = c((1-good_to_bad_transition_prob), good_to_bad_transition_prob,
                                            bad_to_good_transition_prob, (1-bad_to_good_transition_prob)),
                                            byrow = byRow, nrow = 2, dimnames = list(market_states, market_states))

mc_market <- new("markovchain", states = market_states, byrow = byRow, transitionMatrix = market_transition_matrix, name = "market")


# Create a set of sample market outcomes based on your DGP
sim_vector <- 1:sims
mc_market_sim_states <- as.matrix(sapply(sim_vector, function(x) rmarkovchain(n = nobs , object = mc_market, t0 = "good")),
                                  nrow=nobs,ncol=sims)

mc_market_sim_returns <- matrix(NA, nrow = nrow(mc_market_sim_states), ncol = ncol(mc_market_sim_states))

good_index <- which(mc_market_sim_states == "good")
bad_index <- which(mc_market_sim_states == "bad")

mc_market_sim_returns[good_index] <- good_market_returns_daily[good_index]
mc_market_sim_returns[bad_index] <- bad_market_returns_daily[bad_index]
mc_market_sim_returns[1, ] <- 1
mc_market_sim_returns_cumul <- apply(mc_market_sim_returns, 2, function(x) cumprod(x))

mc_market_sim_returns_df_long <- melt(mc_market_sim_returns_cumul)
# ggplot(mc_market_sim_returns_df_long %>% filter(Var2 <200)) +
#   geom_line(aes(x=Var1, y=value, group = Var2), color = adjustcolor("dark green",alpha.f = 0.2 ))

mc_market_sim_returns_decimal <- mc_market_sim_returns - 1

# Define a strategy that should work in that process

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

# Run the strategy through the DGP, compare it to a benchmark
signals <- matrix(NA, nrow = nobs, ncol = sims)
mc_market_sim_returns_decimal <- zoo(mc_market_sim_returns_decimal,
                                     seq.Date(Sys.Date(), (Sys.Date()+nobs-1), by=1))

mean_returns <- rollmean(x= mc_market_sim_returns_decimal,
                         k = window,
                         align = "right", fill = NA)

signals[which(mean_returns >= threshold)] <- 1
signals[which(mean_returns < threshold)] <- 0
signals[1:window,] <- 1

# See how good the strategy is across futures.
buy_index <- which(signals == 1)

mc_strategy_sim_returns <- matrix(0, ncol=ncol(mc_market_sim_returns_decimal), nrow=nrow(mc_market_sim_returns_decimal))
mc_market_sim_returns_decimal <- unname(as.matrix(mc_market_sim_returns_decimal)) # remove zoo stuff

mc_strategy_sim_returns[buy_index] <- mc_market_sim_returns_decimal[buy_index]

strategy_cumul_values <- apply(unname(mc_strategy_sim_returns),       2, function(x) cumprod(x+1))
passive_cumul_values <-  apply(unname(mc_market_sim_returns_decimal), 2, function(x) cumprod(x+1))

strategy_cumul_values_long <- melt(strategy_cumul_values)
strategy_cumul_values_long$strategy <- "active"

passive_cumul_values_long <- melt(passive_cumul_values)
passive_cumul_values_long$strategy <- "passive"

cumul_values_all <- rbind(passive_cumul_values_long, strategy_cumul_values_long)

save.image(here("blog","dgp_for_strategies","data","dgp_for_strategies.RData"))  # 239MB of data.
