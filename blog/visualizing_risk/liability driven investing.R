library(BMTFxns)
library(scales)


rffc <- data.frame(rate = rep(0,600))

timeSeq <- seq(1,10)

target <- 10000

results <- sapply(timeSeq, function(x){
  monthlyAmount <- as.numeric(rec_save_v2(goal_amt = target, term = x, balance = 0, annual_return = 4, annual_vol = 0, rffc = rffc)[1])
  totalAmount <-  monthlyAmount * x * 12
  return(c(monthlyAmount, totalAmount))
})

results <- as.data.frame(t(results))
names(results) <- c("monthly","total")
results$t10_diff_a <- target - results$total 
t10_diff_p <- (results$total / target) - 1
results_print <- apply(results, 2, function(x) dollar(round(x,0)))
results_print <- as.data.frame(cbind(Years = timeSeq, results_print, percent(t10_diff_p)))
print.data.frame(results_print)
