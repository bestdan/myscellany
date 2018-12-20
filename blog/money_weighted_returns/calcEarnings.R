
calcEarnings <- function(returns, deposits){
  allStreams <- xts(order.by = index(returns))
  allValues <- xts(order.by = index(returns))

  for(i in 1:length(deposits)){
    thisDeposit <- deposits[i]

    # Find cumulative return
    thisStream <- cumprod(returns[paste0(index(thisDeposit),"/")]+1)
    names(thisStream) <- paste0("deposit_", index(thisDeposit))

    # Find growth of deposit
    thisValue <- thisStream *  as.numeric(thisDeposit)
    allValues <- merge(allValues, thisValue)

    thisStream <- thisStream-1
    allStreams <- merge(allStreams, thisStream)
  }

  cumulValue <- apply(as.matrix(allValues), 1, function(x) sum(x, na.rm=TRUE))
  cumulValue <- xts(cumulValue, order.by = as.Date(names(cumulValue)))
  netDeposits <- cumsum(deposits)

  veMat <- merge(cumulValue, netDeposits, all = TRUE)
  veMat$netDeposits <- na.locf(veMat$netDeposits, fromLast = FALSE)
  veMat$cumulValue <- na.locf(veMat$cumulValue, fromLast = FALSE)

  veMat$earnings <- veMat$cumulValue - veMat$netDeposits
  veMat$earningsPercent <- veMat$earnings / veMat$netDeposits

  return(list(returnStream = allStreams,
              valueStreams = allValues,
              valueEarningMat = veMat))
}
