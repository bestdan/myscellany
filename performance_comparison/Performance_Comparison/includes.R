#' @title Functions to include
library(magrittr)

simpleRet <- function(x){
  (x/lag(x)) - 1  
}


#' @description  Cumulative returns
getReturns <- function(sym){
  options("getSymbols.warning4.0" = FALSE)
  options("getSymbols.yahoo.warning" = FALSE)
  require(quantmod)
  
  sym %>% 
    getSymbols(., from = "1900-01-01", auto.assign = F) %>%
    Ad(.) %>% 
    simpleRet()
}
#  x <- getReturns("VFINX")


#' @description  Cumulative returns
cumulRet <- function(x){
  tail(cumprod(x+1)-1, 1)
}

generateReturnMatrix <- function(x){
  x %>% 
    as.numeric() %>%
    na.fill(., fill = 0) -> z
  cumulret <-   cumprod(z + 1) 
  #z <- na.fill(as.numeric(), fill = 0)
  #cumulret <- cumprod(as.numeric(z) + 1)
  matHorizonRet <- outer(cumulret, cumulret, "/") - 1
  return(matHorizonRet)
}


extractKthDiagonal <- function(mat, k){
  delta <- rep(seq_len(ncol(mat)), nrow(mat)) - 
    rep(seq_len(nrow(mat)), each = ncol(mat))
  #or Ben Bolker's better alternative
  delta <- row(mat) - col(mat)
  vec <- mat[delta == (k-1) | delta == (k-1)] 
  return(vec)
}
# size <- 6
# mat <- matrix(seq_len(size ^ 2), ncol = size)
# mat
# extractKthDiagonal(mat, 3)


#' @description  Determines how many decimal places a number has. 
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' @description  Finds the mid-points of a sequence
midcut<-function(x,from,to,by){
  ## cut the data into bins...
  x=cut(x,seq(from,to,by),include.lowest=T)
  ## make a named vector of the midpoints, names=binnames
  vec=seq(from+by/2,to-by/2,by)
  names(vec)=levels(x)
  ## use the vector to map the names of the bins to the midpoint values
  unname(vec[x])
}

#' @description  Provides midpoints and 'nice' sequences for decimals. 
autocat <- function(x){
  xr <- range(pretty(x,eps.correct = 2))
  dp <- max(sapply(xr, decimalplaces))
  dp_by <- 1/(10^(dp+1))
  xseq <- seq(from = xr[1], 
              to = xr[2], 
              by = dp_by)
  return(list(
    breaks = c(xr, dp_by), 
    xseq = xseq))
}
# autocat(seq(-0.123, 1.2, 0.01))

arrayPlot <- function(tidyframe){
  require(ggplot2)
  tidyframe %>% 
    ggplot(aes(x=mod_x, y=mod_y, col=value)) + 
    geom_point() + 
    scale_color_gradient2(low = "orange", 
                          mid = "grey", 
                          high = "blue", 
                          midpoint = 0, 
                          labels = percent) + 
    xlab(paste0("Return difference (", input$primary_ticker, " - ", input$secondary_ticker,")")) +
    labs(color = "Return size") +
    theme_minimal() + 
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(), 
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
}



distributionPlot <- function(tidyframe){
  tidyframe %>% 
  ggplot(aes(x=return_cat_mp, y=counter, color= value)) + 
    geom_point(size = 1/3) +
    scale_color_gradient2(low = "orange", 
                          mid = "grey", 
                          high = "blue", 
                          midpoint = 0, 
                          labels = percent)+
    geom_vline(xintercept = 0) + 
    xlab(paste0("Return difference (", input$primary_ticker, " - ", input$secondary_ticker,")")) + 
    scale_x_continuous(label = percent) + 
    ylab("Frequency") + theme_minimal()  
}
