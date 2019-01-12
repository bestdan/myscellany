#' @title Server file for Performance Comparison
#' @author Daniel Egan
#' @section To Do
#' - convert output to labeled plotly
#' - modularize sectional code
#' - emphasize zero in icon array
#' - rolling period plot
#' - use live vs historical comparison
#' - risk versus return of each other
#' - comovement

library(shiny)
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(JuliaCall)
library(quantmod)

julia_setup(JULIA_HOME = "/Applications/Julia-1.0.app/Contents/Resources/julia/bin/")

source("includes.R")

shinyServer(function(input, output) {
  
  updateData <- reactive({
    withProgress(message = 'Fetching data', style = 'notification', detail = "part 0", value = 0, {
      #  input <- list()
      #  input$primary_ticker <- "VASGX"
      #  input$secondary_ticker <- "VFINX" 
      #  browser()
      incProgress(amount = 0.3, message = "Fetching primary data")
      primary_ticker_returns <- getReturns(sym = input$primary_ticker)
      incProgress(amount = 0.3, message = "Fetching secondary data")
      secondary_ticker_returns <- getReturns(sym = input$secondary_ticker)
      #  save(primary_ticker_returns, 
      #       secondary_ticker_returns, file = "temp_cache.Rdata")
      #load(file = "temp_cache.Rdata")
      incProgress(amount = 0.3, message = "Fetched data, processing")
      return_data <- merge(primary_ticker_returns, secondary_ticker_returns, join = 'inner')
      names(return_data) <- c(input$primary_ticker, input$secondary_ticker)
      incProgress(amount = 0.3, message = "Generating return matrices")
      primary_return_matrix <- generateReturnMatrix(return_data[, input$primary_ticker])
      secondary_return_matrix <- generateReturnMatrix(return_data[, input$secondary_ticker])
    })
    return(list(return_data = return_data, 
                primary_return_matrix = primary_return_matrix, 
                secondary_return_matrix = secondary_return_matrix))
  })
  
  
  apply_period <- reactive({
    return_matrices <- updateData()
    withProgress(message = 'Applying periodicity', style = 'notification', detail = "part 0", value = 0, {
      
      return_period <- switch(input$return_period, 
                              "daily" = 2, 
                              "weekly" = 5, 
                              "monthly" = 21, 
                              "annually" = 252)
      incProgress(amount = 0.3, message = "Extracting diagonals")
      period_returns <- data.frame(primary = extractKthDiagonal(return_matrices$primary_return_matrix, return_period), 
                                   secondary =  extractKthDiagonal(return_matrices$secondary_return_matrix, return_period))
      temp <- index(return_matrices$return_data)
      
      period_returns$date <- as.Date(temp[1:(length(temp) - (return_period - 1))])
      
      incProgress(0.3, message = "Cleaning, reshaping")
      period_returns$delta = period_returns$primary - period_returns$secondary
      
      period_returns_long <- tidyr::gather(period_returns, key = series, value = value,  primary, secondary, delta)
    })
    
    return(period_returns_long)
  })
  
  
  output$distPlot <- renderPlotly({
    withProgress(message = 'Plotting distribution (and world dominance)', style = 'notification', detail = "part 0", value = 0, {
      # generate bins based on input$bins from ui.R
      incProgress(amount = 0.3, message = "Fetching data")
      period_returns_long <- apply_period()
      
      incProgress(amount = 0.1, message = "Fetched data, processing")
      return_cat_seq <- 
        period_returns_long %>% 
        filter(series == "delta") %>%
        pull(value) %>%
        autocat()
      
      returns_diff_long_delta <- 
        period_returns_long %>% 
        filter(series == "delta") %>%
        mutate( return_cat_mp = midcut(value,
                                       from = return_cat_seq$breaks[1], 
                                       to = return_cat_seq$breaks[2], 
                                       by = return_cat_seq$breaks[3])) %>%
        group_by(return_cat_mp) %>% 
        mutate(counter = 1:n()) 
      incProgress(amount = 0.3, message = "Plotting")
      
      returns_diff_long_delta %>%
        distributionPlot(., 
                         pt = input$primary_ticker, 
                         st = input$secondary_ticker)
    })
  })
  
  output$arrayPlot <- renderPlotly({
    withProgress(message = 'Plotting array', style = 'notification', detail = "part 0", value = 0, {
      incProgress(amount = 0.3, message = "Fetching data")
      period_returns_long <- apply_period()
      
      incProgress(amount = 0.3, message = "Processing")
      
      returns_diff_long_delta <- 
        period_returns_long %>% 
        filter(series == "delta") 
      
      nobs <- nrow(returns_diff_long_delta)
      square_size <- ceiling(sqrt(nobs))
      
      incProgress(amount = 0.3, message = "Plotting")
      returns_diff_long_delta %>%
        ungroup() %>%
        arrange(value) %>% 
        mutate(nob = 1:n() -1, 
               modulo = nob / square_size, 
               mod_y = floor(modulo), 
               mod_x = round((modulo - mod_y) * square_size,0)
        ) %>%
        arrayPlot(., 
                  pt = input$primary_ticker, 
                  st = input$secondary_ticker)
    })
  })    
  
  
})
