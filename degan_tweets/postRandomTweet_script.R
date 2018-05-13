#' @author Daniel P. Egan
#' @description This is the script that is run as a cron job via postRandomTweet_script.sh

#' Ensure you have dependencies. 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(twitteR, yaml)

#' This is the script that is run as a cron job.
source("degan_tweets/getRandomTweet.R")
source("degan_tweets/postRandomTweet.R")

postRandomTweet()