#' @author Daniel P. Egan
#' @description This is the script that is run as a cron job via postRandomTweet.sh

#' Ensure you have dependencies. 
if(!require(pacman)) install.packages("pacman", repos='http://cran.us.r-project.org')
pacman::p_load(twitteR, yaml)

#' This is the script that is run as a cron job.
source("src/myscellany/degan_tweets/getRandomTweet.R")
source("src/myscellany/degan_tweets/postRandomTweet.R")

twitter_creds <- yaml.load_file("src/degan_creds.yaml")$twitter

setup_twitter_oauth(consumer_key = twitter_creds$consumer_key, 
                    consumer_secret = twitter_creds$consumer_secret, 
                    access_token = twitter_creds$access_token, 
                    access_secret = twitter_creds$access_token_secret)

result <- postRandomTweet()

write(paste0(result, ",     ", Sys.time()),
      file="src/myscellany/degan_tweets/log/tweet_log.txt",append=TRUE)

