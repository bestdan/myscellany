#' @title run_crontwit.R
#' Ensure you have dependencies. 

if(!require(pacman)) install.packages("pacman", repos='http://cran.us.r-project.org')
pacman::p_load(twitteR, yaml, devtools)
if(!require(crontwit)){
  devtools::install_github("bestdan/crontwit")
}

# Nightly rebuild
if(as.numeric(format(Sys.time(), "%H")) == 1){
  devtools::install_github("bestdan/crontwit")
}
library(crontwit)


# Load paths
creds_path    <- "~/src/degan_creds.yaml"
schedule_path <- "~/src/myscellany/degan_tweets/schedule.rda"
tweet_db_path <- "~/src/myscellany/degan_tweets/tweet_db.rda"

twitter_creds <- yaml.load_file(creds_path)$twitter

# Tell it to auto-cache the credentials
options("httr_oauth_cache"=TRUE)
#getOption("httr_oauth_cache")
setup_twitter_oauth(consumer_key = twitter_creds$consumer_key, 
                    consumer_secret = twitter_creds$consumer_secret, 
                    access_token = twitter_creds$access_token, 
                    access_secret = twitter_creds$access_token_secret)

# Load schedule
load(schedule_path)
load(tweet_db_path)

# Check schedule and post if necessary. 
checkScheduleAndPost(schedule, tweet_db)\
