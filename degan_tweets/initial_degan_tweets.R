
if(!require(pacman)) install.packages(pacman)
pacman::p_load("twitteR", "yaml")


setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)


# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
twitter_creds <- yaml.load_file("~/src/degan_creds.yaml")$twitter

setup_twitter_oauth(consumer_key = twitter_creds$consumer_key, 
                    consumer_secret = twitter_creds$consumer_secret, 
                    access_token = twitter_creds$access_token, 
                    access_secret = twitter_creds$access_token_secret)

tw = twitteR::searchTwitter('from:@daniel_egan', n = 1e4, retryOnRateLimit = 1e3)
tw = twitteR::searchTwitter('from:@daniel_egan', n = 1e4,until = '2018-01-01',since = "2017-01-01", retryOnRateLimit = 1e3)


tw = twitteR::searchTwitter('#BeFiFails', n = 1e4, since = "2018-01-01", retryOnRateLimit = 1e3)
d = twitteR::twListToDF(tw)

d$text
table(d$statusSource)
?register_sqlite_backend("")
tweets = searchTwitter("#scala")
store_tweets_db(tweets)
from_db = load_tweets_db()


twitteR::tweet("testing")