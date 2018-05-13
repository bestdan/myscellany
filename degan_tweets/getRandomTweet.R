#' @name getRandomTweets 
#' @description Get a random set of tweets from the \code{\link{tweet_db}}. 
#' @param n How many tweets do you want? 
#' @param search_text A string to filter the resulting tweets by. \code{NULL} by default.
#' @param category A category to filter the resulting tweets by.  \code{NULL} by default.
#' @param tweet_db Optional, \code{NULL} by default. Allows you to pass an in-memory database in.
#' @export
#' @examples 
#' res<- getRandomTweets(search_text="#BeFiFails")

getRandomTweets <- function(n=1, search_text=NULL, category=NULL, tweet_db=NULL){
 if(is.null(tweet_db)){
   load("degan_tweets/tweet_db.Rda")
 }
  
  if(!is.null(category)){
    res <- tweet_db[tweet_db$category == category,]
  } else {
    res <- tweet_db
  }
  
  if(!is.null(search_text)){
    res <- res[grepl(pattern = search_text, x = res$tweet_text),]
  } 
  
  res_tweets <- sample(res$tweet_text, size = n, replace = FALSE)
  
  return(res_tweets)
}