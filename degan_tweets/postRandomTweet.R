#' @name postRandomTweet 
#' @description Posts a random tweet from a category (optional) to twitter
#' @param category A category to filter the resulting tweets by.  \code{NULL} by default.
#' @param tweet_db Optional. A tweet database. 
#' @note ToDo: prevent from re-posting same previous tweet. 
#' @export
#' @examples 
#' \dontrun{
#' postRandomTweet()
#' }

postRandomTweet <- function(category=NULL, tweet_db=NULL){
  result <- getRandomTweets(category = category, tweet_db = tweet_db)
  twitteR::tweet(result, bypassCharLimit=TRUE)
  return(result)
}