#' @name addNewTweetToDB 
#' @description Add a new tweet to the \code{\link{tweet_db}}. Automatically saved the database back down too
#' @note If inputs are not specified, it will ask in the console for input. 
#' @param tweet_text The text of the tweet.
#' @param category A category to assign the tweet to. 
#' @param tweet_db Optional, \code{NULL} by default. Allows you to pass an in-memory database in.
#' @export
#' @examples 
#' addNewTweetToDB("TestTweet", category="test")


addNewTweetToDB <- function(tweet_text=NULL, category=NULL, tweet_db=NULL, ask=TRUE){
  if(any(is.null(tweet_text), is.null(category))){
    tweet_text <- readline(prompt = "What is the tweet text?")
    category <- readline(prompt = "Which category?")
  }
  # load existing DB. 
  if(is.null(tweet_db)){
    if(file.exists("degan_tweets/tweet_db.Rda")){
      load("degan_tweets/tweet_db.Rda")  
    } else {
      setupnew_response <- askYesNo(msg = "Hmm. I don't see any database yet. Do you want me to set one up?")
      if(!setupnew_response){
        message("Ok, cancelling.")
        return()
      }
    }
    
  }
  if(!category %in% unique(tweet_db$category) & ask==TRUE){
    category_response <- askYesNo(msg = "This category doesn't exist yet. Are you sure you want to create a new one?")
    if(!category_response){return()}
  } 
  
  tweet_db <- rbind(tweet_db, data.frame(tweet_text = tweet_text, 
                                         category = category))
  
  save(tweet_db, file = "degan_tweets/tweet_db.Rda")  
}

