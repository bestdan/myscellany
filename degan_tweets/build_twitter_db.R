


addNewTweet <- function(tweet_text=NULL, category=NULL, tweet_db=NULL){
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
  if(!category %in% unique(tweet_db$category)){
      category_response <- askYesNo(msg = "This category doesn't exist yet. Are you sure you want to create a new one?")
    if(!category_response){return()}
  } 
  
  tweet_db <- rbind(tweet_db, data.frame(tweet_text = tweet_text, 
                                          category = category))
  
  save(tweet_db, file = "degan_tweets/tweet_db.Rda")  
}



addNewTweet(tweet_text=c("Should we change the portfolio, or the investor?\n  http://www.dpegan.com/change_portfolio_or_investor/"),
            category = "dpegan_blog_posts")


addNewTweet(tweet_text = "'Now that I've constructed a diversified portfolio, I'll track the performance of each individual component separately.'
\n\n
            #BeFiFails", 
            category = "befi_fails")

addNewTweet(tweet_text = "Goal based investing is great, but always keep an eye on the big picture.\n\n
            http://www.dpegan.com/goal-based-investing-big-picture/", 
            category = "dpegan_blog_posts")


addNewTweet(tweet_text = "Historically, past performance has not been predictive of future performance. I expect this to continue.\n\n
            #BeFiFails", 
            category = "befi_fails")

addNewTweet(tweet_text = "Data shows that women are better (behaved) investors.\n\n
            https://www.betterment.com/resources/data-suggests-women-are-better-behaved-investors/", 
            category = "betterment_posts")
