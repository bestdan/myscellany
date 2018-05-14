#!/bin/bash

{
    sudo R CMD BATCH ~/src/myscellany/degan_tweets/postRandomTweet_script.R
} >> ~/src/myscellany/degan_tweets/log/bash_log.txt
