library(crontwit)

schedule <- data.frame(minute=numeric(), 
                       hour=numeric(), 
                       dow=numeric(), 
                       category=character(), 
                       id=character())

schedule <- rbind(schedule, 
                  data.frame(minute   = 05, 
                             hour     = 12, 
                             dow      = 1, 
                             category = "dpegan_blog_posts", 
                             id       = NA))

schedule <- rbind(schedule, 
                  data.frame(minute   = 05, 
                             hour     = 12, 
                             dow      = 3, 
                             category = "dpegan_blog_posts", 
                             id       = NA))

schedule <- rbind(schedule, 
                  data.frame(minute   = 05, 
                             hour     = 12, 
                             dow      = 5, 
                             category = "dpegan_blog_posts", 
                             id       = NA))

schedule <- rbind(schedule, 
                  data.frame(minute   = 05, 
                             hour     = 08, 
                             dow      = 00, 
                             category = "befi_fails", 
                             id       = NA))

schedule <- rbind(schedule, 
                  data.frame(minute   = 05, 
                             hour     = 08, 
                             dow      = 02, 
                             category = "betterment_posts", 
                             id       = NA))

schedule <- rbind(schedule, 
                  data.frame(minute   = 05, 
                             hour     = 08, 
                             dow      = 04, 
                             category = "betterment_posts", 
                             id       = NA))

schedule <- rbind(schedule, 
                  data.frame(minute   = 05, 
                             hour     = 16, 
                             dow      = 06, 
                             category = "fintwit_quotes", 
                             id       = NA))

validateSchedule(schedule)
save(schedule, file = "degan_tweets/schedule.rda")
