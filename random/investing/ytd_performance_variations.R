####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### YTD Performance Variations 
###
### Notes:
###  Only works if you can source the returns information. I used proprietary source. 
### Primary creator(s): Daniel Egan
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(betterutils)
library(dplyr)
library(lubridate)
library(gganimate)
library(ggplot2)
library(scales)
library(gifski)
library(tweenr)
library(broom)
# set base data path
BASEPATH <- here::here()
kDataPath <- file.path(BASEPATH, "random/investing","data")
dir.create(kDataPath)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Collect Data ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

VASGX <- betterutils::getReturns("VASGX", from = "1990-01-01")

# Data starts in 1995, looking at full years only, so cut to 1995 onwards
start_date <- "1995-01-01"
VASGX <- VASGX[paste0(start_date, "/")] 

# One problem with this as it stands: it works in calendar days, so weekends/holidays
# cause random drop-outs in yday.
# Fill in weekends? 
full_match <- expand.grid(year = seq(1995, 2018), 
                          yday = seq(1, 365))


# Cumulative return function
cumulRet <- function(x){
  cumprod(x+1) - 1
}
  
VASGX_tidy <- VASGX %>% tidy %>% 
  mutate(year = year(index), 
         yday = lubridate::yday(index)) %>%
  merge(full_match, by=c("year","yday"), all = TRUE) %>% 
  filter(yday < 366) %>% filter(!(year == 2018 && yday > 340)) %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  group_by(year) %>% mutate(return_cumul = cumulRet(value)) %>% ungroup %>%
  mutate(year_factor = as.factor(year))

t120_vals <- VASGX_tidy %>% filter(yday %in% seq(120,130)) %>% group_by(year) %>% select(return_cumul, year) %>% summarise(return_cumul_t100 = mean(return_cumul)) %>% ungroup()
VASGX_tidy <- merge(VASGX_tidy, t120_vals, by="year", all.x = TRUE)

save(VASGX_tidy, file = file.path(kDataPath,"ytd_performance_variations.RData"))
#VASGX_tidy %>% group_by(year, yday) %>% tally() %>% data.frame() %>% spread(year, n, fill = 0, sep=".") %>% View()
#table(VASGX_tidy$year_factor, useNA = 'always')
  
#load("data/ytd_performance_variations.RData")

# Time series by year of performance
# Adapted from https://github.com/thomasp85/gganimate/wiki/Temperature-time-series
p <- ggplot(VASGX_tidy, aes(x=yday, y=return_cumul, group=year_factor, color=return_cumul_t100)) + 
  geom_line() + geom_hline(yintercept = 0)  + 
  scale_colour_gradient2(low = muted("red"), mid = "grey", high = muted("green"), midpoint = 0) + 
  geom_segment(aes(xend = 365, yend = return_cumul), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 368, label = year_factor), hjust = 0) + 
  transition_reveal(year_factor, yday) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'YTD Performance for Aggressive Allocation (VASGX)', y = 'Cumulative return', x="Day of year") + 
  theme_minimal() + theme(legend.position="none") + guides(color=FALSE) + 
  theme(plot.margin = margin(4, 10, 4, 4)) + scale_y_continuous(label=percent)   

gganimate::animate(p,  renderer = gifski_renderer(loop = F, width= 900, height=500, file = "ytd_performance.gif"),fps=10, duration = 18)  


# Full props to @HowYaDoing
# https://stackoverflow.com/questions/22312207/how-to-assign-cut-range-midpoints-in-r

midcut<-function(x,from,to,by){
  ## cut the data into bins...
  x=cut(x,seq(from,to,by),include.lowest=T)
  ## make a named vector of the midpoints, names=binnames
  vec=seq(from+by/2,to-by/2,by)
  names(vec)=levels(x)
  ## use the vector to map the names of the bins to the midpoint values
  unname(vec[x])
}

# Horizontal barchar
breaks <- seq(-1,0.50, 0.03)
VASGX_tidy <- VASGX_tidy %>% 
  mutate(return_cumul_cat = cut(return_cumul, breaks = breaks), 
         return_cumul_cat_mp = midcut(return_cumul, -1, 0.50, 0.03)) %>%
  group_by(yday, return_cumul_cat) %>% 
  mutate(return_cumul_cat_n = row_number()) %>% 
  ungroup()

p <- ggplot(VASGX_tidy, aes(x=return_cumul_cat_n, y=return_cumul_cat_mp)) + 
  geom_tile(aes(fill=year_factor), col="white") + geom_text(aes(label = year_factor), hjust = 0.5, col="white", size =3, check_overlap = TRUE) + 
  labs(title = 'YTD Performance for Aggressive Allocation (VASGX)', x = 'Frequency', y="Cumulative return", subtitle = "Day of Year: {round(frame_time)}") + 
  scale_y_continuous(label=percent) + 
  theme_minimal() + theme(legend.position="none") + guides(color=FALSE) + transition_time(yday)

gganimate::animate(p,  renderer = gifski_renderer(loop = F, file = "ytd_performance_hist.gif"), height = 500, width = 800, nframes = length(unique(VASGX_tidy$yday)), fps = 20)  


# Attempt at Tweenr
# Failing so far
VASGX_tidy <- VASGX_tidy %>% arrange(year_factor, yday) %>% mutate(ease = "linear")
ungroup(VASGX_tidy)
VASGX_tidy_tween <- tween_elements(VASGX_tidy,time =  "yday", group = "year_factor", ease = "ease", nframes = 600) %>%
  mutate(yday = round(yday), year_factor = .group) 
str(VASGX_tidy)
head(VASGX_tidy_tween, 120)

p <- ggplot(VASGX_tidy_tween, aes(x=return_cumul_cat, y=return_cumul_cat_n)) + 
  geom_tile(aes(fill=year_factor), col="white") + geom_text(aes(label = year_factor), hjust = 0.5, col="white", size = 2) + 
  scale_colour_gradient2(low = muted("red"), mid = "grey", high = muted("green"), midpoint = 0) + 
  labs(title = 'YTD Performance for Aggressive Allocation (VASGX)', y = 'Frequency', x="Cumulative return", subtitle = "Day of Year: {round(frame_time)}") + 
  theme_minimal() + theme(legend.position="none") + guides(color=FALSE) + transition_time(.frame)

gganimate::animate(p,  renderer = gifski_renderer(loop = F, width= 900, height=500, file = "ytd_performance_hist.gif"), duration = 12)  

