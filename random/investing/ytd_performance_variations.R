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

# set base data path
kDataPath <- file.path(BASEPATH, "reports/investing/portfolio_performance","data")

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Collect Data ####
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

VASGX <- betterutils::getReturns("VASGX", from = "1990-01-01")

# Data starts in 1995, looking at full years only, so cut to 1995 onwards
start_date <- "1995-01-01"
VASGX <- VASGX[paste0(start_date, "/")] 


VASGX_tidy <- broom::tidy(VASGX)
VASGX_tidy <- VASGX_tidy %>% mutate(year = year(index), 
                                    yday = lubridate::yday(index))

# Cumulative return function
cumulRet <- function(x){
  cumprod(x+1) - 1
}

VASGX_tidy <- VASGX_tidy %>% group_by(year) %>% mutate(return_cumul = cumulRet(value), 
                                                       year_factor = as.factor(year))

t120_vals <- VASGX_tidy %>% filter(yday %in% seq(120,130)) %>% select(return_cumul, year) %>% summarise(return_cumul_t100 = mean(return_cumul))
VASGX_tidy <- merge(VASGX_tidy, t120_vals, by.x="year", by.y="year")

save(VASGX_tidy, file = file.path(kDataPath,"ytd_performance_variations.RData"))
#load("data/ytd_performance_variations.RData")

# Adapted from https://github.com/thomasp85/gganimate/wiki/Temperature-time-series
p <- ggplot(VASGX_tidy, aes(x=yday, y=return_cumul, group=year_factor, color=return_cumul_t100)) + 
  geom_line() + geom_hline(yintercept = 0)  + 
  scale_colour_gradient2(low = muted("red"), mid = "grey", high = muted("green"), midpoint = 0) + 
  geom_segment(aes(xend = 366, yend = return_cumul), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 368, label = year_factor), hjust = 0) + 
  transition_reveal(year_factor, yday) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'YTD Performance for Aggressive Allocation (VASGX)', y = 'Cumulative return', x="Day of year") + 
  theme_minimal() + theme(legend.position="none") + guides(color=FALSE) + 
  theme(plot.margin = margin(4, 10, 4, 4)) + scale_y_continuous(label=percent)   

gganimate::animate(p,  renderer = gifski_renderer(loop = F, ani.width= 900, ani.height=500, file = "ytd_performance.gif"),fps=10, duration = 18)  
#gganimate::animate(p,  width= 800, height=500, file = "ytd_performance.gif", duration = 12, units = "px" )  
