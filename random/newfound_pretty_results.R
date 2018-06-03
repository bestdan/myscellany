rm(list=ls())
library(rvest)
library(dplyr)
library(ggplot2)
page_url <- "https://blog.thinknewfound.com/2018/05/dollar-cost-averaging-improved-by-trend/"
ref_page <- page_url %>% read_html() 

strategies <- c("momentum", "slope", "mar", "zscore", "rsi", "vol")
table_index_df <- data.frame(strategy = strategies, 
                             xpath = c('//*[@id="post-6185"]/div[1]/div[2]/table[3]', 
                                       '//*[@id="post-6185"]/div[1]/div[2]/table[5]',
                                       '//*[@id="post-6185"]/div[1]/div[2]/table[7]',
                                       '//*[@id="post-6185"]/div[1]/div[2]/table[9]',
                                       '//*[@id="post-6185"]/div[1]/div[2]/table[11]',
                                       '//*[@id="post-6185"]/div[1]/div[2]/table[13]'), 
                             data_type = rep("regtable", 6))


hitrate_table_index <- data.frame(strategy = strategies,
                                  xpath = c('//*[@id="post-6185"]/div[1]/div[2]/table[4]', 
                                            '//*[@id="post-6185"]/div[1]/div[2]/table[6]',
                                            '//*[@id="post-6185"]/div[1]/div[2]/table[8]',
                                            '//*[@id="post-6185"]/div[1]/div[2]/table[10]',
                                            '//*[@id="post-6185"]/div[1]/div[2]/table[12]',
                                            '//*[@id="post-6185"]/div[1]/div[2]/table[14]'), 
                                  data_type = rep("hitrate", 6))

table_index_df <- rbind(table_index_df, hitrate_table_index)
rm(hitrate_table_index)

cleanTable <- function(x){
  names(x) <- x[1,]
  x <- x[-1,-1]
  return(x)
}


extractStatsTable <- function(ref_table){
  table_percent <- as.data.frame(matrix(NA, nrow=nrow(ref_table), ncol=ncol(ref_table)))
  table_significance <- table_percent
  table_count <- table_percent
  
  for(i in 1:ncol(ref_table)){
    temp <- strsplit(ref_table[,i], split = "%")
    table_percent[,i]  <- as.vector(unlist(lapply(temp, function(x) as.numeric(x[1]))))
    temp <- lapply(temp, function(x) strsplit(x[2], split = " "))
    table_significance[,i]  <- as.vector(unlist(
      lapply(temp, function(x) x[[1]][1])
    ))
    
    table_count[,i]  <- as.vector(unlist(
      lapply(temp, function(x) as.numeric(
        gsub("\\(","",gsub(")", "",x[[1]][2]))
      )
      )
    ))
    
    
  }
  
  names(table_percent)[1:5] <- c("4W", "3M", "6M", "12M", "4Q")
  names(table_significance)[1:5] <- c("4W", "3M", "6M", "12M", "4Q")
  names(table_count)[1:5] <- c("4W", "3M", "6M", "12M", "4Q")
  
  table_percent$rindex <- 1:nrow(table_percent)
  table_significance$rindex <- 1:nrow(table_significance)
  table_count$rindex <- 1:nrow(table_count)
  
  return(list(percent = table_percent,
              significance = table_significance, 
              count = table_count))  
}


extractHRTable <- function(ref_table){
  clean_table <- as.data.frame(apply(ref_table, 2, function(x) as.numeric(gsub("%", "", x))) / 100)
  clean_table_offset <- clean_table - 0.50
  
  
  clean_table$rindex <- 1:nrow(clean_table)
  clean_table_offset$rindex <- 1:nrow(clean_table)
  
  return(list(hr = clean_table,
              hr_offset = clean_table_offset))  
}

datalist_regs <- c("percent", "significance", "count")
datalist_hr <- c("hr", "hr_offset")
datalist <- c(datalist_regs, datalist_hr)
for(dname in datalist){
  assign(paste0("all_", dname), data.frame())
}


# Collate results into long format
for(i in 1:length(strategies)){

  stat_name <- strategies[i]
  
  temp_results_name <- paste0("temp_results_", stat_name)
  
  # Perform the data extraction and formatting of regression results
  this_xpath <- table_index_df %>% filter(strategy == stat_name, data_type == "regtable") %>% pull(xpath) %>% as.character()
  ref_page  %>% 
    html_nodes(xpath = this_xpath) %>%
    html_table() %>% .[[1]] %>% 
    cleanTable %>% 
    extractStatsTable %>% 
    assign(x = paste0(temp_results_name,"_regs"), value = ., pos = 1) 
  
  # Hitrate Table
  this_xpath <- table_index_df %>% filter(strategy == stat_name, data_type == "hitrate") %>% pull(xpath) %>% as.character()
  ref_page  %>% 
    html_nodes(xpath = this_xpath) %>%
    html_table() %>% .[[1]] %>% 
    cleanTable %>% 
    extractHRTable %>% 
    assign(x = paste0(temp_results_name,"_hr"), value = ., pos = 1) 
  
  
  # Unpack and move to long
  # Regs
  for(dname in datalist_regs){
    all_name <- paste0("all_",dname)
    tx <- get(paste0(temp_results_name, "_regs"))[[dname]]
    tx$stat <- stat_name
    assign(all_name, rbind(get(all_name),tx))
    rm(tx, all_name)
  }
  
  # Hit rates
  for(dname in datalist_hr){
    all_name <- paste0("all_",dname)
    tx <- get(paste0(temp_results_name, "_hr"))[[dname]]
    tx$stat <- stat_name
    assign(all_name, rbind(get(all_name),tx))
    rm(tx, all_name)
  }
 rm(list = c(paste0(temp_results_name,"_hr"), paste0(temp_results_name,"_regs")))
}

for(dl in datalist){
  temp <- get(paste0("all_", dl))
  temp$type <- dl
  assign(paste0("all_", dl), temp)
  rm(temp)
}


results_wide <- do.call(rbind, args = lapply(ls(pattern = "all"), get))
results_long <- results_wide %>% gather(time, value, -rindex, -stat, -type)
results_long <- results_long %>% spread(key = type, value = value)
str(results_long)
results_long$count <- as.numeric(results_long$count)
results_long$percent <- as.numeric(results_long$percent)
results_long$hr <- as.numeric(results_long$hr)
results_long$hr_offset <- as.numeric(results_long$hr_offset)
results_long$time <- factor(results_long$time, levels = c("4W", "3M", "6M", "4Q","12M"))
results_long$stat <- as.factor(results_long$stat)

results_long <- results_long %>% group_by(stat, time)


ps_dodge <- 1
ggplot(results_long) + 
  geom_bar(   aes(x=time, y=hr, group=rindex), width = 0.8, stat="identity", alpha = 0.3, position = position_dodge(ps_dodge)) +
  geom_point( aes(x=time, y=percent, group=rindex), alpha = 0.3,  position = position_dodge(ps_dodge* 0.9)) +  # , color=rindex
  geom_line(  aes(x=time, y=percent, group=rindex), alpha = 0.3,  position = position_dodge(ps_dodge* 0.9)) +
  geom_text(  aes(x=time, y=percent, group=rindex, label = significance), color = "orange",position = position_dodge(ps_dodge* 0.9)) +
  geom_hline(yintercept = 0.50, col="dark grey") +
  facet_grid(. ~ stat)
