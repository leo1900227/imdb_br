# 这段代码测试罗楠爬的数据的质量

library(tidyverse)
library(data.table)
library(here)
library(magrittr)

setwd("/data1/zhiliang/RWorkSpace/one_belt_one_road/data")


dt <- fread("imdb_0103.csv")

imdb <- dt

dt %>%
  select(title) %>% 
  filter(is.na(title)) %>% 
  nrow()


dt %>% 
  filter(tt_num == "tt6220972")

kong <- fread("/data1/zhiliang/imdb/jiexi/big_kong.csv",header = T)

kong_ttnum <- kong$tt_num %>% as_vector()


c("tt6415537") %in% kong_ttnum

colnames(dt[,c(2:13)])

dt[,c(1:7)] %>% 
  filter(tt_num %in% kong_ttnum) %>% 
 filter_at(vars(title,type,running_time,score,score_cnt,year),all_vars(!is.na(.))) %>% 
  select(tt_num) %>% 
  fwrite("/data1/zhiliang/fei_kong.csv")


