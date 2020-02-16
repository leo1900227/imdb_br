# 把罗楠新爬虫的数据导进来

library(tidyverse)
library(data.table)
library(here)
library(magrittr)



# 10个服务器找出来的数据 ------------------------------------------------------------
# 地址：
setwd("/data1/zhiliang/imdb/jiexi/")

imdb_jiexi <-
  list.files(pattern = "imdb_jiexi_\\d{1,2}.csv") %>% 
  map_df(~fread(.)) # 带合并


# 瞧瞧这数据长什么样子
dim(imdb_jiexi)
# [1] 1941255      23

colnames(imdb_jiexi)


# ⭐ ⭐ ⭐ ⭐ ⭐ ⭐ ⭐ imdb_jiexi_selected⭐ ⭐ ⭐ ⭐ ⭐ ⭐ ⭐ 


imdb_jiexi_selected <- 
  imdb_jiexi %>% 
  select(tt_num,score,score_cnt,Budget,Cumulative_Worldwide_Gross) %>% 
  mutate(box = Cumulative_Worldwide_Gross)


# assertthat::
is.data.frame(imdb_jiexi_selected)


# 罗楠找的空的数据 ----------------------------------------------------------------
kong_vars <- 
  list.files(pattern = "kong_\\d{1,2}.csv") %>% 
  map_df(~fread(.,header = T)) %>% 
  .$tt_num %>% 
  as_vector()







