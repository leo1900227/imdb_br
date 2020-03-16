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
# is.data.frame(imdb_jiexi_selected)


imdb_jiexi_selected$box


# >> 处理美元符号 ---------------------------------------------------------------
imdb_jiexi_selected %<>% 
  mutate_all(funs(str_remove_all(., ","))) %>% 
    mutate(
      currency_budget = str_replace_all(Budget,"[0-9]","") %>% as.character(),
      budget = str_replace_all(Budget,"[^0-9]","") %>% as.numeric(),
      currency_box = str_replace_all(box,"[0-9]","") %>% as.character(),
      world_box = str_replace_all(box,"[^0-9]","") %>% as.numeric()#,
      # exchange_rate = if_else(currency == "P",0.0065,1),
      # world_box_usd = world_box * exchange_rate
    )


## 看看都有什么价格符号

tb_currency_box <- 
  imdb_jiexi_selected$currency_box %>% 
  table() %>% 
  as_tibble()

# .           n
# <chr>   <int>
#   1 ""    1918770
# 2 "$"     22463
# 3 "GBP"       1
# 4 "INR"      21


tb_currency_budget <- 
  imdb_jiexi_selected$currency_budget %>% 
  table() %>% 
  as_tibble() %>% 
  arrange(desc(n)) %>% 
  .[-1,]


tb_currency_budget %>% fwrite("/data1/zhiliang/RWorkSpace/one_belt_one_road/output/tb_currency_budget.csv")


# budget里面太多外国货币了，算了，不要了🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟


imdb_jiexi_selected %<>% 
  mutate(
     exchange_rate = case_when(
       currency_box == "GBP" ~ 1.3,
       currency_box == "INR" ~ 0.014,
       FALSE ~ 1
       ),
     world_box_usd = world_box * exchange_rate

  )


imdb_jiexi_selected$world_box_usd %>% table()

# .
# 252000    333200  463478.4    630000     7e+05   2100000   2800000 
# 1         1         1         1         1         1         1 
# 3080000   3220000   3257800   3500000   4480000   4900000   5040000 
# 1         1         1         1         1         1         1 
# 5460000   6160000     7e+06   7140000   7700000  10500000  11060000 
# 1         1         1         1         1         1         1 
# 448500000 
# 1 

imdb_jiexi_selected %>%
  filter(!is.na(world_box_usd)) %>% 
  nrow()

# 只有22行有数据！


# >> note:这数据质量不用要了！ 🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕🐕------------------------------------------------------





# 罗楠找的空的数据 ----------------------------------------------------------------
kong_vars <- 
  list.files(pattern = "kong_\\d{1,2}.csv") %>% 
  map_df(~fread(.,header = T)) %>% 
  .$tt_num %>% 
  as_vector()







