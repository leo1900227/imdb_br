# 数据：imdb_belt_road, imdb_long,imdb_belt_road_long_box



colnames(imdb_long)
colnames(imdb_belt_road_long)

colnames(imdb_belt_road)
nrow(imdb_belt_road)



# 变量：belt_road

library(pacman)
p_load(data.table,tidyverse,xlsx,magrittr,corrplot)

setwd("/data1/zhiliang/RWorkSpace/one_belt_one_road/")

# 每个国家合作电影的比重，非合作电影的比重 ----------------------------------------------------
imdb_belt_road_long <- 
  imdb_long %>% 
  dplyr::filter(country %in% belt_road) %>% 
  select(-produce)



## 一带一路国家参与的电影的所有tt_num
tt_br <- 
  imdb_belt_road_long %>% 
  group_by(tt_num) %>% 
  summarise(n = n())



## “一带一路”沿线国家🐟独作🐟的电影的tt_num
tt_br_alone <- 
  tt_br %>% 
  filter(n == 1) %>% 
  .[1] %>% 
  as_vector()


## “一带一路”沿线国家🐟合作🐟的电影的tt_num
tt_br_col <- 
  tt_br %>% 
  filter(n != 1) %>% 
  .[1] %>% 
  as_vector()

“一带一路”战略区域
# 独作的电影，属于哪些国家的呢？ ---------------------------------------------------------
## 哪些电影是独作的呢？
imdb_br_alone <- 
  imdb_belt_road_long %>% 
  filter(tt_num %in% tt_br_alone)


nrow(imdb_br_alone)

## colnames(imdb_br_alone)
统计一下，不同国家独作的电影数量有多少呢？
tb_br_alone <- 
  imdb_br_alone %>% 
  group_by(country) %>% 
  summarise(n_alone = n()) %>% 
  mutate(
    mean_score_alone = mean(score, na.rm = T),
    mean_score_cnt_alone = mean(score_cnt, na.rm = T)
  ) %>% 
  arrange(desc(n_alone))

  
tb_br_alone

# country            n_alone
# <chr>          <int>
#   1 Turkey         13375
# 2 Philippines    12941
# 3 Russia         11970
# 4 China          10170
# 5 Hungary         9847
# 6 Poland          9444
# 7 Israel          6452
# 8 Czech Republic  5994
# 9 Iran            5287
# 10 Bulgaria        3005

# 合作的电影，有哪些国家都参加了呢？ -------------------------------------------------------
imdb_br_col <- 
  imdb_belt_road_long %>% 
  filter(tt_num %in% tt_br_col)

## 统计一下，不同国家参与国际合作的电影的数量有多少？
tb_br_col <- 
  imdb_br_col %>% 
  group_by(country) %>% 
  summarise(n_co
    l = n()) %>,
    mean_score_col = mean(score, na.rm = T),
    mean_score_cnt_col = mean(score_cnt, na.rm = T)
    % 
  arrange(desc(n_col))

tb_br_col
# A tibble: 54 x 2
# country            n_col
# <chr>          <int>
#   1 Russia           908
# 2 Czech Republic   607
# 3 Ukraine          552
# 4 Poland           465
# 5 Israel           444
# 6 Slovakia         408
# 7 China            366
# 8 Singapore        343
# 9 Hungary          316
# 10 Turkey           316

# 把独作电影与合作电影的数据合并一下 -------------------------------------------------------
tb_br_alone_col_count <- 
  left_join(tb_br_alone,tb_br_col,by = "country") %>% 
  mutate(
    col_prop = n_col/(n_alone + n_col)
  ) %>% 
  arrange(desc(col_prop)) %>% 
  as.data.table()


tb_br_alone_col_count %<>% 
  mutate(
    n = n_alone + n_col,
    mean_score = (mean_score_alone*n_alone+mean_score_col*n_col)/(n_alone + n_col),
    mean_score_cnt = (mean_score_cnt_alone*n_alone+mean_score_cnt_col*n_col)/(n_alone + n_col)
  )



tb_br_alone_col_count %>% 
  write.xlsx("./output/一带一路国家合作和独作对比.xlsx")


# “一带一路”沿线国家：合作与独作的对比 -----------------------------------------------------
attach(tb_br_alone_col_count)

names(tb_br_alone_col_count)
# [1] "country"              "n_alone"             
# [3] "mean_score_alone"     "mean_score_cnt_alone"
# [5] "n_col"                "mean_score_col"      
# [7] "mean_score_cnt_col"   "col_prop"            
# [9] "n"                    "mean_score"          
# [11] "mean_score_cnt"  

# >> 均分对比 -----------------------------------------------------------------
t.test(mean_score_alone,mean_score_col)

# data:  mean_score_alone and mean_score_col
# t = -1.3695, df = 95.242, p-value = 0.1741
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.30385298  0.05577075
# sample estimates:
#   mean of x mean of y 
# 6.827076  6.951117 


# >> 评分人次对比 ---------------------------------------------------------------
t.test(mean_score_cnt_alone,mean_score_cnt_col)

# Welch Two Sample t-test
# 
# data:  mean_score_cnt_alone and mean_score_cnt_col
# t = -2.5119, df = 99.574, p-value = 0.01361
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -709.52189  -83.30422
# sample estimates:
#   mean of x mean of y 
# 439.1711  835.5841 



# >> 数量对比 -----------------------------------------------------------------
t.test(n_alone,n_col)



# Welch Two Sample t-test
# 
# data:  n_alone and n_col
# t = 4.4193, df = 53.27, p-value = 4.898e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1166.496 3104.874
# sample estimates:
#   mean of x mean of y 
# 2305.4630  169.7778 


# >> >> t检验的总结 ------------------------------------------------------------
国际合作对评分提升有限，但有助于提升评分人数


# >> 合作越多，影响力越大吗？ ---------------------------------------------------------
cor(n,col_prop,use = "pairwise.complete.obs",method = "pearson")
# [1] -0.5280454
cor(mean_socre,col_prop,use = "pairwise.complete.obs",method = "pearson")
# [1] 0.15939
cor(mean_socre_cnt,col_prop,use = "pairwise.complete.obs",method = "pearson")
# [1] -0.1630997




detach(tb_br_alone_col_count)


