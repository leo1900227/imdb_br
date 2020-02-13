# data: imdb_belt_road_long_box


# export:
# tb_imdb_belt_road_box


colnames(imdb_belt_road_long_box)


# table -------------------------------------------------------------------
# 

tb_br



tb_imdb_belt_road_box <- 
  imdb_belt_road_long_box %>% 
  group_by(country) %>% 
  summarise(box_sum = sum(world_box_usd, na.rm = TRUE)) %>%   arrange(desc(box_sum))

tb_imdb_belt_road_box_nan <- 
  imdb_belt_road_long_box %>% 
  group_by(country) %>% 
  summarise(
    na = sum(is.na(world_box_usd)),
    non_na = sum(!is.na(world_box_usd))
    )
 
tb_imdb_belt_road_box_nan %>% write_csv("/data1/zhiliang/RWorkSpace/one_belt_one_road/datatb_imdb_belt_road_box_nan.csv")

# 年份-国家-NA
tb_imdb_belt_road_box_yr_nan <- 
  imdb_belt_road_long_box %>% 
  group_by(country,year) %>% 
  summarise(
    na = sum(is.na(world_box_usd)),
    non_na = sum(!is.na(world_box_usd))
  ) %>% 
  arrange(desc(year))

tb_imdb_belt_road_box_yr_nan %>% write_csv("/data1/zhiliang/RWorkSpace/one_belt_one_road/datatb_imdb_belt_road_box_yr_nan.csv")





# 票房最高的电影 -----------------------------------------------------------------
imdb_belt_road_long_box %>% 
  select(country,world_box_usd,tt_num,year) %>% 
  arrange(desc(world_box_usd)) %>% 
  head(20)

# country world_box_usd    tt_num
# 1           China    1515047671 tt2820852
# 2        Thailand     868390560 tt0121766
# 3           China     791115104 tt4912910
# 4           China     553810228 tt4701660
# 5           China     511595957 tt2709692
# 6           China     228122928 tt5061814
# 7           China     227091290 tt6654316
# 8           China     213525736 tt0190332
# 9           China     182206924 tt1717715
# 10       Bulgaria     180650747 tt1959563
# 11 Czech Republic     179265204 tt0311429
# 12          China     177395557 tt0299977
# 13          China     173839072 tt6044910
# 14          China     145819137 tt5354664
# 15          China     125837070 tt2909116
# 16          China     119107580 tt6074202
# 17          China     106980263 tt3667798
# 18          China     106380000 tt4819498
# 19          China     103197501 tt5112622
# 20          China     102205175 tt4687848


# 票房最高的国家 -----------------------------------------------------------------
tb_belt_road__box_ranking <- 
  imdb_belt_road_long_box %>% 
  group_by(country) %>% 
  summarise(sum_box = sum(world_box_usd, na.rm = T)) %>%
  arrange(desc(sum_box))



“一带一路”战略区域
# 独作的电影，属于哪些国家的呢？ ---------------------------------------------------------
## 哪些电影是独作的呢？
imdb_br_alone <- 
  imdb_belt_road_long_box %>% 
  filter(tt_num %in% tt_br_alone)


nrow(imdb_br_alone)



colnames(imdb_br_alone)
# 统计一下，不同国家独作的电影数量有多少呢？
tb_br_alone <- 
  imdb_br_alone %>% 
  group_by(country) %>% 
  summarise(
    n_alone = n(),
    mean_score_alone = mean(score, na.rm = T),
    mean_score_cnt_alone = mean(score_cnt, na.rm = T),
    sum_box_alone = sum(world_box_usd, na.rm = T)
  ) %>% 
  arrange(desc(n_alone))

# tb_br_alone$sum_box_alone

# colnames(tb_br_alone)
# [1] "country"              "n_alone"             
# [3] "mean_score_alone"     "mean_score_cnt_alone"
# [5] "sum_box_alone"

# 合作的电影，有哪些国家都参加了呢？ -------------------------------------------------------
imdb_br_col <- 
  imdb_belt_road_long_box %>% 
  filter(tt_num %in% tt_br_col)

## 统计一下，不同国家参与国际合作的电影的数量有多少？
tb_br_col <- 
  imdb_br_col %>% 
  group_by(country) %>% 
  summarise(
    n_col = n(),
    mean_score_col = mean(score, na.rm = T),
    mean_score_cnt_col = mean(score_cnt, na.rm = T),
    sum_box_col = sum(world_box_usd, na.rm = T)
  ) %>% 
  arrange(desc(n_col))


# # colnames(tb_br_col)

# 把独作电影与合作电影的数据合并一下 -------------------------------------------------------
tb_br_alone_col_count <- 
  left_join(tb_br_alone,tb_br_col,by = "country") %>% 
  mutate(
    col_prop = n_col/(n_alone + n_col)
  ) %>% 
  arrange(desc(col_prop)) %>% 
  as.data.table()

# colnames(tb_br_alone_col_count)
# [1] "country"              "n_alone"             
# [3] "mean_score_alone"     "mean_score_cnt_alone"
# [5] "sum_box_alone"        "n_col"               
# [7] "mean_score_col"       "mean_score_cnt_col"  
# [9] "sum_box_col"          "col_prop"  

tb_br_alone_col_count %<>% 
  mutate(
    n = n_alone + n_col,
    mean_score = (mean_score_alone*n_alone+mean_score_col*n_col)/(n_alone + n_col),
    mean_score_cnt = (mean_score_cnt_alone*n_alone+mean_score_cnt_col*n_col)/(n_alone + n_col),
    sum_box = sum(sum_box_alone + sum_box_col)
  ) 

tb_br_alone_col_count %>% 
  write.xlsx("./output/一带一路国家合作和独作对比.xlsx")


# “一带一路”沿线国家：合作与独作的对比 -----------------------------------------------------
attach(tb_br_alone_col_count)
# detach(tb_br_alone_col_count)


names(tb_br_alone_col_count)
# [1] "country"              "n_alone"             
# [3] "mean_score_alone"     "mean_score_cnt_alone"
# [5] "sum_box_alone"        "n_col"               
# [7] "mean_score_col"       "mean_score_cnt_col"  
# [9] "sum_box_col"          "col_prop"            
# [11] "n"                    "mean_score"          
# [13] "mean_score_cnt"       "sum_box" 



# >> 票房对比 -----------------------------------------------------------------
t.test(sum_box_alone,sum_box_col)

# Welch Two Sample t-test
# 
# data:  sum_box_alone and sum_box_col
# t = 1.2885, df = 53.01, p-value = 0.2032
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -100625057  462155228
# sample estimates:
#   mean of x mean of y 
# 186040978   5275892

> 均分对比 -----------------------------------------------------------------
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
国际合作对评分提升有限，但有助于提升评分人数，对票房帮助巨大


# >> 合作越多，影响力越大吗？ ---------------------------------------------------------
cor(n,col_prop,use = "pairwise.complete.obs",method = "pearson")
# [1] -0.5280454
cor(mean_socre,col_prop,use = "pairwise.complete.obs",method = "pearson")
# [1] 0.15939
cor(mean_socre_cnt,col_prop,use = "pairwise.complete.obs",method = "pearson")
# [1] -0.1630997




detach(tb_br_alone_col_count)



