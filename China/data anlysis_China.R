# 分析中国相关的数据
# data: imdb_cn



library(purrr)
`%not_in%` <- purrr::negate(`%in%`)



# 中国参与的电影 -----------------------------------------------------------------
# 全部：
tt_cn <- 
  imdb_long %>% 
  as_tibble() %>% 
  filter(country == "China") %>% 
  .$tt_num %>% 
  as_vector()

length(tt_cn)
# [1] 8059


# 跟一带一路国家合作或独作
tt_br_cn <- 
  imdb_br_long %>% 
  as_tibble() %>% 
  filter(country == "China") %>% 
  .$tt_num %>% 
  as_vector()


# 筛选出中国独立完成的电影 ------------------------------------------------------------

# 独作电影(所有国家)
tt_alone <- 
  imdb_long %>% 
  group_by(tt_num) %>% 
  summarise(
    n = n()
  ) %>% 
  as_tibble() %>% 
  filter(n == 1) %>% 
  .$tt_num

# length(tt_alone)
# [1] 1126884


# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟

# 独作电影（one belt one road）

tt_br_alone <- 
  imdb_br_long %>% 
  group_by(tt_num) %>% 
  summarise(
    n = n()
  ) %>% 
  as_tibble() %>% 
  filter(n == 1) %>% 
  .$tt_num

tt_cn_alone <- 
  imdb_long %>% 
  filter(tt_num %in% tt_alone) %>%
  filter(country == "China") %>% 
  as_tibble() %>% 
  .$tt_num %>% 
  as_vector()

length(tt_cn_alone)
# [1] 5662
# colnames(imdb_cn_long)

# >>中国与别的地区合作的电影的tt_num ---------------------------------------------------
tt_cn_col <- 
  tt_cn[tt_cn %not_in% tt_cn_alone] 


length(tt_cn_col)
# [1] 2397


# 中国与One Belt One Road地区合作
tt_br_cn_col <- 
  tt_br_cn[tt_br_cn %not_in% tt_cn_alone]

length(tt_br_cn_col)
# [1] 1133



imdb_cn_col <- 
  imdb_long %>% 
  filter(tt_num %in% tt_cn_col) 


imdb_br_cn_col <- 
  imdb_long %>% 
  filter(tt_num %in% tt_br_cn_col) 




# 中国独立完成的电影的表现 -----------------------------------------------------------
imdb_cn_alone <- 
  imdb_long %>% 
  filter(tt_num %in% tt_cn_alone) 


mean(imdb_cn_alone$score,na.rm = T)
# [1] 6.169282

mean(imdb_cn_alone$score_cnt,na.rm = T)
# [1] 237.1085

mean(imdb_cn_alone$world_box_usd,na.rm = T)
# [1] 320693.1



# >> 独立完成的电影佳作一览 ----------------------------------------------------------
## 按票房
imdb_cn_alone %>% 
  arrange(desc(world_box_usd)) %>% 
  select(title) %>% 
  head()



# >> 中国独立完成的电影都是哪些类型的？ ----------------------------------------------------
# genre_vars <- colnames(imdb_long)[8:30]

imdb_cn_alone_genres <- 
  imdb_cn_alone %>% 
  gather(genre_vars,key = "genre",value = "n") %>%  #将多列的电影类型转成一列
  filter(n != 0);imdb_cn_alone_genres 



tb_cn_al <- 
  imdb_cn_alone_genres %>% 
  group_by(genre) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = prop.table(n))

tb_cn_al
# # A tibble: 21 x 3
# genre           n   prop
# <chr>       <int>  <dbl>
#   1 Drama        2460 0.288 
# 2 Short        1270 0.148 
# 3 Documentary   632 0.0739
# 4 Comedy        624 0.0730
# 5 Romance       510 0.0596
# 6 Action        472 0.0552
# 7 Family        336 0.0393
# 8 Animation     324 0.0379
# 9 Crime         310 0.0362
# 10 War           265 0.0310





# 中国合作电影表现 ----------------------------------------------------------------
mean(imdb_cn_col$score,na.rm = T)
# [1] 6.56943

mean(imdb_cn_col$score_cnt,na.rm = T)
# [1] 10541.34

mean(imdb_cn_col$world_box_usd,na.rm = T)
# [1] 608025.6


# >> 中国合作完成的电影都是哪些类型的？ ----------------------------------------------------

imdb_cn_col_genres <- 
  imdb_cn_col %>% 
  gather(genre_vars,key = "genre",value = "n") %>%  #将多列的电影类型转成一列
  filter(n != 0);imdb_cn_col_genres 


tb_cn_col <- 
  imdb_cn_col_genres %>% 
  group_by(genre) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = prop.table(n)) #算一下比例

tb_cn_col
# # A tibble: 21 x 3
# genre           n   prop
# <chr>       <int>  <dbl>
#   1 Documentary  2845 0.193 
# 2 Drama        2610 0.177 
# 3 Short        1656 0.112 
# 4 Adventure    1093 0.0742
# 5 Action       1020 0.0692
# 6 Biography     814 0.0552
# 7 Comedy        770 0.0523
# 8 Romance       594 0.0403
# 9 History       518 0.0352
# 10 Crime         498 0.0338

# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 与一带一路国家合作

imdb_br_cn_col_genres <- 
  imdb_br_cn_col %>% 
  gather(genre_vars,key = "genre",value = "n") %>%  #将多列的电影类型转成一列
  filter(n != 0);imdb_cn_col_genres 


tb_br_cn_col <- es %>% 
  group_by(genre) %br_>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = prop.table(n)) #算一下比例

tb_br_cn_col





# >> 小结 -------------------------# # A tibble: 20 x 3
# genre           n    prop
# <chr>       <int>   <dbl>
#   1 Drama         728 0.196  
# 2 Documentary   583 0.157  
# 3 Short         394 0.106  
# 4 Action        354 0.0951 
# 5 Adventure     281 0.0755 
# 6 Comedy        206 0.0553 
# 7 Romance       190 0.0510 
# 8 Biography     159 0.0427 
# 9 History       152 0.0408 
# 10 Crime         117 0.0314 
# 11 Family        105 0.0282 
# 12 Fantasy       103 0.0277 
# 13 Animation     102 0.0274 
# 14 Thriller       72 0.0193 
# 15 Mystery        45 0.0121 
# 16 War            38 0.0102 
# 17 Sci-Fi         33 0.00886
# 18 Horror         32 0.00860
# 19 Sport          25 0.00672
# 20 Western         4 0.00107----------------------------------------


## 简单总结：尽管中国独立完成的电影和中国合作完成的电影在评分上差异不大，但是合作电影得到的国际关注度远超独立完成的电影

## 中国独立完成的电影集中剧情片和短片，而合作的电影电影更多元

## 合作电影票房表现更佳

# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🎃哪些国家/地区和中国合作比较多，合作的表现是什么？ --------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟

tb_cn_col <- 
  imdb_cn_col %>% 
  group_by(country) %>% 
  summarise(
    n=n(),
    score=mean(score,na.rm = T),
    score_cnt=mean(score_cnt,na.rm = T),
    box=mean(world_box_usd,na.rm = T)
    ) %>% 
  arrange(desc(n)) %>% 
  mutate(belt_road= if_else(country %in% br_vars,"yes","no" )) %>% 
  filter(country != "China")



tb_br_cn_col <- 
  tb_cn_col %>% 
  filter(belt_road == "yes") %>% 
  left_join(.,country_info,by = "country")

# tb_br_cn_col %>% fwrite("../output/tb_br_cn_col.csv")


# >> 区域比较 -----------------------------------------------------------------
tb_br_cn_area_col <- 
  tb_br_cn_col %>% 
  group_by(area) %>% 
  summarise(
    n=sum(n),
    score=mean(score,na.rm = T),
    score_cnt=mean(score_cnt,na.rm = T),
    box=mean(box,na.rm = T)
  ) %>% 
  arrange(desc(n))
  
tb_br_cn_area_col

# area                    n score score_cnt     box
# <chr>               <int> <dbl>     <dbl>   <dbl>
#   1 Asia & Pacific        390  7.22      788.  36673.
# 2 Europe                289  7.33     1883. 836548.
# 3 Africa                118  7.43     3542.  55468.
# 4 South/Latin America    69  6.72      135.    NaN 
# 5 Middle east            53  7.09    15534.  61970.
# 6 Arab States            13  6.24      465.    NaN 


# >> 中国与“一带一路”沿线国家与非“一带一路”沿线国家合作数量 ----------------------------------------


tb_cn_col %>% 
  group_by(belt_road) %>% 
  summarise(n = n()) 

# belt_road     n
# <chr>     <int>
#   1 no           59
# 2 yes          99


# >> 和“一带一路”国家合作还是非“一带一路”国家合作更好？ ------------------------------------------

output_ttest_cn_col <- 
  table_cn_countries_col %>% 
  select_if(is.numeric) %>% 
  map_df(~ broom::tidy(t.test(. ~ as.factor(table_cn_countries_col$belt_road))), .id = 'var')
  
# output_ttest_cn_col %>% write_csv("./output/output_ttest_cn_col.csv")


# var	estimate	estimate1	estimate2	statistic	p.value
# n	33.958021	47.827586	13.869565	2.255878	0.0258644
# score	-0.2214389	6.948538	7.169977	-1.495284	0.1383218
# score_cnt	2438.296782	4142.059926	1703.763144	2.138837	0.0344031



## 中国的数据

# 1 China        3151  6.50     9817. yes  



# 选择出和中国有合作关系的国家
# imdb_cn_wide <- imdb_cn %>% 
#   select(1:7,which(map_lgl(., ~ var(.x) != 0)))
# 
# 
# colnames(imdb_cn_wide)
# 
# # 和中国有合作关系的所有国家
# imdb_cn_col_countries <- imdb_cn_wide[,c(31:192)]
# 
# 
# 
# colnames(imdb_cn_col_countries)
# 
# # 和中国的合作矩阵
# matrix_cn_col <- imdb_cn_col_countries  %>% as.matrix() %>% crossprod()
# 
# 
# matrix_cn_col %>% write.table("./test/t2.csv")
# 
# 
# dim(matrix_cn_col)
# # [1] 162 162
# 
# ## 中国和161个国家和地区有合作关系
# 
# 
# # 中国合作电影edge_list
# 
# list_cn <- reshape2::melt(matrix_cn_col) %>% 
#   filter(Var1 == "China" )
# 
# 
# reshape2::melt(matrix_cn_col) %>% write_csv("./test/test.csv")
# 
# 
# net_cn <- network(matrix_cn_col,matrix.type="adjacency")
# 
# 


# 从时间维度看中国的合作伙伴 -----------------------------------------------------------
table_cn_countries_yr_col <- 
  imdb_cn_col_long %>% 
  group_by(country,year) %>% 
  mutate(belt_road= if_else(country %in% br_countries,"yes","no" )) %>% 
  filter(country != "China") %>% 
  arrange(desc(year))
  

# >> 与中国合作的“一带一路”沿线国家（按年份排列） ----------------------------------------------

table_cn_countries_yr_col_br <- 
  table_cn_countries_yr_col %>% 
  dplyr::filter(belt_road == "yes") %>% 
  arrange(desc(year))


## 最早与中国开展合作的“一带一路”沿线国家
table_cn_countries_yr_col_br %>% 
  select(year,country) %>% 
  tail()

# year country    
# <dbl> <chr>      
#   1  1997 Poland     
# 2  1997 Russia     
# 3  1996 Singapore  
# 4  1995 Russia     
# 5  1987 Philippines
# 6  1987 Thailand   



# >> 与中国合作的非“一带一路”沿线国家（按年份排列） ----------------------------------------------
table_cn_countries_yr_col_Nbr <- 
  table_cn_countries_yr_col %>% 
  filter(belt_road == "no") %>% 
  arrange(desc(year))
  
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 代表作分析 -------------------------------------------------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟


# >> 独作电影 -----------------------------------------------------------------
imdb_long %>% 
  filter(tt_num %in% tt_cn_alone) %>% 
  arrange(-world_box_usd)


# >> 合作电影 -----------------------------------------------------------------
top_col <- c("Italy","Singapore","Thailand","Russia","Malaysia","Austria","Philippines","Indonesia","Poland","South Africa")

mv_col_top_cn <- 
  imdb_long %>% 
    filter(tt_num %in% tt_cn_col &
             country %in% top_col) %>% 
    arrange(-world_box_usd) 


mv_col_top_cn %>% fwrite("../output/mv_col_top_cn2.csv")
