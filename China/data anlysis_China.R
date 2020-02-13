# 分析中国相关的数据
# data: imdb_cn


library(purrr)
`%not_in%` <- purrr::negate(`%in%`)



# 筛选出中国独立完成的电影 ------------------------------------------------------------

## 先转成long data，再根据tt_num来group_by，选出总数为1的
imdb_cn_long <- 
  imdb_cn %>% 
  gather(-non_countries_vars,key = "country",value = "produce") %>% 
  filter(produce != 0) %>% 
  select(-produce) 

# colnames(imdb_cn_long)

# >>中国与别的地区合作的电影的tt_num ---------------------------------------------------


cn_alone_tt_num <- 
  imdb_cn_long %>% 
  group_by(tt_num) %>% 
  summarise(n=n()) %>% 
  filter(n == 1) %>% 
  .$tt_num %>% 
  as.vector()

length(cn_alone_tt_num)
# [1] 7385



# >>有中国参与的电影的tt_num -------------------------------------------------------
cn_tt_num <- 
  imdb_cn$tt_num %>% 
  as.vector()


# >> 中国与别的地区合作的电影的tt_num --------------------------------------------------

cn_col_tt_num <- cn_tt_num[cn_tt_num %not_in% cn_alone_tt_num]

length(cn_col_tt_num)
# [1] 3151




imdb_cn_col <- 
  imdb_cn %>% 
  filter(tt_num %not_in% cn_alone_tt_num)

colnames(imdb_cn_col)

attr_vals <- colnames(imdb_cn_col)[1:32]

imdb_cn_col_long <- imdb_cn_col %>% 
  gather(-attr_vals,key = "country",value = "produce") %>% 
  filter(produce != 0) %>% 
  select(-produce) %>% 
  left_join(imdb_belt_road_long_box[,c(1,34)], by ="tt_num") %>%
  rename(box = world_box_usd)



colnames(imdb_cn_col_long)

# [1] "tt_num"       "title"        "type"        
# [4] "running_time" "score"        "score_cnt"   
# [7] "year"         "Action"       "Adult"       
# [10] "Adventure"    "Animation"    "Biography"   
# [13] "Comedy"       "Crime"        "Documentary" 
# [16] "Drama"        "Family"       "Fantasy"     
# [19] "Film-Noir"    "History"      "Horror"      
# [22] "Music"        "Musical"      "Mystery"     
# [25] "Romance"      "Sci-Fi"       "Short"       
# [28] "Sport"        "Thriller"     "War"         
# [31] "Western"      "TV"           "country" 


# 中国独立完成的电影的表现 ------------------------------------------------------------
imdb_cn_alone <- imdb_cn %>% .[,c(1:32)] %>%  # 排除国家数据
  filter(tt_num %in% cn_alone_tt_num) %>% 
  left_join(imdb_belt_road_long_box[,c(1,34)], by ="tt_num") %>%
  rename(box = world_box_usd)


mean(imdb_cn_alone$score,na.rm = T)
# [1] 6.383606

mean(imdb_cn_alone$score_cnt,na.rm = T)
# [1] 203.5546

mean(imdb_cn_alone$box,na.rm = T)
# [1] 24523787


colnames(imdb_cn_alone_genres)



# >> 独立完成的电影佳作一览 ----------------------------------------------------------
## 按票房
imdb_cn_alone %>% 
  arrange(desc(box)) %>% 
  select(title)



# >> 中国独立完成的电影都是哪些类型的？ ----------------------------------------------------


imdb_cn_alone_genres <- 
  imdb_cn_alone %>% 
  gather(genre_vars,key = "genre",value = "n") %>%  #将多列的电影类型转成一列
  filter(n != 0);imdb_cn_alone_genres 



table_cn_al <- 
  imdb_cn_alone_genres %>% 
  group_by(genre) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = prop.table(n))

# # A tibble: 23 x 3
# genre           n   prop
# <chr>       <int>  <dbl>
#   1 Drama        3140 0.286 
# 2 Short        1370 0.125 
# 3 Comedy        769 0.0700
# 4 Romance       760 0.0692
# 5 Documentary   726 0.0661
# 6 Action        617 0.0561
# 7 Animation     394 0.0359
# 8 Family        388 0.0353
# 9 Crime         374 0.0340
# 10 War           351 0.0319
# # … with 13 more rows




# 中国合作电影表现 ----------------------------------------------------------------


mean(imdb_cn_col$score,na.rm = T)
# [1] 6.495895

mean(imdb_cn_col$score_cnt,na.rm = T)
# [1] 9817.206

mean(imdb_cn_col$box,na.rm = T)
# [1] 43342734





# >> 中国合作完成的电影都是哪些类型的？ ----------------------------------------------------


imdb_cn_col_genres <- imdb_cn_col %>% 
  gather(genre_vars,key = "genre",value = "n") %>%  #将多列的电影类型转成一列
  filter(n != 0);imdb_cn_col_genres 





table_cn_col <- imdb_cn_col_genres %>% group_by(genre) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = prop.table(n)) #算一下比例

table_cn_col

# genre           n   prop
# <chr>       <int>  <dbl>
#   1 Drama        1047 0.145 
# 2 Documentary   866 0.120 
# 3 Comedy        843 0.117 
# 4 Short         708 0.0981
# 5 Family        706 0.0978
# 6 Animation     702 0.0973
# 7 Action        464 0.0643
# 8 Adventure     344 0.0477
# 9 Romance       275 0.0381
# 10 Biography     234 0.0324
# … with 13 more rows



# >> 小结 -------------------------------------------------------------------


## 简单总结：尽管中国独立完成的电影和中国合作完成的电影在评分上差异不大，但是合作电影得到的国际关注度远超独立完成的电影

## 中国独立完成的电影集中剧情片和短片，而合作的电影电影更多元

## 合作电影票房表现更佳


# 哪些国家/地区和中国合作比较多，合作的表现是什么？ -----------------------------------------------
table_cn_countries_col <- imdb_cn_col_long %>% 
  group_by(country) %>% 
  summarise(
    n=n(),
    score=mean(score,na.rm = T),
    score_cnt=mean(score_cnt,na.rm = T),
    box=mean(box,na.rm = T)
    ) %>% 
  arrange(desc(n)) %>% 
  mutate(belt_road= if_else(country %in% br_countries,"yes","no" )) %>% 
  filter(country != "China")

table_cn_countries_col

# >> 中国与“一带一路”沿线国家与非“一带一路”沿线国家合作数量 ----------------------------------------


##  
table_cn_countries_col %>% 
  group_by(belt_road) %>% 
  summarise(n = n()) 

# A tibble: 2 x 2
# belt_road     n
# <fct>     <int>
# 1 no          116
# 2 yes          46



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
  

