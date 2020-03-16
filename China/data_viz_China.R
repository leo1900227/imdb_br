library(pacman)

p_load(tidyverse,magrittr)

# 中国的国际化合作：历年变化 -----------------------------------------------------------

# data:imdb_cn_long

tb_cn_yr_countries <- imdb_cn_long %>% 
  dplyr::filter(country != "China") %>% 
  group_by(year,country) %>% 
  summarise(n= n()) %>% 
  drop_na()

colnames(tb_cn_yr_countries)
# [1] "year"    "country" "n"      



tb_cn_yr_countries$year %<>% lubridate::ymd(truncated = 2L) %>% lubridate::year()

tb_cn_yr_countries$country %<>% as.factor()


## 增加一列，判断是否是一带一路国家
colnames(tb_cn_yr_countries)

tb_cn_yr_countries %<>% mutate(
  belt_road = if_else(country %in%  br_countries, "yes","no")
)



## 堆叠图：看从历史来看中国的合作变化有哪些变化。

### 🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱


p_cn_yr_countries <-
  tb_cn_yr_countries %>%
   drop_na() %>%
   ggplot() + geom_line(aes(year,log(n),group = country,color=belt_road)) +
   scale_x_continuous(breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) + 
  labs(x="年份",
       y ="计数（log）",
       title  =  "中国电影国际合作伙伴历年变化图") ;


ggsave("./pic/中国电影国际合作伙伴历年变化图.png")


# 
# 
# tb_cn_yr_countries %>%
#   drop_na() %>%
#   ggplot() + geom_bin2d(aes(year,log(n),group = country,fill=belt_road)) +
#   scale_x_continuous(breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) + labs(x="年份",y ="计数（log）",title  =  "中国电影国际合作伙伴历年变化图") ;
# 
# 
# ggsave("./pic/中国电影国际合作伙伴历年变化图.png")
