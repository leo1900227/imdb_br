# 看一下转出来的adjacency matrix怎么分析

library(pacman)
p_load(tidyverse)


# 读取数据 --------------------------------------------------------------------

df_genres <- read_csv("df_genres.csv")
df_place_of_production <- read_csv("df_place_of_production.csv")


# 开始清理 --------------------------------------------------------------------
df_genres %>% names() %>% View()
## 杂乱数据非常多，暴力一点的办法是乱七八糟的数据直接扔掉算了

## 先选出非电视节目类的
df_genres_non_TV <- df_genres %>% select(Action,Adult,Adventure,Animation,Biography,Comedy,Crime,Documentary,Drama,Family,Fantasy,"Film-Noir",History,Horror,Music,Musical,Mystery,Romance,"Sci-Fi",Short,Sport,Thriller,War,Western)

## 找出电视类的，并最终合成一列（列相加，最后大于1转化为1，否则为0）
df_genres__TV <- df_genres %>% 
  select(starts_with("TV"),"Reality-TV","Talk-Show",News,"Game-Show",Musical) %>% 
  mutate(TV = rowSums(.)) %>% 
  select(TV) %>% 
  mutate(TV = if_else(TV != 0,1,0)
           )

new_df_genres <- cbind(df_genres_non_TV,df_genres__TV)

dim(new_df_genres)
## 25种类型
colnames(new_df_genres)

df_place_of_production %>% names()
## 国家层面的数据非常干净



# 重新合成一个新的数据集 -------------------------------------------------------------
imdb <- read_csv("imdb_1224.csv")

colnames(imdb)

imdb_0103 <- cbind(imdb,new_df_genres,df_place_of_production) %>% select(-c(genres,place_of_production))

colnames(imdb_0103)


write_csv(imdb_0103,"imdb_0103.csv")



