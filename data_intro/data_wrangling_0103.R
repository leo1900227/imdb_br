library(pacman)
p_load(tidyverse)

imdb <- read_csv("imdb_0103.csv")

##前32个变量不是关于国家的，后面的全是
non_countries_vars <- colnames(imdb) %>% .[1:32]
countries_vars <- colnames(imdb) %>% .[33:275]

## 电影类型变量
genre_vars <- colnames(imdb) %>% .[8:32]
non_genre_vars <- colnames(imdb) %>% .[-c(8:32)]



## 一带一路国家列表
br_countries <- c("Mongolia","Russia","Pakistan","Bangladesh","Sri Lanka","Afghan","Nepal","Maldives","Bhutan","Mongolia","Russia","Indonesia","Thailand","Malaysia","Vietnam","Singapore","Philippines","Myanmar","Cambodia","Laos","Brunei","east Timor","Kazakhsta","Uzbekistan","Turkmenistan","Kyrghyzstan","Tajikista","Saudi Arabia","UAE","Oman","Iran","Turkey","Israel","Egypt","Kuwait","Iraq","Katar","Jordan","Lebanon","Bahrain","Yemen","Syria","Palestine","Poland","Rumania","Czech Republic","Slovakia","Bulgaria","Hungary","Latvia","Lithuania","Slovenia","Estonia","Croatia","Albania","Serbia","Macedonia","Bosnia and Herzegovina","Ukraine","Azerbaijan","Armenia","Belarus","Georgia","Moldova","China")


## 在我们的数据里面的一代一路的国家,一共有55个
belt_road <- br_countries[br_countries %in% countries_vars]



# imdb一带一路 宽数据 ------------------------------------------------------------

imdb_belt_road <- imdb %>% select(non_countries_vars,belt_road) %>% filter_at(vars(belt_road),any_vars(. != 0)) # 把没有这些国家参与的数据过滤掉

nrow(imdb_belt_road)



# 长数据 ---------------------------------------------------------------------

# 数据转换，将宽数据转成长数据
long_imdb_belt_road <- imdb_belt_road %>% 
  gather(-non_countries_vars,key = "country",value = "produce") %>%  #将多列的国家转成一列
  filter(produce != 0)


# imdb中有关中国的数据 ------------------------------------------------------------
imdb_cn <- imdb  %>% filter(China != 0) %>% select(non_countries_vars)


nrow(imdb_cn)
# [1] 10536








