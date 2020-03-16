library(pacman)
p_load(data.table,tidyverse,readxl,xlsx)



here()

# 选出一带一路相关的数据 -------------------------------------------------------------
imdb_br_long <- 
  imdb_long %>% 
  filter(country %in% br_vars) %>% 
  .[,c(1:34)] %>% 
  lazy_dt()


dim(imdb_br_long)


# 一带一路国家名称 ----------------------------------------------------------------
br_vars <- read_excel("../data/一带一路国家名称.xlsx",sheet = "Sheet1") %>% 
  .$country %>% 
  as.vector()




# 一带一路国家参与的电影的所有tt_num ----------------------------------------------------

tt_br <- 
  imdb_br_long %>% 
  group_by(tt_num) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  as_tibble()


tt_br_vars <- 
  tt_br %>% 
  .[1] %>% 
  as_vector()



# ## “一带一路”沿线国家🐟独作🐟的电影的tt_num -------------------------------------------


tt_br_alone <- 
  tt_br %>% 
  filter(n == 1) %>% 
  .[1] %>% 
  as_vector()

tt_br_alone_vars <- tt_br_alone

# ## “一带一路”沿线国家🐟合作🐟的电影的tt_num -------------------------------------------
tt_br_col <- 
  tt_br %>% 
  filter(n != 1) %>% 
  .[1] %>% 
  as_vector()

tt_br_col_vars <- tt_br_col
