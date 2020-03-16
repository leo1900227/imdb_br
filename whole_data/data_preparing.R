library(pacman)
p_load(data.table,tidyverse)

# imdb_long


# data --------------------------------------------------------------------

imdb_long <- fread("/data1/zhiliang/RWorkSpace/one_belt_one_road/data/imdb_long.csv")


colnames(imdb_long)

# >> imdb_long_genres -----------------------------------------------------
imdb_long_genres <- imdb_long %>%
  gather(genre_vars,key = "genre",value = "n") %>%  
  filter(n != 0)


imdb_long_genres %>% 
  fwrite("./data/imdb_long_genres.csv")

# 电影数量国家排名 ----------------------------------------------------------------
tb_country_n <- 
  imdb_long$country %>% 
  table() %>% 
  as.data.frame() %>% 
  magrittr::set_colnames(c("country","Freq")) %>% 
  arrange(desc(Freq))

head(tb_country_n,10)

# > head(tb_country_n,10)
# country   Freq
# 1        USA 901757
# 2         UK 255202
# 3     France 105460
# 4      Japan 103091
# 5  Argentina  86241
# 6     Canada  84894
# 7    Germany  74133
# 8  Australia  68846
# 9      Spain  64054
# 10     Italy  48389


# 年份-国家-数量 ----------------------------------------------------------------
tb_yr_n <- 
  imdb_long %>% 
  group_by(year,country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


