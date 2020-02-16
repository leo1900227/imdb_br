
imdb_belt_road_long_box %>% colnames()

# 阿尔巴尼亚为什么位置那么重要 ----------------------------------------------------------


tt_Albania <- imdb_belt_road_long_box %>% 
  filter(country == "Albania") %>% 
  .$tt_num %>% 
  unique()

# 230部影片

tb_Albania_col <- imdb_belt_road_long_box %>% 
  filter(tt_num %in% tt_Albania) %>% 
  group_by(tt_num) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

tb_Albania_col %>% filter(n > 1) %>% 
  nrow()
