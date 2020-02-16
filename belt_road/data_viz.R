
## 国家名称中英对照
belt_road_cn_name <- read_csv("./data/belt_road_cn_name.csv")


# tb_br ：一带一路国家关键信息

tb_br %>% write_csv("./output/tb_br.csv")

# 分类数据 --------------------------------------------------------------------

tb_br <- 
  imdb_belt_road_long_box %>% 
  group_by(country) %>% 
  summarise(
    n = n(),
    score = mean(score, na.rm = T),
    score_cnt = sum(score_cnt, na.rm = T),
    box = sum(world_box_usd, na.rm = T)
  ) %>% 
  left_join(belt_road_cn_name,by = "country") %>% 
  arrange(desc(n)) %>% 
  mutate(
    box_m = round(box/1000000)
  )


tb_br$country %>% View()


tb_br[,-6] %>% fwrite("./output/“一带一路”战略区域电影数量、评分及评分总人数统计表_new.csv")

# 对factor进行排序（画图时用到

tb_br$country <- factor(tb_br$country, levels = tb_br$country [order(tb_br$n)])

tb_br$country_cn <- factor(tb_br$country_cn, levels = tb_br$country_cn [order(tb_br$n)])

tb_br


# 散点图：评分及电影生产量关系图 🐕🐕 ----------------------------------------------------------------

# 
# # p_place <- 
#   tb_br %>% 
#   ggplot(aes(n,country,size = box,color=score)) + 
#   geom_point() + 
#   labs(x="评分",
#        y ="国家/地区",
#        title  =  "“一带一路”沿线国家电影评分及电影生产量关系图") +
#   scale_y_discrete(labels = country) 

# p_n_box <-
  tb_br %>% 
  ggplot() + 
# 加线条
    # geom_segment(aes(x=0, 
  #                  xend=n,
  #                  y=country_cn, 
  #                  yend=country_cn)) +
  geom_point(aes(n,country_cn,size=box,color=score_cnt)) +
  scale_colour_gradient(low="#8cba51", high="#ff5d6c") +
  labs(x="数量",y ="国家/地区")+ 
  theme_bw()

ggsave()

# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟



# 评分分布图 ------------------------------------------
jpeg("./pic/电影评分分布图.jpg")
imdb_belt_road %>% 
  ggplot(aes(x=score,fill = "bbded6")) + 
  geom_histogram() + 
  labs(x="评分",
       y ="计数",
       title  =  "“一带一路”沿线国家IMDB电影评分分布表")
dev.off()


# 年份 ----------------------------------------------------------------------

tb_year <- 
  table(imdb_belt_road$year) %>%
  as.data.frame() %>% 
  rename(year=Var1) %>% 
  arrange(desc(year)) ;tb_year 


tb_year$year <- lubridate::ymd(tb_year$year, truncated = 2L) %>% lubridate::year()


# “一带一路”沿线国家IMDB电影数量年度分布图 +++++++++++++++

p_year_n <- 
  tb_year %>%
  filter(year < 2019) %>%
  ggplot(aes(x=20ar,y=Freq,coln = "geom_smooth(aes(x=year,y=Freq))+ 
  lement_text(angle = 30, hjust = 1))  + 
  scale_x_continuous(breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)) + 
  labs(x="年份",
       y ="计数",
       title  =  "“一带一路”沿线国家IMDB电影数量年度分布图");p_year_n



ggsave("./pic/“一带一路”沿线国家IMDB电影数量年度分布图.png")



# 电影类型与评分的关系图 ---------------------------------------------------------

# 对factor进行排序（画图时用到）
genre_df$genre <- factor(genre_df$genre, levels = genre_df$genre[order(genre_df$score)])



## 散点图

p_score_genree_df  %>% ggplot(aes(x=score,y=genre,size=n,colour = "#007f7f"))  +
  geom_point() + 
  labs(y="电影类型",x ="评分",title  =  "电影类型与评分的关系图") +
  theme_light();p_score_genre # 

