library(here)
getwd()

here("data/") %>% setwd()


tt_br[,1] %>% write_csv("tt_num_belt_road.csv",col_names = FALSE)

# 票房数据 --------------------------------------------------------------------
## 一带一路国家参与的电影的所有tt_num：tt_br

imdb_belt_road_box <- fread("belt_road_box.csv",header = TRUE)

colnames(imdb_belt_road_box)
# [1] "Budget"                    
# [2] "Cumulative_Worldwide_Gross"
# [3] "Gross_USA"                 
# [4] "Opening_Weekend_USA"       
# [5] "tt_num" 

# Cumulative_Worldwide_Gross处理非美元符号

imdb_belt_road_box %<>%
  mutate(
    currency = str_extract(Cumulative_Worldwide_Gross,"^[^0-9]") %>% as.character(),
    world_box = str_replace_all(Cumulative_Worldwide_Gross,"[^0-9]","") %>% as.numeric(),
    exchange_rate = if_else(currency == "P",0.0065,1),
    world_box_usd = world_box * exchange_rate
  )


# 
# # >>test --------------------------------------------------------------------
# test <- imdb_belt_road_box %>%
#   mutate(
#     currency = str_extract(Cumulative_Worldwide_Gross,"^[^0-9]") %>% as.character(),
#     world_box = str_replace_all(Cumulative_Worldwide_Gross,"[^0-9]","") %>% as.numeric(),
#     exchange_rate = if_else(currency == "P",0.0065,1),
#     world_box_usd = world_box * exchange_rate
#   )

# 
# test$currency %>% table()
# 
# test %>% filter(currency == "P") %>% select(tt_num)
# 
# # tt_num
# # 1 tt4635548
# # PKR = 0.0065 USD
# 
# 
# test$Cumulative_Worldwide_Gross %>% table()
# test$world_box
# test$exchange_rate %>% table()
# test$world_box_usd %>% summary()
# 
# imdb_belt_road_box
# 
# imdb_belt_road_box$currency %>% head()
# 




# join together -----------------------------------------------------------

# join imdb_belt_road_long and imdb_box with tt_num

imdb_belt_road_long_box <- merge(imdb_belt_road_long,imdb_belt_road_box,by = "tt_num")


colnames(imdb_belt_road_long_box)


imdb_belt_road_long_box %<>% .[,c(1:33,41)]
  
colnames(imdb_belt_road_long_box)


# 把数据到出来给冯老师 --------------------------------------------------------------
imdb_belt_road_long_box %>% select(-type) %>% 
  write_csv("imdb_br_box.csv")

