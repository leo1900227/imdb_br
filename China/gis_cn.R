library(maps)
map.world <- map_data('world')
map.world
dim(map.world)
map.world %>% colnames()
# [1] "long"      "lat"       "group"     "order"     "region"   
# [6] "subregion"
# 
# map.world %>% head()
# map.world$region %>% unique()
# map.world$subregion %>% unique()
# map.world$group %>% unique()
# 
# map.world %>% group_by(group)
# 
# map.world  %>% filter(region == "China")

map_world <- map_data('world')

# map_world

df_cn_tmp <- 
  list_df_cc %>% 
  filter(from == "China" | to == "China") 



# 和中国有过合作的国家及合作数量 -----------------------------------------------------------


df_cn_col <- 
  bind_rows(df_cn_tmp[,-2],df_cn_tmp[,-1]) %>% 
  mutate(
    country = if_else(!is.na(from),from,to)
  ) %>% 
  filter(country != "China") %>% 
  .[,c(4,2)] %>% 
  mutate(
    region_br = if_else(country %in% c(br_vars,"China"),"yes","no")
  )
  
nrow(df_cn_col)
# [1] 158
table(df_cn_col$region_br)
# no yes 
# 59  99 


df_cn_friends <- 
  left_join( map_world, df_cn_col, by = c('region' = 'country'))



# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🐟---------------------地图：中国的合作伙伴-------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟


df_cn_friends %>%
  ggplot(aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = weight, color = region_br)) +
  scale_color_manual(values = c('yes' = 'blue', 'no' = NA )) +
  scale_fill_gradientn(
    colors = c('#FFA07A','#FF7F50','#FF6347','#FF0000','#FA8072','#F08080','#E9967A','#DC143C','#CD5C5C','#B22222','#A52A2A'),
    values = scales::rescale(c(1,10,50,100,200,300,500,800,900,1000)),
    breaks = c(1,10,50,100,200,300,500,800,900,1000)
  ) + 
  guides(fill = guide_legend(reverse = T))

