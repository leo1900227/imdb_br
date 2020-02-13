library(maps)
map.world <- map_data('world')
map.world
dim(map.world)
map.world %>% colnames()
# [1] "long"      "lat"       "group"     "order"     "region"   
# [6] "subregion"

map.world %>% head()
map.world$region %>% unique()
map.world$subregion %>% unique()
map.world$group %>% unique()

map.world %>% group_by(group)

map.world  %>% filter(region == "China")

map_world <- map_data('world')

map_world

table_cn_countries_col


df_cn_friends <- left_join( map_world, table_cn_countries_col, by = c('region' = 'country'))



df_cn_friends %>%
  filter(!is.na(belt_road) & region != "Antarctica") %>%
  ggplot(aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = n, color = belt_road)) +
  scale_color_manual(values = c('yes' = 'blue', 'no' = NA )) +
  scale_fill_gradientn(
    colors = c('#FFA07A','#FF7F50','#FF6347','#FF0000','#FA8072','#F08080','#E9967A','#DC143C','#CD5C5C','#B22222','#A52A2A'),
    values = scales::rescale(c(1,10,50,100,200,300,500,800,900,1000)),
    breaks = c(1,10,50,100,200,300,500,800,900,1000)
  ) +guides(fill = guide_legend(reverse = T))

