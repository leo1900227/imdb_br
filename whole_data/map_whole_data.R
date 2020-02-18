
map.world <- map_data('world')
map_br <- filter(map.world,region %in% br_vars)


map.world_pca <- left_join(map.world,tb_imdb_pca,by = c("region" = "country")) %>%
  mutate(
    region_br = if_else(region %in% br_vars,"yes","no")
)



# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🐟 市场影响力因子----------------------------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
map.world_pca %>%
  ggplot(aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = Dim.1, color = region_br)) +
  scale_color_manual(values = c('yes' = 'red', 'no' = NA )) +
  scale_fill_gradientn(
    colors = c('white','#f7beff','#cd4dcc','#7e0cf5','#400082'),
    values = scales::rescale(c(0,1,3,5,20)),
    breaks = c(0,1,3,5,20)
) +
  guides(fill = guide_legend(reverse = T))




# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🐟 世界电影网络口碑（score）地图----------------------------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟


map.world_pca %>%
  ggplot(aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = score, color = region_br)) +
  scale_color_manual(values = c('yes' = 'red', 'no' = NA )) +
  scale_fill_gradientn(
    colors = c('#ffffff','#f7beff','#cd4dcc','#7e0cf5','#400082'),
    values = scales::rescale(c(6,6.5,7,7.5,8)),
    breaks = c(6,6.5,7,7.5,8)
  ) +
  guides(fill = guide_legend(reverse = T)) +
  labs(x = "", y = "")




# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🐟 世界电影网络传播度（score_cnt）地图----------------------------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
log(map.world_pca$score_cnt)  %>% range(na.rm = T)





map.world_pca %>%
  ggplot(aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = log(score_cnt), color = region_br)) +
  scale_color_manual(values = c('yes' = 'red', 'no' = NA )) +
  scale_fill_gradientn(
    colors = c('#ffffff','#f7beff','#cd4dcc','#7e0cf5','#400082'),
    values = scales::rescale(c(1,3,5,7,9)),
    breaks = c(1,3,5,7,9)
  ) +
  guides(fill = guide_legend(reverse = T)) +
  labs(x = "", y = "")





