library(pacman)
p_load("RColorBrewer","wesanderson")



# 挑一下颜色 -------------------------------------------------------------------
display.brewer.all()
display.




map.world <- map_data('world')
map_br <- filter(map.world,region %in% br_vars)


map.world_pca <- left_join(map.world,tb_imdb_pca,by = c("region" = "country")) %>%
  mutate(
    region_br = if_else(region %in% c(br_vars,"Taiwan"),"yes","no")
)


# >> 修改一下南极洲的数据 



# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🐟 市场影响力因子----------------------------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟

map_dim1 <- 
  map.world_pca %>%
  ggplot(aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = Dim.1, color = region_br)) +
  scale_color_manual(values = c('yes' = 'red', 'no' = NA )) +
  scale_fill_gradientn(
    colors = brewer.pal(n = 5, name = 'BuPu'),
    values = scales::rescale(1:20)
    ) +
  guides(color = FALSE) +
  labs(fill="市场影响力",
       x = "",
       y = "") +
  theme(legend.position = c(0.95,0.2) )

map_dim1 




# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🐟 世界电影网络口碑（score）地图----------------------------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟

map_score <- 
  map.world_pca %>%
    ggplot(aes( x = long, y = lat, group = group )) +
    geom_polygon(aes(fill = score, color = region_br)) +
    scale_color_manual(values = c('yes' = 'red', 'no' = NA )) +  
    scale_fill_gradientn(
      colors = brewer.pal(n = 5, name = 'BuPu'),
      values = scales::rescale(c(6,6.5,7,7.5,8)),
      breaks = c(6,6.5,7,7.5,8)
    ) +
  guides(color = FALSE) +
  labs(fill="网络口碑",
       x = "",
       y = "") +
  theme(legend.position = c(0.95,0.2) )



# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# 🐟 世界电影网络传播度（score_cnt）地图----------------------------------------------
# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
log(map.world_pca$score_cnt)  %>% range(na.rm = T)


map_score_cnt <- 
  map.world_pca %>%
    ggplot(aes( x = long, y = lat, group = group )) +
    geom_polygon(aes(fill = log(score_cnt), color = region_br)) +
    scale_color_manual(values = c('yes' = 'red', 'no' = NA )) +
    scale_fill_gradientn(
      colors = c('#EDF3FF','#f7beff','#cd4dcc','#7e0cf5','#400082'),
      values = scales::rescale(c(1,3,5,7,9)),
      breaks = c(1,3,5,7,9)
    ) +
  guides(color = FALSE) +
  labs(fill="网络传播度",
       x = "",
       y = "") +
  theme(legend.position = c(0.95,0.2) )

