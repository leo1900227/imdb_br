
p_load("igraph", "tidyverse", "ggmap", "purrr", "sp")


g2 <- prep_g(g, e.att = e.att, cutoff = cutoff)

vs <- get.data.frame(g2, what = 'vertices')

es <- get.data.frame(g2, what = 'edges')

es <- merge(es, vs %>% select(name, x = lon, y = lat), by.x = 'from', by.y = 'name') %>%
  
  merge(vs %>% select(name, xend = lon, yend = lat), by.x = 'to', by.y = 'name')

# Then plotting worked like this. And I used ggrepel for the labels.

ggmap(region) +
  
  geom_segment(data = es, color = 'grey30', alpha = 0.8,
               
               aes(x = x, y = y, xend = xend, yend = yend)) +
  
  geom_label_repel(data = l$vs, aes(x = lon, y = lat, label = abbrev)) +
  
  geom_point(data = l$vs, pch = 21, color = 'white', aes(x = lon, y = lat, fill = as.factor(memb), size = size)) +
  
  scale_color_brewer(palette = 'Set1') +
  
  theme_blank()