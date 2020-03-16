library(maps)
library(readxl)


map.world <- map_data('world') %>% 
  filter(region %in% belt_road | region %in% "Taiwan")


loc <- read_csv("./data/0111-经纬度.csv")
edge_list <- read_excel("./output/edege_list.xlsx")

br_map <- 
  edge_list %>% 
  left_join(loc,by=c('from'='Label'))%>%
  left_join(loc,by=c('to'='Label'))


colnames(br_map)
# [1] "from"        "to"          "weight"      "Latitude.x" 
# [5] "Longitude.x" "Latitude.y"  "Longitude.y"

br_map_cn <- br_map %>% 
  filter(from == "China"| to == "China")


ggplot() +
  geom_polygon(data=map.world, aes( x = long, y = lat, group = group),fill="white",colour="black",size=0.25)+
  geom_curve(data=br_map,aes(x=Longitude.x,y=Latitude.x,xend=Longitude.y,yend=Latitude.y, size = weight),curvature = 0.1)+
  geom_curve(data=br_map_cn,aes(x=Longitude.x,y=Latitude.x,xend=Longitude.y,yend=Latitude.y, color = "red",size = weight ),curvature = 0.1) + 
  scale_size(range=c(0.01,1))+
  geom_point(data =br_map,aes(x=Longitude.y,y=Latitude.y),
             size=1,shape=21,fill="#F00000",colour="black",stroke=0.1)
