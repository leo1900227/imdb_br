p_load(ggraph,tidygraph,graphlayouts,oaqc)

cc_list_df %>% head()


# "./pic/ggraph"

graph <- cc_list_df %>% as_tbl_graph()

# Not specifying the layout - defaults to "auto"
ggraph(graph) + 
  geom_edge_link(aes(colour = factor(weight))) + 
  geom_node_point()


ggraph(graph, layout = 'linear') + 
  geom_edge_arc(aes(colour = factor(weight)))


# A coord diagram
ggraph(graph, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(aes(colour = factor(weight))) + 
  coord_fixed()



# graphlayouts 的layout ----------------------------------------------------
pa <- graph



ggraph(graph,layout = "nicely")+
  geom_edge_link(width=0.2,colour="grey")+
  geom_node_point(col="black",size=0.3)+
  theme_graph()


ggraph(pa,layout="stress")+
  geom_edge_link(width=0.2,colour="grey")+
  geom_node_point(col="black",size=0.3)+
  theme_graph()


ggraph(pa,layout = "nicely") +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

ggraph(pa,layout = "stress")+
  geom_edge_link(colour=rgb(0,0,0,0.5),width=0.1)+
  scale_color_brewer(palette = "Set1")+
  theme_graph()+
  theme(legend.position = "none")



ggraph(pa,layout="stress")+
  geom_edge_link(width=0.2,colour="grey")+
  geom_node_point(col="black",size=0.3)+
  theme_graph()


g <- pa %>% as.undirected()


ggraph(g, layout = 'stress') + 
  geom_edge_density(aes(fill = weight)) + 
  geom_edge_link(alpha = 0.25)


bb <- layout_as_backbone(g,keep=0.4)
E(g)$col <- F
E(g)$col[bb$backbone] <- T

ggraph(g,layout="manual",x=bb$xy[,1],y=bb$xy[,2])+
  geom_edge_link(aes(col=col),width=0.1)+
  scale_color_brewer(palette = "Set1")+
  scale_edge_color_manual(values=c(rgb(0,0,0,0.3),rgb(0,0,0,1)))+
  theme_graph()+
  theme(legend.position = "none")

