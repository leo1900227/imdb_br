library(pacman)
p_load("tidyverse", "igraph", "tidygraph", "ggraph",network,statnet,reshape2)


# “一带一路”沿线国家的合作关系 ---------------------------------------------------------
long_imdb_belt_road %>% colnames()


# long data
df_country <- long_imdb_belt_road %>% 
  select(tt_num,country) 


# 每一行是一部电影，每一列是一个国家（doc-term matrix）
matrix_country <- df_country %>% table()

matrix_country[1:10,1:3]

matrix_country %>% dim()


# 国家-国家 矩阵, https://url.cn/5g09Z95

cc_matrix <- crossprod(matrix_country)

dim(cc_matrix)
## 53
class(cc_matrix)

# write.table(cc_matrix,"cc_matrix.csv")
# 还可以让对角线变为0

net <- network(cc_matrix,matrix.type="adjacency")

class(net)
summary(net)

p_net <- gplot(net,vertex_col=2,displaylables=FALSE) # statnet包的功能

# # adjacency list
# cc_list <- reshape2::melt(cc_matrix) 
# 
# names(cc_list) <- c("var1","var2","value")
# 
# head(cc_list)
# class(cc_list)
# dim(cc_list)
# 
# ## 删除重复项
# cc_list_df <-cc_list %>% filter(var1 != var2)
# 
# ## 删除重复项
# cols <- c(1,2)
# newdf <- cc_list_df[,cols]
# for (i in 1:nrow(cc_list_df)){
#   newdf[i, ] = sort(cc_list_df[i,cols])
# }
# 
# cc_list_df <- cc_list_df[!duplicated(newdf),]



#  🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟


cc_list_df <- cc_matrix %>% graph.adjacency(mode = "undirect",weighted = TRUE) %>%  get.data.frame() %>% filter(from != to) 

dim(cc_list_df)
head(cc_list_df,50)



# 哪些国家之间合作比较多？
cc_list_df %>% arrange(desc(weight))


  
ad_list_net <- graph.data.frame(cc_list_df, directed = FALSE)

net <- ad_list_net

class(ad_list_net)
E(ad_list_net)
V(ad_list_net)


plot.igraph(ad_list_net, layout=layout_with_kk, vertex.color="yellow")


cliques(net)

ceb <

# plotting ----------------------------------------------------------------


# >> layout ---------------------------------------------------------------
l <- layout_in_circle(net) 
plot(net,layout=l)

l <- layout_with_kk(net) 
plot(net,layout=l)


