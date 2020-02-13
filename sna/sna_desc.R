
# 本部分代码复制自《网络数据的统计分析》第四章 网络图特征的描述性统计
# data: net

library(pacman)
p_load(sand,igraphdata,network,sna,igraph)

head(net)
class(net)

setwd("/data1/zhiliang/RWorkSpace/one_belt_one_road")

# 1. 节点与边的特征 --------------------------------------------------------------


# >> 节点度 ------------------------------------------------------------------

## 节点度的分布🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟

hist(igraph::degree(net), col="lightblue", xlim=c(0,50),
     xlab="Vertex Degree", ylab="Frequency", main="“一带一路”沿线国家网络图节点度的分布图")

ggsave("./pic/sna/“一带一路”沿线国家网络图节点度的分布图.png")

## 节点强度的分布🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟


hist(graph.strength(net), col="yellow",
     xlab="Vertex Strength", ylab="Frequency", main="“一带一路”沿线国家网络图节点强度度的分布图")
ggsave("./pic/sna/“一带一路”沿线国家网络图节点强度的分布图.png")


# 边的数量
ecount(net)
# ---
## [1] 805
# ---


# 节点的数量
vcount(net)
# ---
## [1] 54
# ---


## 考虑到分布的递减趋势，双对数坐标在表达度的信息史更为有效
d.net <- igraph::degree(net)

dd.net <- degree.distribution(net)

d <- 1:max(d.net)-1
ind <- (dd.net != 0)

plot(d[ind], dd.net[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")



## 节点度与邻居平均节点度的关系（双对数坐标）

a.nn.deg.net <- graph.knn(net,V(net))$knn

plot(d.net, a.nn.deg.net, log="xy", 
     col="goldenrod", xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))

A <- get.adjacency(net, sparse=FALSE)


# >> 节点中心性 ----------------------------------------------------------------
# sna::gplot.target 函数可以将中心度较高的点放到网络中心位置突出显示

g <- network::as.network.matrix(A)
 # Convert a graph to an adjacency matrix


## 将中心都较高的节点展示在中心位置🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱🐱�

sna::gplot.target(g, igraph::degree(g), main="Degree",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col=c("blue", rep("red", 32), "yellow"),
                  edge.col="darkgray")


# >> 边的特征  ----------------------------------------------------------------

## 边介值最大的三个数值rder(eb, decreasing=T)[1:3]]

eb <- igraph::edge.betweenness(net)
E(net)[order(eb,decreasing = T)[1:10]]


# + 10/805 edges from 12fb83a (vertex names):
#   [1] Syria     --Turkmenistan Latvia    --Philippines 
# [3] Israel    --Mongolia     Bangladesh--Russia      
# [5] Lithuania --Mongolia     Indonesia --Mongolia    
# [7] Russia    --Yemen        Brunei    --Laos        
# [9] Brunei    --Maldives     Mongolia  --Ukraine 



# 3. 网络凝聚性特征 --------------------------------------------------------------


# >> 子图与普查 ----------------------------------------------------------------

table(sapply(cliques(net), length))

# ---
## 
##  1  2  3  4  5 
## 34 78 45 11  2
# ---

# CHUNK 13
cliques(net)[sapply(cliques(net), length) == 5]
# ---
## [[1]]
## [1] 1 2 3 4 8
## 
## [[2]]
## [1]  1  2  3  4 14
# ---

# CHUNK 14
table(sapply(maximal.cliques(net), length))
# ---
## 
##  2  3  4  5 
## 11 21  2  2
# ---

# CHUNK 15
clique.number(net)
# ---
## [1] 23
# ---

# CHUNK 16
cores <- graph.coreness(net)
sna::gplot.target(g, cores, circ.lab = FALSE, 
                  circ.col="skyblue", usearrows = FALSE, 
                  vertex.col=cores, edge.col="darkgray")
detach("package:network")
detach("package:sna")

# CHUNK 17
aidsblog <- simplify(aidsblog)
dyad.census(aidsblog)
# ---
## $mut
## [1] 3
## 
## $asym
## [1] 177
## 
## $null
## [1] 10405
# ---



# >> 密度与相对频率 --------------------------------------------------------------

ego.instr <- induced.subgraph(net,
                              neighborhood(net, 1, 1)[[1]])
ego.admin <- induced.subgraph(net,
                              neighborhood(net, 1, 34)[[1]])
graph.density(net)
# ---
## [1] 0.1390374
# ---
graph.density(ego.instr)
# ---
## [1] 0.25
# ---
graph.density(ego.admin)
# ---
## [1] 0.2091503
# ---

# CHUNK 19
transitivity(net)
# ---
## [1] 0.2556818
# ---

# CHUNK 20
transitivity(net, "local", vids=c(1,34))
# ---
## [1] 0.1500000 0.1102941
# ---

# CHUNK 21
reciprocity(aidsblog, mode="default")
# ---
## [1] 0.03278689
# ---
reciprocity(aidsblog, mode="ratio")
# ---
## [1] 0.01666667
# ---



# >> 流通性、割与流 --------------------------------------------------------------
is.connected(net)
# ---
## [1] FALSE
# ---

# CHUNK 23
comps <- decompose.graph(net)
table(sapply(comps, vcount))
# ---
## 
##    2    3    4    5    6    7 2375 
##   63   13    5    6    1    3    1
# ---

# CHUNK 24
net.gc <- decompose.graph(net)[[1]]

# CHUNK 25
average.path.length(df_cn_friends) %>%
  filter(!is.na(belt_road) & region != "Antarctica") %>%
    ggplot(aes( x = long, y = lat, group = group )) +
    geom_polygon(aes(fill = n, color = belt_road)) +
    scale_color_manual(values = c('yes' = 'red', 'no' = NA )) +
    scale_fill_gradientn(
      colors = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F'),
      values = scales::rescale(c(1,100,300,500,1000)),
      breaks = c(1,100,300,500,1000)) +
    guides(fill = guide_legend(reverse = T))

  
  ggsave("./pic/sna/与中国合作的国家地图.jpg")


# ---
## [1] 5.09597
# ---

# CHUNK 26
diameter(net.gc)
# ---
## [1] 15
# ---

# CHUNK 27
transitivity(net.gc)
# ---
## [1] 0.4686663
# ---

# CHUNK 28
vertex.connectivity(net.gc)
# ---
## [1] 1
# ---
edge.connectivity(net.gc)
# ---
## [1] 1
# ---

# CHUNK 29
net.cut.vertices <- articulation.points(net.gc)
length(net.cut.vertices)
# ---
## [1] 350
# ---

# CHUNK 30
is.connected(aidsblog, mode=c("weak"))
# ---
## [1] TRUE
# ---

# CHUNK 31
is.connected(aidsblog, mode=c("strong"))
# ---
## [1] FALSE
# ---

# CHUNK 32
aidsblog.scc <- clusters(aidsblog, mode=c("strong"))
table(aidsblog.scc$csize)
# ---
## 
##   1   4 
## 142   1
# ---

# CHUNK 33
kc <- fastgreedy.community(net)

# CHUNK 34
length(kc)
# ---
## [1] 3
# ---
sizes(kc)
# ---
## Community sizes
##  1  2  3 
## 18 11  5
# ---

# CHUNK 35
membership(kc)
# ---
##    Mr Hi  Actor 2  Actor 3  Actor 4  Actor 5  Actor 6 
##        2        2        2        2        3        3 
##  Actor 7  Actor 8  Actor 9 Actor 10 Actor 11 Actor 12 
##        3        2        1        1        3        2 
## Actor 13 Actor 14 Actor 15 Actor 16 Actor 17 Actor 18 
##        2        2        1        1        3        2 
## Actor 19 Actor 20 Actor 21 Actor 22 Actor 23 Actor 24 
##        1        2        1        2        1        1 
## Actor 25 Actor 26 Actor 27 Actor 28 Actor 29 Actor 30 
##        1        1        1        1        1        1 
## Actor 31 Actor 32 Actor 33   John A 
##        1        1        1        1
# ---

# CHUNK 36
plot(kc,net)

# CHUNK 37
library(ape)
dendPlot(kc, mode="phylo")

# CHUNK 38
k.lap <- graph.laplacian(net)
eig.anal <- eigen(k.lap)

# CHUNK 39
plot(eig.anal$values, col="blue",
     ylab="Eigenvalues of Graph Laplacian")

# CHUNK 40
f.vec <- eig.anal$vectors[, 33]

# CHUNK 41
faction <- get.vertex.attribute(net, "Faction")
f.colors <- as.character(length(faction))
f.colors[faction == 1] <- "red"
f.colors[faction == 2] <- "cyan"
plot(f.vec, pch=16, xlab="Actor Number",
     ylab="Fiedler Vector Entry", col=f.colors)
abline(0, 0, lwd=2, col="lightgray")

# CHUNK 42
func.class <- get.vertex.attribute(net.gc, "Class")
table(func.class)
# ---
## func.class
##   A   B   C   D   E   F   G   M   O   P   R   T   U 
##  51  98 122 238  95 171  96 278 171 248  45 240 483
# ---

# CHUNK 43
yc <- fastgreedy.community(net.gc)
c.m <- membership(yc)

# CHUNK 44
table(c.m, func.class, useNA=c("no"))
# ---
##     func.class
## c.m    A   B   C   D   E   F   G   M   O   P   R   T   U
##   1    0   0   0   1   3   7   0   6   3 110   2  35  14
##   2    0   2   2   7   1   1   1   4  39   5   0   4  27
##   3    1   9   7  18   4   8   4  20  10  23   8  74  64
##   4   25  11  10  22  72  84  81 168  14  75  16  27 121
##   5    1   7   5  14   0   4   0   2   3   6   1  34  68
##   6    1  24   1   4   1   4   0   7   0   1   0  19  16
##   7    6  18   6  76   7   9   3   7   8   5   1   7  33
##   8    8  12  67  59   1  34   0  19  60  10   7   6  73
##   9    4   1   7   7   2  10   5   3   2   0   3   0  11
##   10   0   0   0   6   0   0   0   2   0   5   0  11   1
##   11   0   9   0  10   1   3   0   0   0   0   0   2   4
##   12   0   1   3   0   0   0   0   6  10   0   0   0   2
##   13   0   1   1   2   0   1   0   0   2   0   0  16  10
##   14   1   0   4   1   0   1   0   0   4   0   1   0  11
##   15   0   1   0   0   0   2   0   2   0   0   1   0   8
##   16   0   1   2   0   0   1   0   0  10   0   0   0   0
##   17   0   0   1   3   0   0   0   2   0   0   0   2   3
##   18   0   0   0   0   3   1   0   9   0   0   1   0   1
##   19   0   1   1   1   0   0   0   0   0   0   0   0   3
##   20   0   0   0   6   0   0   0   1   0   0   0   1   2
##   21   1   0   0   0   0   0   0   0   6   0   0   1   0
##   22   0   0   0   0   0   0   0   1   0   0   0   0   8
##   23   0   0   0   0   0   0   0   4   0   0   0   0   0
##   24   0   0   0   0   0   0   2   2   0   0   0   1   0
##   25   0   0   0   0   0   0   0   5   0   0   0   0   0
##   26   0   0   1   0   0   0   0   4   0   0   1   0   1
##   27   3   0   4   0   0   1   0   0   0   0   0   0   0
##   28   0   0   0   0   0   0   0   0   0   6   0   0   0
##   29   0   0   0   1   0   0   0   1   0   0   3   0   0
##   30   0   0   0   0   0   0   0   0   0   2   0   0   2
##   31   0   0   0   0   0   0   0   3   0   0   0   0   0
# ---

# CHUNK 45
assortativity.nominal(net, (V(net)$Class=="P")+1, 
                      directed=FALSE)
# ---
## [1] 0.4965229
# ---

# CHUNK 46
assortativity.degree(net)
# ---
## [1] 0.4610798
# ---

