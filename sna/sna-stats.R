library(pacman)
p_load(igraph)

# data:net

class(net)





# Network and node descriptives ---------net----------------------------------

# >> density --------------------------------------------------------------
edge_density(net, loops=F)


# >> Reciprocity ----------------------------------------------------------


# >> Transitivity ---------------------------------------------------------



# >> Node degrees ---------------------------------------------------------
deg <- degree(net)
plot(net, vertex.size=deg*3)



# >> Degree distribution --------------------------------------------------
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")


cluster_edge_betweenness(net)
p_ceb <- dendPlot(ceb, mode="hclust")

plot(ceb,net)

class(ceb)


length(ceb) # number of communities
## 25

membership(ceb) # community membership for each node

crossing(ceb, net) # boolean vector: TRUE for edges across communities


# >> Community detection based on based on propagating labels 
clp <- cluster_label_prop(net)
plot(clp, net)

# >> Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net))

## plot the communities without relying on their built-in plot:

V(net)$community <- cfg$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community])


# >> K-core decomposition -------------------------------------------------
kc <- coreness(net, mode="all")
plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])


# Assortativity and Homophily ---------------------------------------------


