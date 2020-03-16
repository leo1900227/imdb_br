#library
library(igraph)

# Create data
set.seed(10)
data <- matrix(sample(0:2, 25, replace=TRUE), nrow=5)
colnames(data) = rownames(data) = LETTERS[1:5]

class(data)


# build the graph object
network <- graph_from_adjacency_matrix(data)

# plot it
plot(network)



# https://stackoverflow.com/questions/54338215/convert-a-dataframe --------


library(tidyverse)
p_load(combinat)

df <- data.frame(n = c(2, 3, 2, 2), 
                 x = c("a, b", "a, c, d", "c, d", "d, b"))

df1 <- df %>% 
  ## Parse entries in x into distinct elements
  mutate(split = map(x, str_split, pattern = ', '), 
         flat = flatten(split)) %>% 
  ## Construct 2-element subsets of each set of elements
  mutate(combn = map(flat, combn, 2, simplify = FALSE)) %>% 
  unnest(combn) %>% 
  ## Construct permutations of the 2-element subsets
  mutate(perm = map(combn, permn)) %>% 
  unnest(perm) %>% 
  ## Parse the permutations into row and column indices
  mutate(row = map_chr(perm, 1), 
         col = map_chr(perm, 2)) %>% 
  count(row, col)

df1

df2 <- df1 %>% 
  ## Long to wide representation
  spread(key = col, value = n, fill = 0) %>% 
  ## Coerce to matrix
  column_to_rownames(var = 'row') %>% 
  as.matrix()

df2

# 🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟🐟
# data.table
library(data.table)


df <- data.frame(n = c(2, 3, 2, 2), 
                 x = c("a, b", "a, c, d", "c, d", "d, b"))
#generate the combis
combis <- df[, transpose(combn(sort(strsplit(x, ", ")[[1L]]), 2L, simplify=FALSE)), 
             by=1L:df[,.N]]

#create new rows for identical letters within a pair or any other missing combi
withDiag <- out[CJ(c(V1,V2), c(V1,V2), unique=TRUE), on=.(V1, V2)]

#duplicate the above for lower triangular part of the matrix
withLowerTri <- rbindlist(list(withDiag, withDiag[,.(df, V2, V1)]))

#pivot to get weights matrix
outDT <- dcast(withLowerTri, V1 ~ V2, function(x) sum(!is.na(x)), value.var="df")

























library(tidyverse)

df <- tribble(
  ~id, ~name,
  1, "a", 
  1, "b", 
  2, "b", 
  2, "c",
  3, "b",
  3, "c"
)# create a 2-mode sociomatrix

mat <-  t(table(df))
# create adjacency matrix as product of the 2-mode sociomatrix
adj.mat <- mat %*% t(mat)
# if you want the diagonal to be 0 use : diag(adj.mat) <- 0. This can also be done directly
# with igraph
# define your network
library(igraph)
net <- graph_from_adjacency_matrix(adj.mat, mode = "undirected", weighted = TRUE,
                                   diag = FALSE)
V(net)$name # vertices (nodes) name
E(net) # edges
E(net)$weight # edges weight
# example of plot
library(ggraph)
ggraph(net, layout = "igraph", algorithm = "kk") +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(size = 8, colour = "steelblue") + 
  geom_node_text(aes(label = name)) +
  ggforce::theme_no_axes()
# output










