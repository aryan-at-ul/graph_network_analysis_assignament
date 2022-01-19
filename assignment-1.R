rm(list=ls())

load("hypertext.RData")
library(igraph)


node_names <- vector()
for(i in 1:113){
  node_names[i] <- paste("V",i,sep="")
}
print(node_names)


gra <- graph.adjacency(adj, mode = "undirected")
V(gra)$name <- node_names

set.seed(128)

list.vertex.attributes(gra)

V(gra)$name
vertex_attr_names(gra)

nrow(adj)
ncol(adj)


X <- length(V(gra))
  

#layout <- layout.fruchterman.reingold(gra)
layout <- layout.auto(gra)


#colors <- c(paste0(rep("grey",X),seq(X,1)))
plot(gra, vertex.label = NA, vertex.size = 5,layout = layout)

n_nodes <- nrow(adj)
n_edges <- sum(adj)/2
n_possible_edges <- n_nodes * (n_nodes-1) / 2
n_triangles <- sum(diag(adj %*% adj %*% adj)) / 6
p <- n_edges / n_possible_edges

degrees <- rowSums(adj)
degree_freqs <- table(degrees)
plot(degree_freqs, type = "h", lwd = 4)


detach(package:igraph)
library(sna)

print(n_nodes) # 113 nodes in total and 441 dyads in total (mutual)
dyad.census(adj) 

triad.census(adj) 

components(adj) # 6 component

connectedness(adj) #0.9130

cutpoints(adj,mode="digraph",connected = c("strong"),return.indicator = FALSE) # 1  3 38 39 45 49 51 70

detach(package:sna)
library(igraph)

largest_cliques(gra)  # 6 is the highest clique

#centrality , degree,closeness and betweenness,eigenvector,presitge(not valid here) 

total_degree <- degree(gra,mode = "total")
head(total_degree)

btw <- betweenness(gra)
head(btw)

aevcent <- evcent(gra)
aevcent <- aevcent$vector
head(aevcent)


plot(btw, type = "h", lwd = 3)

central_table <- data.frame(total_degree,btw,aevcent)
head(central_table)

cor(central_table)


barplot(aevcent,names.arg = V(gra)$name)


library(CINNA)

comps <- graph_extract_components(gra)
comps

comp1 <- comps[[1]]
comp1
plot(comp1,vertex.label = NA)


pr_cent <- proper_centralities(comp1)




comp1_central <- calculate_centralities(comp1,include = c(pr_cent[8],pr_cent[10],pr_cent[13],
                                                          pr_cent[15],pr_cent[21],pr_cent[46]))
comp1_central

comp1_central <- as.data.frame(comp1_central)

head(comp1_central)


#social influence studies




### SPECTRAL CLUSTERING from lecture code 
adj <- as_adjacency_matrix(
  comp1,
  sparse = igraph_opt("sparsematrices")
)


eigen_dec <- eigen(adj) # find eigenvalues and eigenvectors
plot(eigen_dec$values, pch = 20) # make a decision on K
abline(v=2)
K <- 3 # K = 3 and K = 4 also give very reasonable clustering solutions
embedding <- eigen_dec$vectors[,1:K] # project the nodes into a low-dimensional latent space
memberships <- kmeans(embedding, K, nstart = 100)$cluster # cluster the nodes
plot(comp1, vertex.label = NA, vertex.size = 5, vertex.color = memberships, layout = layout)
res <- make_clusters(comp1, memberships)
plot(x = res, y = comp1, layout = layout, vertex.label = NA, vertex.size = 5)


### STOCHASTIC BLOCKMODELLING
library(blockmodels) # this package uses S4 classes
sbm <- BM_bernoulli(membership_type = "SBM_sym", # data type
                    adj = adj, # adjacency matrix
                    verbosity = 1, # how much should be printed out while running the algorithm
                    plotting="", # could be used to show how the values of the ICL evolve during the estimation
                    explore_max = 4) # maximum number of clusters to consider

sbm$estimate() # this line runs the VEM on the dataset
K_star <- which.max(sbm$ICL) # extract the best model according to ICL values
soft_clustering <- sbm$memberships[[K_star]]$Z # posterior probabilities for group memberships ("taus")
hard_clustering <- apply(soft_clustering, 1, which.max) # maximum a posteriori clustering
sbm$memberships[[K_star]]$plot() # plot group sizes
sbm$plot_parameters(K_star) # plot connection probabilities

random_allocs <- sample(1:n_nodes,n_nodes,F) # shuffle the nodes' labels to erase any potential patterns or orderings
image(adj[order(random_allocs),rev(order(random_allocs))], xaxt = "n", yaxt = "n") # plot the adjacency matrix
abline(a = 1, b=-1, lty = 2, col = 2)

image(adj[order(hard_clustering),rev(order(hard_clustering))], xaxt = "n", yaxt = "n") # plot the adjacency matrix
abline(a = 1, b=-1, lty = 2, col = 2)
# highlight the blocks (very basic coding, it could be improved)
group_counts <- (as.numeric(table(hard_clustering)))
abline(v = cumsum(group_counts/sum(group_counts)))
abline(h = 1-cumsum(group_counts/sum(group_counts)))

memberships_sbm <- make_clusters(gra, hard_clustering) # create the new communities object according to partition found
plot(memberships_sbm, mark.groups = NULL, edge.color = NULL, gra, vertex.label = NA, vertex.size = 2.5*sqrt(rowSums(adj)), layout = layout)


## End of lecture exercises (sketch of solutions)

# 1
# the package does not return the estimated lambdas. We can estimate them in two ways
colSums(soft_clustering) / sum(colSums(soft_clustering))
table(hard_clustering) / n_nodes

# 2
sbm$model_parameters[[K_star]]$pi[1,2]

# 3
sbm$plot_parameters(K_star)# group 1 and 2 exhibit a clear community structure

# 4
lambda <- colSums(soft_clustering) / sum(colSums(soft_clustering))
lambda %*% sbm$model_parameters[[K_star]]$pi # group number 2 has more connections

# 5
n_nodes * lambda %*% sbm$model_parameters[[K_star]]$pi # around 25

# 6
table(memberships, hard_clustering)
# the groups with community structure are similar in both partitions, the group of hubs is where the partitions are fundamentally different

# we can use the misclassification error to compare the results
require(mclust)
classError(memberships, classes)$errorRate
classError(hard_clustering, classes)$errorRate


