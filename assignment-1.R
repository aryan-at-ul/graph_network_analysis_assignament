rm(list=ls())

load("hypertext.RData")
library(igraph)


node_names <- vector()
for(i in 1:113){
  node_names[i] <- paste("V",i,sep="")
}
print(node_names)


gra <- graph.adjacency(adj, mode = "undirected")

write_graph(
  gra,
  'exported-grpah.dot',
  format = c("dot")
)

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
n_nodes
n_edges <- sum(adj)/2
n_edges
n_possible_edges <- n_nodes * (n_nodes-1) / 2
n_possible_edges
n_triangles <- sum(diag(adj %*% adj %*% adj)) / 6
n_triangles
p <- n_edges / n_possible_edges
p
degrees <- rowSums(adj)
degrees
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
max_cliques(gra)

#centrality , degree,closeness and betweenness,eigenvector,presitge(not valid here) 

total_degree <- degree(gra,mode = "total")
head(total_degree)

btw <- betweenness(gra)
head(btw)

eigne_vec <- evcent(gra)
eigne_vec <- eigne_vec$vector
head(eigne_vec)


plot(btw, type = "h", lwd = 4)
plot(eigne_vec, type = "h", lwd = 3)
plot(total_degree, type = "h", lwd = 4)

central_table <- data.frame(total_degree,btw,eigne_vec)
head(central_table)

cr <- cor(central_table)
library(corrplot)
corrplot(cr, method = 'number')



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


#exponential random graph modeles , parametric model of network
library(ergm)
library(sna)
library(intergraph)



adj2 <- asNetwork(gra)
adj2

er1 <- ergm(adj2~edges)
summary(er1)

invlogit <- function(x){ 1/ (1 + exp(-x))}
x <- coef(er1)
invlogit(x) # value same as network density???


#goodness of fir
er1.gof <- gof(er1~degree) 

er1.gof$pval.deg
par(mflow=c(1,1))
plot(er1.gof)

#second erga model  dont run this
er2 <- ergm(adj2~edges+triangle)


#network.density(adj2)
library(NetCluster)
library(sna)
library(intergraph)
library(igraph)

orRule <- symmetrize(adj2,rule="weak")
class(orRule)
orRule <- network(symmetrize(adj2,rule="weak"),directed = FALSE)
andRule <- network(symmetrize(adj2,rule="strong"),directed = FALSE)

n <- network.size(adj2) 
v1 <- sample((0:(n-1))/n)
v2 <- sample(v1)
x <- n/(2 * pi) * sin(2*pi*v1)
y <- n/(2 * pi) * cos(2*pi*v1)

mycord <- cbind(x,y)
library(RColorBrewer)


#par(mar=c(1,1,2,1))
#par(mfrow=c(2,2))

plot(adj2,main="original network",coord = mycord,vertex.cex = 3,edge.col = 'azure4',
     vertex.col="#E41A1C",vertex.border='azure4',label=seq(1:20),label.pos=5,
     label.cex= .5,label.col='gray15')

plot(orRule,main="original network",coord = mycord,vertex.cex = 3,edge.col = 'azure4',
     vertex.col="#E41A1C",vertex.border='azure4',label=seq(1:20),label.pos=5,
     label.cex= .5,label.col='gray15')


plot(orRule,main="and network",coord = mycord,vertex.cex = 3,edge.col = 'azure4',
     vertex.col="#E41A1C",vertex.border='azure4',label=seq(1:20),label.pos=5,
     label.cex= .5,label.col='gray15')


plot(orRule, main = ' Symmetrized with OR Rule', coord=mycord, vertex.cex =3,edge.col='azure4', vertex.col="#377EB8", vertex.border='azure4', label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')
plot(andRule, main = ' Symmetrized with and Rule', coord=mycord, vertex.cex =3,edge.col='azure4', vertex.col="#377EB8", vertex.border='azure4', label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')

library(RColorBrewer) 
#par(mar=c(1,1,1,1),mfrow=c(2,3)) 
col11 <- brewer.pal(11, 'Set3')

cols<-vector()


eucdist <- dist(orRule, method="euclidian", diag=FALSE)
thick <- as.vector(eucdist)
#par(mar=c(0,0,0,0))

plot(orRule,
     vertex.cex =3, edge.col='pink', vertex.col=col11[5], vertex.border='azure4', label=seq(1:113),label.pos=5, label.cex=.5,label.col='gray15', edge.lwd = thick^2)


Matrix_dist<-dist(adj2)
Full_Cluster <- hclust(Matrix_dist)
plot(Full_Cluster)
dend <- as.dendrogram(Full_Cluster)
plot(dend)


bm <- blockmodel(adj2,Full_Cluster,k = 4,diag = FALSE)

mem <- bm$block.membership
mem

heatmap(bm[[4]])
Full_Cluster
ct <- cutree(Full_Cluster, k = 4)
class(ct)
plot(gra, vertex.label = NA, vertex.size = 5, vertex.color = mem, layout = layout)
#plot(gra, mark.groups = NULL, edge.color = NULL, gra, vertex.label = NA, vertex.size = 2.5*sqrt(rowSums(adj)), layout = layout)



### SPECTRAL CLUSTERING from lecture code 
adj <- as_adjacency_matrix(
  comp1,
  sparse = igraph_opt("sparsematrices")
)


eigen_dec <- eigen(adj) # find eigenvalues and eigenvectors
plot(eigen_dec$values, pch = 20) # make a decision on K
abline(v=2)
K <- 4 # K = 3 and K = 4 also give very reasonable clustering solutions
embedding <- eigen_dec$vectors[,1:K] # project the nodes into a low-dimensional latent space
memberships <- kmeans(embedding, K, nstart = 100)$cluster # cluster the nodes
plot(comp1, vertex.label = NA, vertex.size = 5, vertex.color = memberships, layout = layout)
res <- make_clusters(gra, memberships)
plot(x = res, y = gra, layout = layout, vertex.label = NA, vertex.size = 5)

#remove it later
library(lsa)
coords = layout_with_fr(gra)
c1 = cluster_fast_greedy(gra)
modularity(c1)
B = modularity_matrix(gra, membership(c1))
plot(c1, gra,vertex.label = NA,layout=coords)
plot(gra,vertex.label = NA ,vertex.color=membership(c1), layout=coords)
plot_dendrogram(c1)


c2 <-  cluster_leading_eigen(gra)
modularity(c2)
plot(c2, gra,vertex.label = NA, layout=coords)


c3 = cluster_edge_betweenness(gra)
modularity(c3)
plot(c3, gra,vertex.label = NA,layout=coords)



#
## STOCHASTIC BLOCKMODELLING
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
group_counts
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
sbm$model_parameters[[K_star]]$pi[1,4]
sbm$model_parameters[[K_star]]$pi[2,3] #2,3 has greater chances 

# 3
sbm$plot_parameters(K_star)# group 1 and 2 exhibit a clear community structure

# 4
lambda <- colSums(soft_clustering) / sum(colSums(soft_clustering))
lambda %*% sbm$model_parameters[[K_star]]$pi # group number 2 has more connections

# 5
n_nodes * lambda %*% sbm$model_parameters[[K_star]]$pi # around 25

# 6
memberships
hard_clustering
table(memberships, hard_clustering)
# the groups with community structure are similar in both partitions, the group of hubs is where the partitions are fundamentally different

# we can use the misclassification error to compare the results
require(mclust)
classError(memberships, classes)$errorRate
classError(hard_clustering, classes)$errorRate


