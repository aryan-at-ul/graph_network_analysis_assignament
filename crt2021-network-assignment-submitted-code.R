rm(list=ls())

load("hypertext.RData")
library(igraph)


node_names <- vector()
for(i in 1:113){
  node_names[i] <- paste("V",i,sep="")
}
print(node_names)


gra <- graph.adjacency(adj, mode = "undirected")

#QUESTION 1
# assignment question 1 plot.
GNC <- cluster_edge_betweenness(gra, weights = NULL)
V(gra)$color <-membership(GNC)              #Plot setting specifying the coloring of vertices by community
# supports only 50 palette <- diverging_pal(length(GNC))   #Plot setting specifying the color pallette I am using (iGraph supports 3)
plot(gra, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA,vertex.size = 2.5*sqrt(rowSums(adj)))


# writing graph to a file to analyize in other languages etc
write_graph(
  gra,
  'exported-grpah.dot',
  format = c("dot")
)

#not required as no attributes further present in data
V(gra)$name <- node_names

set.seed(128)

list.vertex.attributes(gra)
V(gra)$name
vertex_attr_names(gra)
nrow(adj)
ncol(adj)
X <- length(V(gra))
layout <- layout.fruchterman.reingold(gra)
#another plot for question 1, todo: pick one
plot(gra, vertex.label = NA, vertex.size = 5,layout = layout)


#QUESTION 2 descriptive statistics
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
#degrees
degree_freqs <- table(degrees)
plot(degree_freqs, type = "h", lwd = 4)

# not for assignment 
if(FALSE){
detach(package:igraph)
library(sna)
print(n_nodes) # 113 nodes in total and 441 dyads in total (mutual)
dyad.census(adj) 
triad.census(adj) 
components(adj) # 6 component
connectedness(adj) #0.9130
cutpoints(adj,mode="digraph",connected = c("strong"),return.indicator = FALSE) # 1  3 38 39 45 49 51 70
}
detach(package:sna)



library(igraph) #check some centralities
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
barplot(eigne_vec,names.arg = V(gra)$name)



library(CINNA)

comps <- graph_extract_components(gra)
comps

comp1 <- comps[[1]]
comp1
plot(comp1,vertex.label = NA)
pr_cent <- proper_centralities(comp1)
comp1_central <- calculate_centralities(comp1,include = c(pr_cent[8],pr_cent[10],pr_cent[13],
                                                          pr_cent[15],pr_cent[21],pr_cent[46]))comp1_central
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
invlogit(x) # value same as network density??? why ? 


#goodness of fir
er1.gof <- gof(er1~degree) 

er1.gof$pval.deg
par(mflow=c(1,1))# this is useless
plot(er1.gof) #not good fit at-all to-do : try on something else with node



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


#nothing useful from or and & rules
plot(adj2,main="original network",coord = mycord,vertex.cex = 3,edge.col = 'azure4',
     vertex.col="#E41A1C",vertex.border='azure4',label=seq(1:20),label.pos=5,
     label.cex= .5,label.col='gray15')

plot(orRule,main="original network",coord = mycord,vertex.cex = 3,edge.col = 'azure4',
     vertex.col="#E41A1C",vertex.border='azure4',label=seq(1:20),label.pos=5,
     label.cex= .5,label.col='gray15')


plot(orRule,main="and network",coord = mycord,vertex.cex = 3,edge.col = 'azure4',
     vertex.col="#E41A1C",vertex.border='azure4',label=seq(1:20),label.pos=5,
     label.cex= .5,label.col='gray15')



library(RColorBrewer) 
#par(mar=c(1,1,1,1),mfrow=c(2,3)) 
col11 <- brewer.pal(11, 'Set3')

cols<-vector()


eucdist <- dist(orRule, method="euclidian", diag=FALSE)
thick <- as.vector(eucdist)
#par(mar=c(0,0,0,0))

#thicker eges means closer
plot(orRule,
     vertex.cex =3, edge.col='pink', vertex.col=col11[5], vertex.border='azure4', label=seq(1:113),label.pos=5, label.cex=.5,label.col='gray15', edge.lwd = thick^2)

library("ape") # this modele is for dendrogram only
#http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
Matrix_dist<-dist(adj2)
Matrix_dist
Full_Cluster <- hclust(Matrix_dist)
plot(Full_Cluster, hang = -1, cex = 0.6)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")
# Customized plot; remove labels
plot(as.phylo(Full_Cluster), cex = 0.6, label.offset = 0.5)
plot(as.phylo(Full_Cluster), type = "fan")

colors = c("red", "blue", "green", "black")
clus4 = cutree(Full_Cluster, 4)
plot(as.phylo(Full_Cluster), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)

library(dendextend)
library(ggplot2)
dend <- as.dendrogram(Full_Cluster)
plot(dend)

ggd1 <- as.ggdend(dend)
ggplot(ggd1) 

dend %>% set("branches_k_color", k = 4) %>% 
  plot(main = "Default colors")

ggplot(ggd1, theme = theme_minimal()) 

ggplot(ggd1, labels = FALSE) + 
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")
##################################end of section for analysing dendogram to guess any clusters


# Basic block model along with distance matrix NOT included in assignment
bm <- blockmodel(adj2,Full_Cluster,k = 5,diag = FALSE)
mem <- bm$block.membership
#mem
heatmap(bm[[4]])
Full_Cluster
ct <- cutree(Full_Cluster, k = 5)
class(ct)
plot(gra, vertex.label = NA, vertex.size = 5, vertex.color = mem, layout = layout)
#plot(gra, mark.groups = NULL, edge.color = NULL, gra, vertex.label = NA, vertex.size = 2.5*sqrt(rowSums(adj)), layout = layout)



### SPECTRAL CLUSTERING  QUESTION 3
eigen_dec <- eigen(adj) # find eigenvalues and eigenvectors
plot(eigen_dec$values, pch = 20) 
abline(v=4)
K <-  4

embedding <- eigen_dec$vectors[,1:K] 
memberships <- kmeans(embedding, K, nstart = 100)$cluster # clustering the nodes
#memberships

#color changes , the community behaviour is between 2 largest group based on number of edges
plot(gra, vertex.label = NA, vertex.color = memberships, layout = layout,vertex.size = 2.5*sqrt(rowSums(adj)))
#res <- make_clusters(gra, memberships)
#layout2 <- layout_with_kk(gra)
#plot(x = res, y = gra, layout = layout, vertex.label = NA, vertex.size = 5)

#Some other clustering methods used Not in assignment
#to-do: not in submission, not in assignment
Stretch = 6
LO_F5 = Stretch*layout.circle(gra)
plot(gra, layout=LO_F5)

co <- cluster_fast_greedy(gra)
plot(co, gra, main="Cluster Optimal Communities",vertex.label = NA)


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
# end on some other clustering methods


#
## STOCHASTIC BLOCKMODELLING QUESTION 4
library(blockmodels) 
sbm <- BM_bernoulli(membership_type = "SBM_sym", 
                    adj = adj, 
                    verbosity = 1, 
                    plotting="",
                    explore_max = 4) 

sbm$estimate() # this line runs the VEM on the dataset
K_star <- which.max(sbm$ICL) # extract the best model according to ICL values
soft_clustering <- sbm$memberships[[K_star]]$Z 
hard_clustering <- apply(soft_clustering, 1, which.max) 
sbm$memberships[[K_star]]$plot() # though group 4 has more nodes its not showing community behavior
sbm$plot_parameters(K_star) # important 

random_allocs <- sample(1:n_nodes,n_nodes,F) # shuffle the nodes' labels to erase any potential patterns or orderings
image(adj[order(random_allocs),rev(order(random_allocs))], xaxt = "n", yaxt = "n") # plot the adjacency matrix heatmap
abline(a = 1, b=-1, lty = 2, col = 2)

image(adj[order(hard_clustering),rev(order(hard_clustering))], xaxt = "n", yaxt = "n") # heat map plot of adjacency matrix
abline(a = 1, b=-1, lty = 2, col = 2)
group_counts <- (as.numeric(table(hard_clustering)))
group_counts
abline(v = cumsum(group_counts/sum(group_counts)))
abline(h = 1-cumsum(group_counts/sum(group_counts)))

memberships_sbm <- make_clusters(gra, hard_clustering) # create the new communities object according to partition found
#final plot
plot(memberships_sbm, mark.groups = NULL, edge.color = NULL, gra, vertex.label = NA, vertex.size = 2.5*sqrt(rowSums(adj)), layout = layout)
legend("topright", c("group 1","group 2","group 3","group 4"), pch = 20, cex = .5, pt.cex = .5, col = categorical_pal(8)[1:4])



#some statistics for question 4 in table 2 and table 3
colSums(soft_clustering) / sum(colSums(soft_clustering)) #mixing proportion
table(hard_clustering) / n_nodes
table(hard_clustering) #nodes in groups

#prob of edge formation between groups
sbm$model_parameters[[K_star]]$pi[1,2]
sbm$model_parameters[[K_star]]$pi[1,3]
sbm$model_parameters[[K_star]]$pi[1,4]
sbm$model_parameters[[K_star]]$pi[2,1] 
sbm$model_parameters[[K_star]]$pi[2,3]
sbm$model_parameters[[K_star]]$pi[2,4]
sbm$model_parameters[[K_star]]$pi[3,1]
sbm$model_parameters[[K_star]]$pi[3,2]
sbm$model_parameters[[K_star]]$pi[3,4]
# 3
sbm$plot_parameters(K_star)# group 3 and 3 exhibit a clear community structure

# 4
lambda <- colSums(soft_clustering) / sum(colSums(soft_clustering))
lambda %*% sbm$model_parameters[[K_star]]$pi # group 2 has more connections

# 5
n_nodes * lambda %*% sbm$model_parameters[[K_star]]$pi #21,group 2

# 6


