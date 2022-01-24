#Jonathan H. Morgan
#Collaboration Networks Examples Data: Basic Visualization
#5 August 2016
load("hypertext.RData")
#LOADING PACKAGES
library (igraph)
library (readr)
library (haven)
library (ggplot2)

########################################
# IMPORTING Collaboration Network DATA #
########################################

Colleague_Network <- read.csv('C:/Users/Jonathan H Morgan/Desktop/PCMI_Personally Know_Combined Edgelist.csv')

Discussion_Network <- read.csv('C:/Users/Jonathan H Morgan/Desktop/PCMI_Discussion Network_Combined_Edgelist.csv')

node_names <- vector()
for(i in 1:113){
  node_names[i] <- paste("V",i,sep="")
}
print(node_names)


gra <- graph.adjacency(adj, mode = "undirected")
V(gra)$name <- node_names


#####################################################################
# Formatting the Data to Create an iGraph Style Edge List and Graph #
#####################################################################

#To Learn More about iGraph in R: http://kateto.net/networks-r-igraph

#Creating an iGraph Style Edgelist
Colleague_EdgeList <- Colleague_Network
#Creating a Graph Obeject for Subsequent Analyses
#Importantly, we define here whether the graph will be directed or not
Colleague_Graph=graph.data.frame(Colleague_EdgeList, directed=TRUE)

#Creating an iGraph Style Edgelist
Discussion_EdgeList <- Discussion_Network
#Creating a Graph Obeject for Subsequent Analyses
#Importantly, we define here whether the graph will be directed or not
Discussion_Graph=graph.data.frame(Discussion_EdgeList, directed=TRUE)

#####################
#  Visualizations   #
#####################

#Colleague Network: First Try

#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout.fruchterman.reingold(gra) #Creating a layout object to tell iGraph what layout I want

#Node or Vertex Options: Color
V(gra)$color <- "grey"

library(intergraph)

adj2 <- asNetwork(gra)

V(gra)[degree(adj2)>8]$color <- "yellow"  #Destinguishing High Degree Nodes as yellow

#Edge Options: Siz
E(gra)$color <- "grey"

#Plotting 
plot(gra)

#Verdict: Pretty Ugly, let's try to fix this.

#Colleague Network: Second Try

#Let work on get the nodes better first by making the sizing proportional to the network measure we care about, in this case degree
V(gra)$size=degree(adj2)/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.

#Node or Vertex Options: Color
V(gra)$color <- "grey"
V(gra)[degree(adj2)>8]$color <- "yellow"  #Destinguishing High Degree Nodes as yellow

#Edge Options: Siz
E(gra)$color <- "grey"

#Plotting 
plot(gra)

#Verdict: Better, but the arrows are distracting. 
#Sizing by in-degree will indicate that this is a directed graph. We don't need the arrow heads.

#Colleague Network: Third Try
#Removing Arrows

#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout.fruchterman.reingold(gra) #Creating a layout object to tell iGraph what layout I want

#Node or Vetex Options: Size and Color
V(gra)$color <- "grey"
V(gra)[degree(adj2)>8]$color <- "yellow"  #Destinguishing High Degree Nodes as yellow
V(gra)$size=degree(adj2)/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.

#Edge Options: Color
E(gra)$color <- "grey"

#Plotting, Now Specifying an arrow size and getting rid of arrow heads
#We are letting the color and the size of the node indicate the directed nature of the graph
plot(gra, edge.arrow.size=0.25,edge.arrow.mode = "-")

#Verdict: Better, but the self-loops are distracting

#Colleague Network: Fourth Try

#Removing Self-Loops (Repondents Nominating Themselves)
gra2<-simplify(gra, remove.multiple=TRUE, remove.loops=TRUE)

#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout.fruchterman.reingold(gra2) #Creating a layout object to tell iGraph what layout I want

#Node or Vetex Options: Size and Color
V(gra2)$color <- "grey"
V(gra2)[degree(adj2)>8]$color <- "yellow"  #Destinguishing High Degree Nodes as yellow
V(gra2)$size=degree(adj2)/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.

#Edge Options: Color
E(gra2)$color <- "grey"

#Plotting, Now Specifying an arrow size and getting rid of arrow heads
#We are letting the color and the size of the node indicate the directed nature of the graph
plot(gra2, edge.arrow.size=0.25,edge.arrow.mode = "-")


##########################
#  Discussion Networks   #
##########################

#Removing Self-Loops (Repondents Nominating Themselves)
gra3<-simplify(gra2, remove.multiple=TRUE, remove.loops=TRUE)


#Discussion Network: Try 2

#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout_with_kk(gra2)
#Node or Vetex Options: Size and Color
V(gra2)$size=degree(adj2)/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
V(gra2)[degree(adj2)>8]$color <- "yellow" 
#Edge Options: Color
E(gra2)$color <- "grey"

#Plotting, Now Specifying an arrow size and getting rid of arrow heads
#We are letting the color and the size of the node indicate the directed nature of the graph
plot(gra2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)

#Verdict: Maybe a bit better, but maybe not??

#######################################################################
#  Thinking about the Network in Terms of Connectivity and Position   #
#######################################################################

#CONNECTIVITY

#Density
graph.density(gra2,loop=FALSE)

#Average Path Length
mean_distance(gra2)

#Degree Distribution
degree_distribution(gra2)
Discussion_DegreeDis <- degree_distribution(gra2)   #Turns this into a data object we can export

Discussion_DegreeDis2 <- as.data.frame(Discussion_DegreeDis)

qplot(Discussion_DegreeDis, data=Discussion_DegreeDis2, geom="histogram", binwidth=.001)

#Clustering Coefficeint 
transitivity(gra2)

Discussion_Trans <- transitivity(gra2)

#POSITION

#Degree: In, Out, All Centrality
Discussion_OutDegree <- degree(adj2)
Discussion_OutDegree <- as.data.frame(Discussion_OutDegree)



#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout.fruchterman.reingold(gra2,niter=500)
#Node or Vetex Options: Size and Color
V(gra2)$size=degree(adj2)/5 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
V(gra2)[degree(adj2)>8]$color <- "yellow" 

#Edge Options: Color
E(gra2)$color <- "grey"

#Plotting, Now Specifying an arrow size and getting rid of arrow heads
#We are letting the color and the size of the node indicate the directed nature of the graph
plot(gra2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)

#Closeness Centrality
closeness(adj2)

Discussion_InCloseness <- closeness(Discussion_Graph2, mode="in")
Discussion_InCloseness <- as.data.frame(Discussion_InCloseness)


Discussion_OutCloseness <- closeness(Discussion_Graph2, mode="out")
Discussion_OutCloseness <- as.data.frame(Discussion_OutCloseness)

closeness(Discussion_Graph2, mode="out")

closeness(Discussion_Graph2, mode="all")

#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout.fruchterman.reingold(Discussion_Graph2,niter=500)
#Node or Vetex Options: Size and Color
V(Discussion_Graph2)$size=closeness(Discussion_Graph, mode = "out")/.05#because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
V(Discussion_Graph2)$color <- ifelse(Discussion_Attributes[V(Discussion_Graph2), 2] == "Researcher", "blue", "red")

#Edge Options: Color
E(Discussion_Graph2)$color <- "grey"

#Plotting, Now Specifying an arrow size and getting rid of arrow heads
#We are letting the color and the size of the node indicate the directed nature of the graph
plot(Discussion_Graph2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)


#Betweeness Centrality
Discussion_Betweeness <- betweenness(Discussion_Graph2)
Discussion_Betweeness <- as.data.frame(Discussion_Betweeness)


#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout.fruchterman.reingold(Discussion_Graph2,niter=500)
#Node or Vetex Options: Size and Color
V(Discussion_Graph2)$size=betweenness(Discussion_Graph)/200 #because we have wide range, I am dividing by 5 to keep the high in-degree nodes from overshadowing everything else.
V(Discussion_Graph2)$color <- ifelse(Discussion_Attributes[V(Discussion_Graph2), 2] == "Researcher", "blue", "red")



######################################################
# Segue to Advanced Network Visualization Techniques #
######################################################
#We might want to know how assorted or "cliquey" the discussion network is.

#We can answer this question using community detection technquies; these techniques could take a Graduat Student course to 
#begin to fully explore, but iGraph allows to start to look at this question.
#Community Detection Techniques are an important way of visualizing large scale networks. So, we will end basic visualiation here
# with a taste of them.

#Community Detection Algorithms in iGraph: Approaches Supported by iGraph
#Detecting communitities by iteratively calculating edge betweeness (e.g., Girvan & Newman 2001)
#Detecting communities by using eigenvector matrices (e.g., Newman 2006)
#Detecting communities by iteratively optimizing for modularity (e.g., Blondel, Guillaume, Lambiotte, & Lefebvre 2008)
#Detecting communities using random walk methods (e.g, Pons & Latapy 2005; Reichardt & Bornholdt 2006)
#Detecting communities using label propogation techniques (e.g., Ragavan, Albert, & Kumara 2007)

#Edge-Betweeness: Girvan-Newman (2001)

GNC <- cluster_edge_betweenness(gra2, weights = NULL)
V(gra2)$color <-membership(GNC)              #Plot setting specifying the coloring of vertices by community
Discussion_Graph2$palette <- diverging_pal(length(GNC))   #Plot setting specifying the color pallette I am using (iGraph supports 3)
plot(gra2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)

#Verdict: This is one answer, probably not the best one, but more on that next time.

eigen_centrality(gra2)
Discussion_EigenCentrality <- eigen_centrality(gra2)
Discussion_EigenCentrality <- as.data.frame(Discussion_EigenCentrality)
#Layout Options
set.seed(3952)  # set seed to make the layout reproducible
layout1 <- layout.fruchterman.reingold(gra2,niter=500)
#Node or Vetex Options: Size and Color
V(gra2)$color <- eigen_centrality(gra2)
#Edge Options: Color
E(gra2)$color <- "grey"
#Plotting, Now Specifying an arrow size and getting rid of arrow heads
#We are letting the color and the size of the node indicate the directed nature of the graph
plot(gra2, edge.arrow.size=0.25,edge.arrow.mode = "-", vertex.label = NA)





