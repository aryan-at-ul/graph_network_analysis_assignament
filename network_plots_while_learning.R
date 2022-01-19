library(sna)

drugpaj <- read.paj('data_networks/drugnet2.paj')
drug <- drugpaj$networks[[1]]
gender <- drugpaj$partitions[[1]] 

suppressPackageStartupMessages(library(knitr)) 
kable(table(gender), col.names=c("Gender","Frequency"))

ethnicity <- drugpaj$partitions[[2]]
kable(table(ethnicity), col.names=c("Ethnicity","Frequency"))

png(filename="plots/simple_drug_user_plots.png")
#plot(drug)
#dev.off()

#story of high turn-over case study

# sociomatrix data for a network

num_nodes <- 15

my_matrix <- matrix(round(runif(num_nodes*num_nodes)),
                    nrow = num_nodes,
                    ncol = num_nodes)

my_matrix

diag(my_matrix) <- 0
dim(my_matrix)

class(my_matrix)

sum(is.na(my_matrix))


my_network <- as.network(x = my_matrix,
                         directed = TRUE,
                         loop = FALSE,
                         matrix.type = "adjacency")

plot(my_network)

par(mar=c(0,0,1,0))# bottom , left , top , right , adjust margin all to 0 except top
plot(my_network)

plot(my_network,main = "Random network")

library(foreign)
# read .dta // stata file
# read.xport // sax xport

drugpaj <- read.paj("data_networks/drugnet2.paj")
names(drugpaj)
names(drugpaj$networks)
names(drugpaj$partitions)

drug <- drugpaj$networks[[1]]
class(drug)
plot(drug)


gender <- drugpaj$partitions[[1]]
table(gender)


female <- ifelse(gender == 2,1,
                 ifelse(gender == 1,0,NA))

drug <- set.vertex.attribute(drug,'female',value = c(female))

ethnicity <- drugpaj$partitions[[2]] 
table(ethnicity)


drug <- set.vertex.attribute(drug,'ethnicity',value = c(ethnicity))

colors <- ifelse(gender == 2,"palevioletred",ifelse(gender == 1,"royalblue2","gray8"))


par(mar=c(0,0,0,0))
plot(drug,vertex.col = colors)


load("data_networks/flo.Rdata")

flo.marriage <- as.network(as.matrix(flo.marriage),directed = FALSE)
flo.biz <- as.network(as.matrix(flo.biz),directed = FALSE)


set.vertex.attribute(flo.marriage,'wealth',flo.att[,2])
set.vertex.attribute(flo.biz,'wealth',flo.att[,2])

par(mar=c(1,1,1,1), mfrow = c(1,2))

plot(flo.marriage,vertex.cex =(get.vertex.attribute(flo.marriage,'wealth')/25),
      displaylabels = TRUE)

plot(flo.biz,vertex.cex =(get.vertex.attribute(flo.biz,'wealth')/25),
     displaylabels = TRUE)



friendmat <- read.csv('hr_data.csv',header = TRUE)
head(friendmat)
class(friendmat)

friendmat <- as.matrix(friendmat)
friendmat


friendnet <- as.network(friendmat)
par(mfrow=c(1,1))
plot(friendnet,directed = TRUE,matrix.type = "adjacency")



hr_attr <- read.csv("Hr_data_Main.csv",header = TRUE)
sex <- hr_attr$Sex
length(sex)

dim(friendmat)

names <- hr_attr$Name
gender_vector <- vector()

detach(package:sna)
detach(package:network)

library(igraph)

friendgraph <- graph_from_adjacency_matrix(friendmat,weighted = TRUE)

friendgraph

for(i in 1:122){
  for(j in 1:68){
    
    if (V(friendgraph)$name[i] == names[j]){
      gender_vector[i] <- sex[j]
      break;
    }
    else{
      gender_vector[i] <-NA
    }
  }
}

head(gender_vector)
length(gender_vector)

friendgraph <- set_vertex_attr(friendgraph,'gender',value = c(gender_vector))

colors <- ifelse(gender_vector == 1,"palevioletred",
                 ifelse(gender_vector == 0, "royalblue2","gray"))
colors

par(mar= c(0,0,1,0),mfrow = c(1,1))

plot(friendgraph,vertex.size = 6.5, vertex.color = colors,
     edge.arrow.size = .2, edge.color = "black",
     vertex.label = NA,main = "hr friendship network")

#start of assignment
load("hypertext.RData")



