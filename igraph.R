#Load the package 
library(igraph)

#example of network
ex <- read.graph("example2.net", format="pajek")
tkplot(ex)

#read the pajek file in igraph
reponetwork <- read.graph("network.net", format = "pajek")
sample <- read.graph("Rnetwork.net", format = "pajek")
#Inspect the data:

#prints the list of vertices(people):
V(reponetwork)
#To count the number of vertices:
vcount(reponetwork)
#prints the list of edges(relationships):
E(reponetwork)
#calculating degree of each vertex
degree(reponetwork)
vorder <- order(degree(reponetwork), decreasing = TRUE)
DF <- data.frame(ID=as.numeric(V(reponetwork)[vorder]), degree=degree(reponetwork)[vorder])
#calculating betweenness

vorder <- order(betweenness(sample), decreasing = TRUE)
DF <- data.frame(ID=as.numeric(V(sample)[vorder]), betweenness=betweenness(sample)[vorder])

#calculating clustering coefficient (or transitivity) of each vertex

vorder <- order(transitivity(reponetwork, type="local"), decreasing = TRUE)
DF <- data.frame(ID=as.numeric(V(reponetwork)[vorder]), transitivity=transitivity(reponetwork, type="local")[vorder])
