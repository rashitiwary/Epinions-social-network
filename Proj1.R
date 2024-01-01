library(igraph)
library(sna)
opinions <- read.table("/Users/rashitiwary/Documents/George Washington University/Masters Subject/CSCI 6444 Big data and Analytics/Project 1/soc-Epinions1_adj.tsv", header=TRUE, sep='')
proj_matrix <- as.matrix(opinions)
proj_data_frame <- data.frame("X3","X1")
print("hi 2")
proj_matrix2 = proj_matrix[,-3]
vector1 <- proj_matrix2[,c("X3")]
vector2 <- proj_matrix2[,c("X1")]
relations <- data.frame(vector1,vector2)
plot_graph <- graph.data.frame(relations, directed = TRUE)
plot_graph
plot.igraph(plot_graph)


#cl <- walktrap.community(plot_graph, steps = 5)
#plot(cl,plot_graph, vertex.size=0.5, layout=layout.fruchterman.reingold)

# Contract vertices
E(plot_graph)$weight <- 1
V(plot_graph)$weight <- 1

plot_graph_simple<-contract.vertices(plot_graph, cl$membership, vertex.attr.comb = list(weight = "sum", name = function(x)x[1], "ignore"))

# Simplify edges
plot_graph_simple<-simplify(plot_graph_simple, edge.attr.comb = list(weight = "sum", function(x)length(x)))

plot_graph_simplified<-induced.subgraph(plot_graph_simple, V(plot_graph_simple)$weight > 30)
V(plot_graph_simplified)$degree<-unname(igraph::degree(plot_graph_simplified))

# Compute communities (clusters)
cl <- walktrap.community(plot_graph_simplified, steps = 5)
plot(cl,as.undirected(plot_graph_simplified), vertex.size=0.5, vertex.label = NA ,layout=layout.fruchterman.reingold)

#Plotting the graph and displaying the vertices and edges of the graph
plot.igraph(plot_graph_simplified)
V(plot_graph_simplified)
E(plot_graph_simplified)
igraph::edge_density(plot_graph_simplified)
igraph::edge_density(g_plot_graph_simplifiednew,loops=T)

#size of the graph
gorder(plot_graph_simplified)

#Checking if the graph generated is simple or not
is.simple(plot_graph_simplified)

degree <- igraph::degree(plot_graph_simplified)
degree

# Sort the vertices by degree and get the indices of the 20 with the lowest degree
lowest_degree_indices <- order(degree)[1:20]

# Remove the vertices with the lowest degree and create a new graph
g_new <- delete_vertices(plot_graph_simplified, lowest_degree_indices)
plot.igraph(g_new)

V(g_new)

E(g_new)

#getting the adjacency matrix
g_new.adj=igraph::get.adjacency(g_new)

g_new.adj

g_new.adj2=as_adjacency_matrix(g_new)
g_new.adj2

class(g_new.adj)

dim(g_new.adj)

sna::gden(proj_matrix2)

g_simple_matrix <- as.matrix(get.adjacency(g_new))
graph_simple_density=gden(g_simple_matrix)
graph_simple_density

igraph::edge_density(g_new)
igraph::edge_density(g_new,loops=T)

#g_new.ego=ego.extract(proj_matrix2)

g_simple_edgelist=get.edgelist(g_new)
geo <- geodist(g_simple_edgelist)
geo
#Degree of the graph
degree <- igraph::degree(g_new)
degree

#Betweenness Centrality
g_new.between=igraph::centr_betw(g_new)
g_new.between

#Closeness Centrality
g_new.closeness=igraph::centr_clo(g_new)
g_new.closeness

g_new.sp=igraph::shortest.paths(g_new)
g_new.sp

g_new.geos=geodist(proj_matrix2)
g_new.geos

g_new.np=g_new.adj%*%g_new.adj
g_new.np

hist(igraph::degree(g_new), xlab = "Degree", ylab = "Frequency", main = "Degree distribution", col = "lightgreen", border = "white")

plot(g_new, vertex.size = 3, vertex.label = NA, edge.arrow.size = 0.5, layout = layout.fruchterman.reingold)

g_new.d=igraph::get_diameter(g_new)
max(g_new.d)

g_new.lgcliques=igraph::clique_num(g_new)
g_new.lgcliques

#Getting the central node of the graph
V(g_new)$central_degree <- centr_degree(g_new)$res
V(g_new)$name[V(g_new)$central_degree==max(centr_degree(g_new)$res)]

#Ego centrality
g_new.ego=ego.extract(proj_matrix2[1:1000,1:2])
g_new.ego[234]

#Power centrality
power_centrality(g_new,exponent = 0.5)

#Calculating the longest path
g_lp = simplify(g_new, remove.multiple = TRUE, edge.attr.comb = NULL, remove.loops = TRUE)
diameter <- get_diameter(g_lp)
longest_path <- induced.subgraph(g_lp, diameter)
longest_path
plot(longest_path, vertex.size = 15, vertex.label.cex = 0.5, layout = layout.fruchterman.reingold)

#Calculating the clique of the graph
g_new_lgcliques=igraph::largest_cliques(g_new)
g_new_lgcliques_graph <- induced.subgraph(g_new, g_new_lgcliques[[1]])
plot(g_new_lgcliques_graph, vertex.size = 15, vertex.label.cex = 0.5, vertex.color ='lightblue', layout = layout.graphopt, edge.arrow.size=0.5)
g_new_lgcliques
library(igraph)
library(sna)
opinions <- read.table("/Users/rashitiwary/Documents/George Washington University/Masters Subject/CSCI 6444 Big data and Analytics/Project 1/soc-Epinions1_adj.tsv", header=TRUE, sep='')
proj_matrix <- as.matrix(opinions)
proj_data_frame <- data.frame("X3","X1")
print("hi 2")
proj_matrix2 = proj_matrix[,-3]
vector1 <- proj_matrix2[,c("X3")]
vector2 <- proj_matrix2[,c("X1")]
relations <- data.frame(vector1,vector2)
plot_graph <- graph.data.frame(relations, directed = TRUE)
plot_graph
plot.igraph(plot_graph)


#cl <- walktrap.community(plot_graph, steps = 5)
#plot(cl,plot_graph, vertex.size=0.5, layout=layout.fruchterman.reingold)

# Contract vertices
E(plot_graph)$weight <- 1
V(plot_graph)$weight <- 1

plot_graph_simple<-contract.vertices(plot_graph, cl$membership, vertex.attr.comb = list(weight = "sum", name = function(x)x[1], "ignore"))

# Simplify edges
plot_graph_simple<-simplify(plot_graph_simple, edge.attr.comb = list(weight = "sum", function(x)length(x)))

plot_graph_simplified<-induced.subgraph(plot_graph_simple, V(plot_graph_simple)$weight > 30)
V(plot_graph_simplified)$degree<-unname(igraph::degree(plot_graph_simplified))

# Compute communities (clusters)
cl <- walktrap.community(plot_graph_simplified, steps = 5)
plot(cl,as.undirected(plot_graph_simplified), vertex.size=0.5, vertex.label = NA ,layout=layout.fruchterman.reingold)

#Plotting the graph and displaying the vertices and edges of the graph
plot.igraph(plot_graph_simplified)
V(plot_graph_simplified)
E(plot_graph_simplified)
igraph::edge_density(plot_graph_simplified)
igraph::edge_density(g_plot_graph_simplifiednew,loops=T)

#size of the graph
gorder(plot_graph_simplified)

#Checking if the graph generated is simple or not
is.simple(plot_graph_simplified)

degree <- igraph::degree(plot_graph_simplified)
degree

# Sort the vertices by degree and get the indices of the 20 with the lowest degree
lowest_degree_indices <- order(degree)[1:20]

# Remove the vertices with the lowest degree and create a new graph
g_new <- delete_vertices(plot_graph_simplified, lowest_degree_indices)
plot.igraph(g_new)

V(g_new)

E(g_new)

#getting the adjacency matrix
g_new.adj=igraph::get.adjacency(g_new)

g_new.adj

g_new.adj2=as_adjacency_matrix(g_new)
g_new.adj2

class(g_new.adj)

dim(g_new.adj)

sna::gden(proj_matrix2)

g_simple_matrix <- as.matrix(get.adjacency(g_new))
graph_simple_density=gden(g_simple_matrix)
graph_simple_density

igraph::edge_density(g_new)
igraph::edge_density(g_new,loops=T)

#g_new.ego=ego.extract(proj_matrix2)

g_simple_edgelist=get.edgelist(g_new)
geo <- geodist(g_simple_edgelist)
geo
#Degree of the graph
degree <- igraph::degree(g_new)
degree

#Betweenness Centrality
g_new.between=igraph::centr_betw(g_new)
g_new.between

#Closeness Centrality
g_new.closeness=igraph::centr_clo(g_new)
g_new.closeness

g_new.sp=igraph::shortest.paths(g_new)
g_new.sp

g_new.geos=geodist(proj_matrix2)
g_new.geos

g_new.np=g_new.adj%*%g_new.adj
g_new.np

hist(igraph::degree(g_new), xlab = "Degree", ylab = "Frequency", main = "Degree distribution", col = "lightgreen", border = "white")

plot(g_new, vertex.size = 3, vertex.label = NA, edge.arrow.size = 0.5, layout = layout.fruchterman.reingold)

g_new.d=igraph::get_diameter(g_new)
max(g_new.d)

g_new.lgcliques=igraph::clique_num(g_new)
g_new.lgcliques

#Getting the central node of the graph
V(g_new)$central_degree <- centr_degree(g_new)$res
V(g_new)$name[V(g_new)$central_degree==max(centr_degree(g_new)$res)]

#Ego centrality
g_new.ego=ego.extract(proj_matrix2[1:1000,1:2])
g_new.ego[234]

#Power centrality
power_centrality(g_new,exponent = 0.5)

#Calculating the longest path
g_lp = simplify(g_new, remove.multiple = TRUE, edge.attr.comb = NULL, remove.loops = TRUE)
diameter <- get_diameter(g_lp)
longest_path <- induced.subgraph(g_lp, diameter)
longest_path
plot(longest_path, vertex.size = 15, vertex.label.cex = 0.5, layout = layout.fruchterman.reingold)

#Calculating the clique of the graph
g_new_lgcliques=igraph::largest_cliques(g_new)
g_new_lgcliques_graph <- induced.subgraph(g_new, g_new_lgcliques[[1]])
plot(g_new_lgcliques_graph, vertex.size = 15, vertex.label.cex = 0.5, vertex.color ='lightblue', layout = layout.graphopt, edge.arrow.size=0.5)
g_new_lgcliques
