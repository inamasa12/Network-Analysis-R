#1.2
dg <- matrix(c(
0,1,1,0,
1,0,1,0,
0,0,0,0,
1,0,0,0),
nrow = 4, ncol = 4, byrow = TRUE)
dg

#1.3
e.list <- c(1,2,1,3,2,1,2,3,4,1)
e.matrix <- matrix(e.list, ncol = 2, byrow = TRUE)
e.matrix

#1.4
write.table(
matrix(c(
0,1,1,0,
1,0,1,0,
0,0,0,0,
1,0,0,0),
nrow = 4,
ncol = 4,
byrow = TRUE),
file = "adj.txt",
row.names = FALSE, col.names = FALSE)

g <- as.matrix(read.table("adj.txt"))
#g <- as.matrix(read.table("C:/adj.txt"))

write.table(
matrix(c(
0,1,1,0,
1,0,1,0,
0,0,0,0,
1,0,0,0),
nrow = 4,
ncol = 4,
byrow = TRUE),
file = "adj.csv",
row.names = FALSE, col.names = FALSE,
sep = ",")

g <- as.matrix(read.csv("adj.csv", header = FALSE))

g <- matrix(
scan("adj.txt"),
nrow = 4, ncol = 4,
byrow = TRUE)

g <- matrix(
scan("adj.csv", sep = ","),
nrow = 4, ncol = 4,
byrow = TRUE)

#1.7
bg <- matrix(c(
0,0,0,0,1,1,0,
0,0,0,0,1,0,1,
0,0,0,0,0,1,1,
0,0,0,0,0,0,1,
1,1,0,0,0,0,0,
1,0,1,0,0,0,0,
0,1,1,1,0,0,0),
nrow = 7)
rownames(bg) <- c("a","b","c","d","A","B","C")
colnames(bg) <- c("a","b","c","d","A","B","C")
bg %*% bg

#1.8
install.packages("statnet")

library(sna)
edgelist <- matrix(c(
1,2,1,
1,3,1,
2,1,1,
2,3,1,
4,1,1),
ncol = 3, byrow = TRUE)

attr(edgelist, "n") <- 4
edgelist

attr(edgelist, "vnames") <- letters[1:4]
edgelist

rownames(dg) <- letters[1:4]
colnames(dg) <- letters[1:4]
dg

as.edgelist.sna(dg)
as.sociomatrix.sna(edgelist)

g1 <- matrix(c(
0,1,0,
1,0,1,
0,1,0),
nrow = 3)
g2 <- matrix(c(
0,0,1,
0,0,1,
1,1,0),
nrow = 3)
g <- array(dim = c(2,3,3))
g[1,,] <- g1
g[2,,] <- g2
g[1,,]
g[2,,]

wg <- matrix(c(
0,2,0,4,
2,0,3,1,
0,3,0,0,
4,1,0,0),
nrow = 4,
byrow = TRUE)

wg <- matrix(c(
1,2,2,
1,4,4,
2,1,2,
2,3,3,
2,4,1,
3,2,3,
4,1,4,
4,2,1),
ncol = 3,
byrow = TRUE)
attr(wg, "n") <- 4

bg2 <- matrix(c(
1,1,0,
1,0,1,
0,1,1,
0,0,1),
nrow = 4, ncol = 3, byrow = TRUE)
rownames(bg2) <- letters[1:4]
colnames(bg2) <- LETTERS[1:3]
bg2
as.sociomatrix.sna(bg2)

bg.edgelist <- matrix(c(
1,5,1,
1,6,1,
2,5,1,
2,7,1,
3,6,1,
3,7,1,
4,7,1,
5,1,1,
5,2,1,
6,1,1,
6,3,1,
7,2,1,
7,3,1,
7,4,1),
ncol = 3, byrow = TRUE)
attr(bg.edgelist, "n") <- 7
attr(bg.edgelist, "vnames") <- c("a","b","c","d","A","B","C")
attr(bg.edgelist, "bipartite") <- 4

#1.9
install.packages("igraph")

library(igraph)
dg <- matrix(c(
0,1,1,0,
1,0,1,0,
0,0,0,0,
1,0,0,0),
nrow = 4, ncol = 4, byrow = TRUE)
(g1 <- graph_from_adjacency_matrix(dg))
E(g1)

(g2 <- make_graph(e.list))

(g3 <- graph_from_edgelist(e.matrix))

(g4 <- graph_from_edgelist(matrix(c(
1,2,
1,3,
1,4,
2,3),
ncol = 2, byrow = TRUE),
directed = FALSE))

(g5 <- graph_from_edgelist(matrix(c(
"A", "B",
"C", "D",
"E", "F"),
ncol = 2,
byrow = TRUE)))
V(g5)

V(g1)$names <- letters[1:4]
V(g1)
V(g1)$names

(g6 <- graph_from_literal(A -+ B, C -+ D, E -+ F))

(g7 <- graph_from_edgelist(matrix(c(
"東京","神田",
"東京","神田",
"東京","神田"),
ncol = 2, byrow = TRUE),
directed = FALSE))

(g8 <- graph_from_adjacency_matrix(
matrix(c(
0,3,
3,0),
nrow = 2,ncol = 2),
mode = "undirected"))

(g8 <- graph_from_adjacency_matrix(
matrix(c(
0,3,
3,0),
nrow = 2, ncol = 2),
mode = "undirected", weighted = TRUE))
E(g8)$weight

g9 <- graph_from_edgelist(
matrix(c(
1, 2,
1, 4,
2, 3,
2, 4),
ncol = 2,byrow = TRUE),
directed = FALSE)
E(g9)$weight <- c(2,4,3,1)
g9
E(g9)$weight

g10 <- make_bipartite_graph(type = c(0,0,0,0,1,1,1),
edges = c(1,5,1,6,2,5,2,7,3,6,3,7,4,7))
V(g10)$names <- c("a","b","c","d","A","B","C")
g10

(g11 <- read_graph("fig7.net", format = "pajek"))
E(g11)$weight

(g12 <- read_graph(
"http://graphml.graphdrawing.org/primer/simple.graphml",
format = "graphml"))

?read_graph