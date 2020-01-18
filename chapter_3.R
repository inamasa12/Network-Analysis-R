#3.1
(2 * 6) / (5 * 4)
6 / (5 * 4)

Fig3.1 <- matrix(c(
0,1,1,1,1,
1,0,0,1,1,
1,0,0,0,0,
1,1,0,0,0,
1,1,0,0,0),
nrow = 5)
Fig3.2 <- matrix(c(
0,1,1,1,1,
0,0,0,1,1,
0,0,0,0,0,
0,0,0,0,0,
0,0,0,0,0),
nrow = 5,
byrow = TRUE)
sum(Fig3.1) / (nrow(Fig3.1) * (nrow(Fig3.1) - 1))
sum(Fig3.2) / (nrow(Fig3.2) * (nrow(Fig3.2) - 1))

#3.2
A <- matrix(c(
0,1,1,1,1,1,0,0,
1,0,1,0,0,0,1,0,
1,1,0,0,0,0,0,0,
1,0,0,0,1,0,0,1,
1,0,0,1,0,1,0,0,
1,0,0,0,1,0,0,0,
0,1,0,0,0,0,0,0,
0,0,0,1,0,0,0,0),
nrow = 8)
A2 <- A %*% A
diag(A2) <- 0
A2
sum(A2)

sum(A2 * A) / sum(A2)

#3.3
Fig3.5 <- matrix(c(
0,1,0,0,1,
1,0,1,0,0,
0,1,0,1,0,
1,0,1,0,0,
0,0,0,1,0),
nrow = 5,
byrow = TRUE)
a <- Fig3.5 * t(Fig3.5)
acd <- Fig3.5 + t(Fig3.5)
acd[which(acd >= 1)] <- 1
sum(a) / sum(acd)

(sum(a) + (5 * 4 - sum(acd))) / (5 * 4)

#3.4
1 - 1 / 6
1 - ((6 - (5 - 1)) / (5 - 1)^2)
1 - ((2 * 4) / ((5 - 1) * (5 - 2))) 

#3.5
library(sna)
gden(Fig3.1)
gden(Fig3.2)
gtrans(A)
grecip(Fig3.5, measure = "dyadic.nonnull")
mutuality(Fig3.5)
grecip(Fig3.5, measure = "dyadic")

Fig3.8 <- matrix(c(
0,0,1,1,0,
0,0,0,1,1,
0,0,0,1,0,
0,0,0,0,0,
0,1,0,0,0),
nrow = 5, ncol = 5,
byrow = TRUE)
connectedness(Fig3.8)
hierarchy(Fig3.8, measure = "krackhardt")
efficiency(Fig3.8)
lubness(Fig3.8)

#3.6
library(igraph)
g3.1 <- graph_from_adjacency_matrix(Fig3.1, mode = "undirected")
g3.2 <- graph_from_adjacency_matrix(Fig3.2)

edge_density(g3.1)
edge_density(g3.2)

g3.3 <- graph_from_adjacency_matrix(A, mode = "undirected")
transitivity(g3.3, type = "global")

g3.5 <- graph_from_adjacency_matrix(Fig3.5)
reciprocity(g3.5, mode =@"ratio")

detach(package:igraph)

#3.7
library(statnet)
data(package = "sna")
data(package = "ergm")

?coleman
data(coleman)
coord1 <- gplot(coleman, g = 1)

#Windows
windows(width = 14, height = 7) 

#MacOS
#quartz((width = 14, height = 7)

#Linux
#X11(width = 14, height = 7)

par(mfrow = c(1,2))
gplot(coleman, g = 1, coord = coord1, main = "Fall, 1957")
gplot(coleman, g = 2, coord = coord1, main = "Spring, 1958")

gden(coleman)
gtrans(coleman)
grecip(coleman, measure = "dyadic.nonnull")
mutuality(coleman)
connectedness(coleman)
hierarchy(coleman, measure = "krackhardt")
efficiency(coleman)
lubness(coleman)

sum(coleman[2,,]) - sum(coleman[1,,])

library(igraph)
?make_graph

(karate <- make_graph("Zachary"))
plot(karate)
edge_density(karate)
transitivity(karate, type = "global")