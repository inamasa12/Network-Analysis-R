#5.1.1
A <- matrix(c(
0,1,1,1,0,0,0,0,0,0, 
1,0,1,0,0,0,0,0,0,0, 
1,1,0,0,0,0,0,0,0,0, 
1,0,0,0,0,0,0,0,0,0, 
0,0,0,0,0,1,1,0,0,0, 
0,0,0,0,1,0,1,0,0,0, 
0,0,0,0,1,1,0,0,0,0, 
0,0,0,0,0,0,0,0,1,0, 
0,0,0,0,0,0,0,1,0,0, 
0,0,0,0,0,0,0,0,0,0),
nrow = 10, ncol = 10, byrow = TRUE)
library(sna)
components(A)
component.dist(A)

component.largest(A, result = "membership")
component.largest(A, result = "graph")

library(igraph)
g1 <- graph_from_adjacency_matrix(A, mode = "undirected")
components(g1)
component_distribution(g1)
component_distribution(g1, cumulative = TRUE)

#5.1.2
B <- matrix(c(
0,1,1,0,1,0,0, 
1,0,1,0,0,0,1, 
1,1,0,1,1,1,0, 
0,0,1,0,1,1,1, 
1,0,1,1,0,1,0, 
0,0,1,1,1,0,0, 
0,1,0,1,0,0,0),
nrow = 7, ncol = 7, byrow = TRUE)
library(sna)
clique.census(B, mode = "graph")

library(igraph)
g2 <- graph_from_adjacency_matrix(B, mode = "undirected")
max_cliques(g2)

#5.1.4
library(igraph)
Fig5.7 <- matrix(c(
1,2,
1,3,
1,4,
1,5,
1,9,
2,3,
2,4,
3,4,
5,6,
5,7,
5,9,
6,7,
6,8,
7,8), ncol = 2, byrow = TRUE)
g3 <- graph_from_edgelist(Fig5.7, directed = FALSE)
edge_betweenness(g3, directed = FALSE)

(eb <- cluster_edge_betweenness(g3, directed = FALSE))
eb$removed.edges
eb$edge.betweenness
eb$merges
plot(as.dendrogram(eb))

e <- matrix(c(
12,1,1,
1,10,1,
1,1,0),
nrow = 3, ncol = 3,
byrow = TRUE) / 28
a <- rowSums(e)
sum(diag(e) - a^2)

(Q <- eb$modularity)
step <- 0:8
plot(step, Q, type = "b")

cut_at(eb, steps = 6)

karate <- make_graph("Zachary")
(karate.eb <- cluster_edge_betweenness(karate))

max(karate.eb$modularity)
karate.eb$membership
plot_dendrogram(karate.eb)

(karate.op <- cluster_optimal(karate))
max(karate.op$modularity)

coord <- layout_with_fr(karate)
windows(width = 14, height = 7)
par(mfrow = c(1,2))
plot(karate.eb, karate, layout = coord,
 main = "cluster_edge_betweenness",
 xlab = paste("modularity =", round(modularity(karate.eb), 4)))
plot(karate.op, karate, layout = coord, main = "cluster_optimal",
 xlab = paste("modularity =", round(modularity(karate.op), 4)))

karate.fg <- cluster_fast_greedy(karate)
karate.le <- cluster_leading_eigen(karate)
karate.lp <- cluster_label_prop(karate)
karate.ml <- cluster_louvain(karate)
karate.wt <- cluster_walktrap(karate)
karate.sg <- cluster_spinglass(karate)

windows(width = 10, height = 7)
par(mfrow = c(2,3))
plot(karate.fg, karate, layout = coord,
 main = "cluster_fast_greedy",
 xlab = paste("modularity =", round(modularity(karate.fg), 4)))
plot(karate.le, karate, layout = coord,
 main = "cluster_leading_eigen",
 xlab = paste("modularity =", round(modularity(karate.le), 4)))
plot(karate.lp, karate, layout = coord,
 main = "cluster_label_prop",
 xlab = paste("modularity =", round(modularity(karate.lp), 4)))
plot(karate.ml, karate, layout = coord,
 main = "cluster_louvain",
 xlab = paste("modularity =", round(modularity(karate.ml), 4)))
plot(karate.wt, karate, layout = coord,
 main = "cluster_walktrap",
 xlab = paste("modularity =", round(modularity(karate.wt), 4)))
plot(karate.sg, karate, layout = coord,
 main = "cluster_spinglass",
 xlab = paste("modularity =", round(modularity(karate.sg), 4)))

#5.1.5
g4 <- Fig5.7
adj <- matrix(0, nrow = max(g4), ncol = max(g4))
for (i in 1:nrow(g4)) {
 adj[g4[i,1], g4[i,2]] <- 1
 adj[g4[i,2], g4[i,1]] <- 1
}
diag(adj) <- 1

link.sim <- matrix(0, nrow = nrow(g4), ncol = nrow(g4))
rownames(link.sim) <- paste(g4[,1],g4[,2], sep = "-")
colnames(link.sim) <- rownames(link.sim)

for (i in 1:(nrow(g4) - 1)) {
 for (j in (i + 1):nrow(g4)) {
  k <- intersect(g4[i,], g4[j,])
  if (length(k) != 0) {
   a <- g4[i,][which(g4[i,] != k)]
   b <- g4[j,][which(g4[j,] != k)]
   link.sim[i,j] <- 
    sum(adj[a,] * adj[b,]) / sum((adj[a,] + adj[b,]) > 0)
   link.sim[j,i] <- link.sim[i,j]
  }
 }
}
round(link.sim, 3)

link.dissimilarity <- as.dist(1 - link.sim)
hc <- hclust(link.dissimilarity, method = "average")

windows()
plot(hc)
cbind(hc$merge, "height" = hc$height)

pd <- rep(NA, nrow(hc$merge))
for (i in 1:nrow(hc$merge)) {
 cut <- cutree(hc, h = hc$height[i])
 ctab <- table(cut) 
 d <- 0
 for (j in 1:length(ctab)) {
  m <- ctab[j]
  n <- length(unique(as.vector(g4[which(cut == j),])))
  if (n > 2) {
   d <- d + m * ((m - n + 1) / ((n - 2) * (n - 1)))
  }
 }
pd[i] <- 2 * d / nrow(g4)
}

cbind(hc$merge, "height" = hc$height, "part.dens" = pd)
max(pd)
hc$height[which.max(pd)]

plot(hc)
abline(h = hc$height[which.max(pd)], col = "red", lty = "dashed")
(link.membership <- cutree(hc, h = hc$height[which.max(pd)]))
(nlc <- max(link.membership))
node.membership <- list()
for (i in 1:nlc) {
 node.membership[[i]] <- unique(as.vector(g4[link.membership == i,]))
}
node.membership

install.packages("linkcomm")
library(linkcomm)

lc <- getLinkCommunities(g4)
print(lc)

windows()
plotLinkCommDend(lc)

lc$edges
lc$nodeclusters

plotLinkCommGraph(lc)
plotLinkCommMembers(lc)

getCommunityCentrality(lc)

#5.2.1
A <- matrix(c(
0,0,1,0,0,0,
0,0,0,1,0,0,
1,1,0,0,0,0,
1,1,0,0,0,0,
1,1,0,0,0,0,
0,0,0,0,1,0),
nrow = 6, ncol = 6, byrow = TRUE)

D <- matrix(NA, nrow = 6, ncol = 6)
for (i in 1:(nrow(D) - 1)) {
 for (j in (i + 1):nrow(D)) {
  D[i,j] <- sqrt(sum((A[i,][-c(i,j)] - A[j,][-c(i,j)])^2)
               + sum((A[,i][-c(i,j)] - A[,j][-c(i,j)])^2))
  D[j,i] <- D[i,j]
 }
}
diag(D) <- 0
D

library(sna)
sedist(A, method = "euclidean")

#5.2.2
streq.cor <- function(dat) {
  n <- nrow(dat)
  cormatrix <- matrix(NA,n,n)
  for (i in 1:n) {
    for (j in i:n) {
      a <- dat[,i][-c(i,j)] - mean(dat[,i][-c(i,j)])
      b <- dat[i,][-c(i,j)] - mean(dat[i,][-c(i,j)])
      c <- dat[,j][-c(i,j)] - mean(dat[,j][-c(i,j)])
      d <- dat[j,][-c(i,j)] - mean(dat[j,][-c(i,j)])
      cormatrix[i,j] <- 
        (sum(a * c) + sum(b * d))/
        (sqrt(sum(a^2) + sum(b^2)) * sqrt(sum(c^2) + sum(d^2)))
      cormatrix[j,i] <- cormatrix[i,j]
      }
  }
  cormatrix
}
streq.cor(A)

sedist(A, method = "correlation")

#5.2.3
library(sna)
clust <- 
 equiv.clust(A, method = "euclidean", cluster.method = "complete")
plot(clust)

(bm <- blockmodel(A, clust, h = 1.5))

B <- cbind(rbind(Fig5.7, Fig5.7[,2:1]), rep(1, nrow(Fig5.7) * 2))
attr(B, "n") <- 9
clust2 <- 
 equiv.clust(B, method = "euclidean", cluster.method = "complete")
plot(clust2)
(bm2 <- blockmodel(B, clust2, k = 3))
plot(bm2)

#5.3
A <- matrix(c(
0,1,1,1,1,1,1,
1,0,1,0,0,1,1,
1,1,0,0,1,0,0,
1,0,0,0,0,0,0,
1,0,1,0,0,0,0,
1,1,0,0,0,0,0,
1,1,0,0,0,0,0),
nrow = 7, ncol = 7)

P <- (A + t(A)) / (rowSums(A) + colSums(A))

constraint <- rep(0, nrow(A))
for (i in 1:nrow(A)) {
 for (j in (1:nrow(A))[-i]) {
  if (A[i,j] > 0) {
   constraint[i] <- 
    constraint[i] + (P[i,j] + sum(P[i,][-c(i,j)] * P[,j][-c(i,j)]))^2
  }
 }
}
constraint

library(igraph)
constraint(graph_from_adjacency_matrix(A, mode = "undirected"))

#5.4
library(igraph)
co <- matrix(c(1,1, 0,0, 2,0), ncol = 2, byrow = TRUE)
windows()
par(mfrow = c(2,2), mar = c(0,0,0,0))
for (i in 1:4) {
 plot(graph_from_isomorphism_class(3, i-1, directed = FALSE),
 layout = co, vertex.color = "red", vertex.label = NA,
 edge.color = "black", edge.arrow.size = 0.5,
 frame = TRUE, margin = 0.1)
 text(0,0,i, cex = 1.5, col = "blue")
}

windows()
par(mfrow = c(4,4), mar = c(0,0,0,0))
for (i in 1:16) {
 plot(graph_from_isomorphism_class(3, i-1, directed = TRUE),
 layout = co, vertex.color = "red", vertex.label = NA,
 edge.color = "black", edge.arrow.size = 0.5,
 frame = TRUE, margin = 0.1)
 text(0,0,i, cex = 1.5, col = "blue")
}

g <- sample_gnp(10, p = 0.5, directed = TRUE)
(count_motifs <- motifs(g, size = 3))

windows()
par(mfrow = c(4,4), mar = c(0,0,0,0))
for (i in 1:16) {
 plot(graph_from_isomorphism_class(3, i-1, directed = TRUE),
 layout = co, vertex.color = "red", vertex.label = NA,
 edge.color = "black", edge.arrow.size = 0.5,
 frame = TRUE, margin = 0.1)
 text(0,0, count_motifs[i], cex = 1.5, col = "blue")
}

#5.5
library(igraph)
edges <- list(
c(),
c(2,1),
c(1,2,2,1),
c(1,2,1,3),
c(2,1,3,1),
c(1,3,2,1),
c(1,3,2,3,3,2),
c(2,3,3,1,3,2),
c(2,1,2,3,3,1),
c(1,2,2,3,3,1),
c(1,2,2,1,2,3,3,2),
c(1,2,1,3,2,3,3,2),
c(2,1,2,3,3,1,3,2),
c(1,3,2,1,2,3,3,2),
c(1,3,2,1,2,3,3,1,3,2),
c(1,2,1,3,2,1,2,3,3,1,3,2))

triads <- list()
triads[[1]] <- make_empty_graph()
for (i in 2:16) {
 triads[[i]] <- graph_from_edgelist(matrix(edges[[i]], ncol = 2, byrow = TRUE))
}

co <- matrix(c(1,1, 0,0, 2,0), ncol = 2, byrow = TRUE)
triad.types <- c("003","012","102","021D","021U","021C","111D",
"111U","030T","030C","201","120D","120U","120C","210","300")

windows()
par(mfrow = c(4,4), mar = c(0,0,0,0))
for (i in 1:16) {
 plot(triads[[i]], layout = co,
 vertex.color = "red", vertex.label = NA,
 edge.color = "black", edge.arrow.size = 0.5,
 frame = TRUE, margin = 0.1)
 text(0, -0.5, triad.types[i], cex = 1.5, col = "blue")
}

detach(package:linkcomm)
detach(package:igraph)
library(sna)
data(coleman)
triad.census(coleman)
triad.classify(coleman, g = 1, tri = c(20,21,22))
triad.classify(coleman, g = 2, tri = c(20,21,22))

library(igraph)
triad_census(graph_from_adjacency_matrix(coleman[1,,],
 mode = "directed"))
triad_census(graph_from_adjacency_matrix(coleman[2,,],
 mode = "directed"))